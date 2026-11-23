# =====================================================================
# Data Analysis in R -- Session 6, slides 4.2 to 4.6
# Who pushes the slope up, who pulls it down? All 58 countries
#
#   Goldsmith and Horiuchi (2012), "In Search of Soft Power",
#   World Politics 64 (3). Table 3, Model 5.
#   Data: data/data_2011-07-26.dta (the replication data; from GitHub)
#
# Run it line by line (Ctrl/Cmd + Enter), not all at once.
# =====================================================================

# install.packages("haven")   # once, if library(haven) gives an error
library(haven)
library(dplyr)
library(ggplot2)


# ---- Set-up, as in session6_regression.R ----

gh <- read_dta("data/data_2011-07-26.dta")
nrow(gh)
s <- gh %>% filter(keep == 1)   # the 58 countries in the paper
nrow(s)

s$op1 <- (s$pos1 - s$neg1) / 100
s$op2 <- (s$pos2 - s$neg2) / 100
s$op3 <- (s$pos3 - s$neg3) / 100
s$opinion <- rowMeans(cbind(s$op1, s$op2, s$op3), na.rm = TRUE)


# ---- Distances from the mid-point and their products (slides 4.1 to 4.3) ----

x_bar <- mean(s$opinion)    # -0.14
y_bar <- mean(s$unvoting)   # 51.7
x_bar
y_bar

s$dx <- s$opinion - x_bar
s$dy <- s$unvoting - y_bar
s$product <- s$dx * s$dy
s$dx_squared <- s$dx^2

# Does the country push the slope up or pull it down?
s$direction <- ifelse(s$product > 0, "pushes up", "pulls down")
table(s$direction)


# ---- The full list: all 58 countries (slide 4.4, not only four) ----

push_pull <- s %>%
  select(country, opinion, unvoting, dx, dy, product, direction) %>%
  arrange(desc(product))
nrow(push_pull)
print(as.data.frame(push_pull), digits = 3)

# The countries that push up, strongest first
pushes_up <- push_pull %>% filter(product > 0)
nrow(pushes_up)
print(as.data.frame(pushes_up), digits = 3)

# The countries that pull down, strongest first
pulls_down <- push_pull %>% filter(product < 0) %>% arrange(product)
nrow(pulls_down)
print(as.data.frame(pulls_down), digits = 3)


# ---- Add up: 95.3 up, 86.9 down, 8.4 left over (slide 4.5) ----

sum(pushes_up$product)
sum(pulls_down$product)
sum(s$product)

# The picture on slide 4.5: the 58 products, sorted
ggplot(push_pull, aes(x = reorder(country, product), y = product,
                      fill = direction)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Product of the two distances", fill = NULL) +
  theme(axis.text.y = element_text(size = 6))


# ---- Divide: the slope, 8.39 / 4.05 = 2.07 (slide 4.6) ----

sum(s$dx_squared)
slope <- sum(s$product) / sum(s$dx_squared)
slope
intercept <- y_bar - slope * x_bar
intercept

# Each country's share of the slope: its product divided by 4.05.
# The 58 shares add up to the slope.
s$slope_share <- s$product / sum(s$dx_squared)
sum(s$slope_share)
s %>%
  select(country, product, slope_share) %>%
  arrange(desc(slope_share)) %>%
  as.data.frame() %>%
  print(digits = 3)


# ---- Residuals: how far each country is from the line ----

m5 <- lm(unvoting ~ opinion, data = s)
coef(m5)

s$predicted <- predict(m5)          # voting the line predicts
s$residual <- s$unvoting - s$predicted   # actual minus predicted
sum(s$residual)                     # zero, up to rounding

# Above the line (votes with the US more than opinion predicts) first
s %>%
  select(country, opinion, unvoting, predicted, residual, direction) %>%
  arrange(desc(residual)) %>%
  as.data.frame() %>%
  print(digits = 3)

# Residuals by group: the pushers and the pullers
s %>%
  group_by(direction) %>%
  summarise(countries = n(),
            sum_product = sum(product),
            mean_residual = mean(residual),
            mean_abs_residual = mean(abs(residual)))
