# =====================================================================
# Data Analysis in R -- Session 6, Monday 23 November 2026
# Linear regression: does public opinion of the US shape UN voting?
#
#   Goldsmith and Horiuchi (2012), "In Search of Soft Power",
#   World Politics 64 (3). Table 3, Models 5 and 6.
#   Data: data/data_2011-07-26.dta (the replication data; from GitHub)
#
# Run it line by line (Ctrl/Cmd + Enter), not all at once.
# =====================================================================


# ---- Set-up (slide 3.1) ----

library(haven)
library(dplyr)
library(ggplot2)
gh <- read_dta("data/data_2011-07-26.dta")
nrow(gh)
s <- gh %>% filter(keep == 1)   # the 58 countries in the paper
nrow(s)

# Opinion: % positive minus % negative about US policy, divided by 100,
# from up to three Gallup surveys (2002-03)
s$op1 <- (s$pos1 - s$neg1) / 100
s$op2 <- (s$pos2 - s$neg2) / 100
s$op3 <- (s$pos3 - s$neg3) / 100

# Missing surveys per country (0, 1 or 2)
table(is.na(s$op1) + is.na(s$op2) + is.na(s$op3))



# ---- Opinion, the scatterplot and the line (slide 3.2) ----

# Average of the surveys each country has
s$opinion <- rowMeans(cbind(s$op1, s$op2, s$op3), na.rm = TRUE)

ggplot(s, aes(x = opinion, y = unvoting)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Opinion of US foreign policy",
       y = "UN voting with the US (%)")


# ---- 4a: the mid-point (slide 4.1) ----

x_bar <- mean(s$opinion)
y_bar <- mean(s$unvoting)
x_bar
y_bar


# ---- 4a: each country's distance from the mid-point (slide 4.2) ----

s$dx <- s$opinion - x_bar
s$dy <- s$unvoting - y_bar
four <- s %>%
  filter(country %in% c("France", "Albania", "Vietnam", "Philippines"))
four %>% select(country, opinion, unvoting, dx, dy)


# ---- 4b: multiply the two distances (slide 4.3) ----

s$product <- s$dx * s$dy
four <- s %>%
  filter(country %in% c("France", "Albania", "Vietnam", "Philippines"))
four %>% select(country, dx, dy, product)


# ---- 4c: add up the products of all 58 countries (slide 4.5) ----

sum(s$product)
sum(s$product[s$product > 0])   # countries that push the slope up
sum(s$product[s$product < 0])   # countries that pull it down


# ---- 4d: divide by the squared opinion distances (slide 4.6) ----

s$dx_squared <- s$dx^2
slope <- sum(s$product) / sum(s$dx_squared)
slope
intercept <- y_bar - slope * x_bar
intercept

# The same with lm(): Model 5
m5 <- lm(unvoting ~ opinion, data = s)
coef(m5)


# ---- 5: an invented world of 190 countries (slide 5.1) ----

set.seed(2084)   # same random numbers for everyone

# Opinion for 190 countries, spread like the 58 we have
world <- data.frame(opinion = rnorm(190, mean = mean(s$opinion),
                                    sd = sd(s$opinion)))
# Voting: 52 + 2 x opinion + things we do not observe
world$unvoting <- 52 + 2 * world$opinion + rnorm(190, mean = 0, sd = 22)

# The true slope: the line through all 190 countries
true_slope <- coef(lm(unvoting ~ opinion, data = world))["opinion"]
true_slope


# ---- 5: one sample, then 1,000 samples (slides 5.2 to 5.5) ----

# One sample of 58 of the 190 countries, and its slope
one_sample <- world[sample(190, 58), ]
coef(lm(unvoting ~ opinion, data = one_sample))["opinion"]

# 1,000 samples of 58 countries; keep the slope of each
slopes <- replicate(1000,
  coef(lm(unvoting ~ opinion, data = world[sample(190, 58), ]))["opinion"])

# Law of large numbers: the average slope approaches the true slope
running_average <- cumsum(slopes) / (1:1000)
plot(running_average, type = "l",
     xlab = "Number of samples", ylab = "Average slope so far")
abline(h = true_slope, lty = 2)
mean(slopes)

# Central limit theorem: the slopes pile up in a bell curve
hist(slopes, breaks = 40,
     main = "1,000 slopes from samples of 58", xlab = "Slope")
abline(v = true_slope, lty = 2)


# ---- 6: standard error, interval, p-value (slides 6.1 to 6.5) ----

# The spread of the 1,000 slopes: the standard error
sd(slopes)

# One sample is all we ever have: R estimates the spread from it
summary(lm(unvoting ~ opinion, data = one_sample))

# Back to the real 58 countries
summary(m5)
confint(m5)


# ---- 7: Lab 1, Models 5 and 6 (slide 7.2) ----

m5 <- lm(unvoting ~ opinion, data = s)
summary(m5)

# Two controls need to be built first, as in the paper
s$democ <- s$pr + s$cl                    # Freedom House: rights + liberties
s$afgh <- as.numeric(s$troops_afgh > 0)   # 1 = troops in Afghanistan

m6 <- lm(unvoting ~ opinion + afgh + icc + s_lead + nato +
           aid_m + aid_e + lntrade + lngdppc + democ +
           muslimpct + europe, data = s)
summary(m6)
nobs(m6)


# ---- 10: Lab 2, one control at a time (slide 10.1) ----

# Each model: opinion plus ONE control. Note the opinion slope each time.
m_afgh <- lm(unvoting ~ opinion + afgh, data = s)
coef(m_afgh)["opinion"]

m_icc <- lm(unvoting ~ opinion + icc, data = s)
coef(m_icc)["opinion"]

m_s_lead <- lm(unvoting ~ opinion + s_lead, data = s)
coef(m_s_lead)["opinion"]

m_nato <- lm(unvoting ~ opinion + nato, data = s)
coef(m_nato)["opinion"]

m_aid_m <- lm(unvoting ~ opinion + aid_m, data = s)
coef(m_aid_m)["opinion"]

m_aid_e <- lm(unvoting ~ opinion + aid_e, data = s)
coef(m_aid_e)["opinion"]

m_lntrade <- lm(unvoting ~ opinion + lntrade, data = s)
coef(m_lntrade)["opinion"]

m_lngdppc <- lm(unvoting ~ opinion + lngdppc, data = s)
coef(m_lngdppc)["opinion"]

m_democ <- lm(unvoting ~ opinion + democ, data = s)
coef(m_democ)["opinion"]

m_muslimpct <- lm(unvoting ~ opinion + muslimpct, data = s)
coef(m_muslimpct)["opinion"]

m_europe <- lm(unvoting ~ opinion + europe, data = s)
coef(m_europe)["opinion"]


# ---- 11: wealth as a common cause (slide 11.1) ----

# Is wealth related to opinion? And to voting?
cor(s$lngdppc, s$opinion)
cor(s$lngdppc, s$unvoting)
