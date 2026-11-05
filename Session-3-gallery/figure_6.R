# Figure 6
library(dplyr)
library(ggplot2)

d <- read.csv("data/lab2_full_table.csv")

d <- d %>%
  mutate(status = ifelse(conflict == 1, "Internal conflict", "No internal conflict"))

# Median income in the two groups
d %>%
  group_by(status) %>%
  summarise(median_gdppc = median(gdppc, na.rm = TRUE))

ggplot(d, aes(x = status, y = gdppc)) +
  geom_boxplot(fill = "grey85") +
  labs(title = "Poverty breeds civil war",
       subtitle = "Countries at war have about a third of the income of countries at peace",
       x = NULL,
       y = "GDP per capita (PPP, constant 2021 international $)",
       caption = "Data: UCDP/PRIO Armed Conflict Dataset v26.1; World Bank WDI. 1990-2025.") +
  theme_minimal()

ggsave("figure_6.png", width = 7, height = 4.5, dpi = 150, bg = "white")
