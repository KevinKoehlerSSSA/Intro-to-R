# Figure 1
library(dplyr)
library(ggplot2)

d <- read.csv("data/lab2_full_table.csv")

# Keep the country-years with an internal conflict
conflict_years <- d %>% filter(conflict == 1)
nrow(conflict_years)

ggplot(conflict_years, aes(x = gdppc)) +
  geom_histogram(binwidth = 2500, boundary = 0, fill = "grey40", colour = "white") +
  labs(title = "Internal conflict is a poor-country phenomenon",
       subtitle = "Most country-years with internal conflict have a GDP per capita below $10,000",
       x = "GDP per capita (PPP, constant 2021 international $)",
       y = "Country-years with internal conflict",
       caption = "Data: UCDP/PRIO Armed Conflict Dataset v26.1; World Bank WDI. 1990-2025.") +
  theme_minimal()

ggsave("figure_1.png", width = 7, height = 4.5, dpi = 150, bg = "white")
