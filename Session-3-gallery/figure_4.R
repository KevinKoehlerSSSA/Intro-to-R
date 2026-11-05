# Figure 4
library(dplyr)
library(ggplot2)

d <- read.csv("data/lab2_full_table.csv")

# One row per country: typical income, and the share of years with conflict
by_country <- d %>%
  group_by(country) %>%
  summarise(median_gdppc = median(gdppc, na.rm = TRUE),
            share_conflict = mean(conflict))
nrow(by_country)

ggplot(by_country, aes(x = median_gdppc, y = share_conflict)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Each extra $10,000 of income cuts conflict risk by the same amount",
       subtitle = "Each dot is a country. A straight line summarises the relationship.",
       x = "Median GDP per capita 1990-2025 (PPP, constant 2021 international $)",
       y = "Share of years with internal conflict",
       caption = "Data: UCDP/PRIO Armed Conflict Dataset v26.1; World Bank WDI. 1990-2025.") +
  theme_minimal()

ggsave("figure_4.png", width = 7, height = 4.5, dpi = 150, bg = "white")
