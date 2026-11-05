# Figure 2
library(dplyr)
library(ggplot2)

d <- read.csv("data/lab2_full_table.csv")

# Keep the country-years with an internal conflict
conflict_years <- d %>% filter(conflict == 1)

# UCDP intensity: 1 = 25 to 999 battle deaths in the year, 2 = 1,000 or more
conflict_years <- conflict_years %>%
  mutate(intensity = ifelse(max_intensity == 2,
                            "War (1,000+ deaths)",
                            "Minor conflict (25-999 deaths)"))

ggplot(conflict_years, aes(x = intensity, y = gdppc)) +
  geom_boxplot(fill = "grey85") +
  labs(title = "Wars are not much poorer than minor conflicts",
       subtitle = "Median GDP per capita: about $5,200 in minor conflicts, $4,200 in wars",
       x = NULL,
       y = "GDP per capita (PPP, constant 2021 international $)",
       caption = "Data: UCDP/PRIO Armed Conflict Dataset v26.1; World Bank WDI. 1990-2025.") +
  theme_minimal()

ggsave("figure_2.png", width = 7, height = 4.5, dpi = 150, bg = "white")
