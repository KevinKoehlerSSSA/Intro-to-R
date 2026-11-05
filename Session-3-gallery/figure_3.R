# Figure 3
library(dplyr)
library(ggplot2)

d <- read.csv("data/lab2_full_table.csv")

# Number of country-years with internal conflict in each World Bank income group
by_group <- d %>%
  filter(conflict == 1) %>%
  count(income_group)
by_group

# Order the groups from poorest to richest
by_group$income_group <- factor(by_group$income_group,
                                levels = c("Low income", "Lower middle income",
                                           "Upper middle income", "High income"))

ggplot(by_group, aes(x = income_group, y = n)) +
  geom_col(fill = "grey40") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Middle-income countries: the main home of civil conflict",
       subtitle = "603 conflict country-years in middle-income countries, 403 in low-income countries",
       x = "World Bank income group",
       y = "Country-years with internal conflict",
       caption = "Data: UCDP/PRIO Armed Conflict Dataset v26.1; World Bank WDI. 1990-2025.") +
  theme_minimal()

ggsave("figure_3.png", width = 7, height = 4.5, dpi = 150, bg = "white")
