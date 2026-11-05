# Figure 5
library(dplyr)
library(ggplot2)

d <- read.csv("data/lab2_full_table.csv")

# Share of countries with an internal conflict, each year
by_year <- d %>%
  group_by(year) %>%
  summarise(share_conflict = mean(conflict))
by_year

ggplot(by_year, aes(x = year, y = share_conflict)) +
  geom_line(linewidth = 1) +
  geom_point() +
  coord_cartesian(ylim = c(0.13, 0.22)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Civil conflict is back",
       subtitle = "The share of countries with internal conflict is up by half since the early 2000s",
       x = NULL,
       y = "Countries with internal conflict",
       caption = "Data: UCDP/PRIO Armed Conflict Dataset v26.1; World Bank WDI. 1990-2025.") +
  theme_minimal()

ggsave("figure_5.png", width = 7, height = 4.5, dpi = 150, bg = "white")
