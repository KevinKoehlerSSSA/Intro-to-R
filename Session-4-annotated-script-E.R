# =============================================================================
# FIGURE E (reader appendix) -- ANNOTATED COPY of step5_conflict_by_income.R (Lab 2, Step 5 figure,
# sent 7 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I ran it on lab2_full_table.csv; it runs as it is.
#
# THE SHORT VERSION
#   + Short, readable, and does what the header says. It counts rows, checks
#     for missing values and checks that the panel is balanced. You could
#     explain every line of this script, which is the course rule.
#   - The y axis is the average NUMBER of conflicts per country, not the share
#     of countries in conflict. The claim is about where conflict happens.
#     27% of conflict country-years have more than one conflict, so this
#     measure gives extra weight to countries like DR Congo (4 in 2020).
#   - "Poorer" means today's World Bank group, applied back to 1990. Syria and
#     Yemen count as low-income in the 1990s because their wars made them poor
#     after 2011.
#   - The colours do not follow the order of the groups, and the caption
#     names a dataset that does not exist.
# =============================================================================

 # Step 5: Is internal conflict since 1990 concentrated in poorer countries?
#
# This script reads the merged panel in `data/lab2_full_table.csv`
# (one row per country per year, 1990-2025) and draws a line chart.
#
# The chart tests the claim by showing, for each year, the AVERAGE number of
# conflicts per country within each of the four income groups. If conflict is
# concentrated in poorer countries, the "Low income" line should sit above the
# "High income" line throughout the period.

library(tidyverse)

# ---------------------------------------------------------------------------
# 1. Read the data
# ---------------------------------------------------------------------------

lab2 <- read_csv("data/lab2_full_table.csv", show_col_types = FALSE)

cat("Rows read in:", nrow(lab2), "\n")
cat("Countries:", n_distinct(lab2$country), "\n")
cat("Years:", min(lab2$year), "to", max(lab2$year), "\n\n")

# ---------------------------------------------------------------------------
# 2. Keep only what we need and check the data are complete
# ---------------------------------------------------------------------------

lab2 <- lab2 %>%
  select(country, year, n_conflicts, income_group)

# Report missing values rather than silently dropping them.
missing_check <- lab2 %>%
  summarise(missing_n_conflicts  = sum(is.na(n_conflicts)),
            missing_income_group = sum(is.na(income_group)))
print(missing_check)
# >> Good to print it. Both are 0, so nothing is dropped. Say so in a comment,
# >> so a reader does not have to run the script to know.

# Rows can be lost if any variable is missing, so state the row count here.
cat("Rows after selecting columns:", nrow(lab2), "\n\n")

# The panel should be balanced: every country present in every year.
rows_per_country <- lab2 %>% count(country)
cat("Countries with all 36 years (1990-2025):",
    sum(rows_per_country$n == 36), "of", nrow(rows_per_country), "\n\n")

# ---------------------------------------------------------------------------
# 3. Build the plotting data: average conflicts per country, by year and group
# ---------------------------------------------------------------------------

# mean() over n_conflicts gives the average number of conflicts per country
# for that income group in that year.
plot_data <- lab2 %>%
  group_by(year, income_group) %>%
# >> The measure. mean(n_conflicts) answers "how many conflicts does a country
# >> in this group have, on average?". mean(conflict) would answer "what share
# >> of countries in this group is in conflict?". The second is closer to the
# >> claim. Try both and compare the two pictures: the low-income line rises
# >> much more steeply with n_conflicts (0.43 -> 0.82) than with the share
# >> (38% -> 54%, 1990-2010 vs 2011-2025).
  summarise(mean_conflicts = mean(n_conflicts),
            n_countries   = n(),
            .groups = "drop")

cat("Rows in plot_data (36 years x 4 income groups):", nrow(plot_data), "\n\n")

# ---------------------------------------------------------------------------
# 4. Choose a colour for each income group
# ---------------------------------------------------------------------------

# The groups are ordered from poorest to richest, so we use a red-to-blue
# colour scheme: red = low income, blue = high income. This makes the
# "poorer countries have more conflict" pattern visible at a glance.
income_levels <- c("Low income", "Lower middle income",
                   "Upper middle income", "High income")

income_colours <- c("Low income"           = "#D55E00",  # orange-red
                    "Lower middle income" = "#E69F00",  # orange
                    "Upper middle income" = "#0072B2",  # blue
                    "High income"          = "#009E73")  # green
# >> The comment above says red-to-blue, poor-to-rich. But Upper middle is blue
# >> and High income is green, so the gradient breaks at the end. Ordered
# >> groups need ordered colours. Did you check the figure against the comment?

plot_data <- plot_data %>%
  mutate(income_group = factor(income_group, levels = income_levels))

# ---------------------------------------------------------------------------
# 5. Draw the line chart
# ---------------------------------------------------------------------------

p <- ggplot(plot_data, aes(x = year,
                           y = mean_conflicts,
                           colour = income_group,
                           group = income_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  scale_colour_manual(values = income_colours,
                      name = "Income group",
                      breaks = income_levels) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Internal conflict is concentrated in poorer countries",
# >> A title that states the finding: good. Precisely, the figure shows more
# >> conflicts per country, not more countries in conflict.
    subtitle = "Average number of conflicts per country, by income group, 1990-2025",
    x = "Year",
    y = "Average number of conflicts per country",
    caption = "Source: UCDP Priority Conflict data merged with World Bank income groups."
# >> There is no "UCDP Priority Conflict" dataset. The source is the UCDP/PRIO
# >> Armed Conflict Dataset, version 26.1 (PRIO is the Peace Research Institute
# >> Oslo). And "World Bank income groups" should say which: the current
# >> classification (2025/26), applied to all years.
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggsave("output/step5.png", p, width = 10, height = 6, dpi = 300)

cat("Saved output/step5.png\n")
# >> FOR SESSION 4: the low-income line is above the high-income line in every
# >> year. Write down, before class, how far apart you think the lines could be
# >> by chance alone if income had nothing to do with conflict.
