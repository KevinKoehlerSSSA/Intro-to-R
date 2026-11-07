# =============================================================================
# FIGURE D (reader appendix) -- ANNOTATED COPY of step5.R (Lab 2, Step 5 figure, sent 7 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I ran it on lab2_full_table.csv (only the path changed) and got
# your numbers exactly: 31.7% vs 4.6%.
#
# THE SHORT VERSION
#   + The clearest test of the five. One question, one comparison: the share
#     of country-years in conflict, in four equal-sized income groups, with
#     the peaceful years in the denominator. Missing GDP is counted, dropped
#     openly and reported in the caption, together with the test design.
#   + Short, simple code that you can explain line by line.
#   - The quartiles are computed over ALL years at once. Incomes rose between
#     1990 and 2025, so the "poorest 25%" is mostly early years. Quartiles
#     WITHIN each year compare countries with their contemporaries. Here it
#     barely changes the answer (27 points vs 29), but you should know which
#     one you did.
#   - Income is from the SAME year as the conflict. War lowers income, so
#     part of the gap could be the war's effect. Use the previous year.
#   - The subtitle types the numbers by hand.
# =============================================================================

# Test whether internal conflict is concentrated in poorer country-years.
library(tidyverse)

# Read the country-year table and report how many observations were supplied.
country_years <- read_csv(
  "C:/Users/<you>/Desktop/R/lab2_full_table.csv",
# >> A full path to your desktop: the script will not run on anyone else's
# >> computer. Put the file in your project's data/ folder and read
# >> "data/lab2_full_table.csv".
  show_col_types = FALSE
)
cat("Rows read:", nrow(country_years), "\n")

# Count missing values before limiting the test to rows with both measures.
missing_gdppc <- sum(is.na(country_years$gdppc))
missing_conflict <- sum(is.na(country_years$conflict))
cat("Rows missing GDP per capita:", missing_gdppc, "\n")
cat("Rows missing conflict status:", missing_conflict, "\n")

# Keep country-years with observed GDP and conflict status so both variables
# contribute to the same comparison, and report exactly how many are removed.
analysis_data <- country_years %>%
  filter(!is.na(gdppc), !is.na(conflict))
# >> Good: drop, count, report. Which rows are they? Run
# >>   country_years %>% filter(is.na(gdppc)) %>% count(country, sort = TRUE)
# >> 16 countries. Some are at peace (Cuba, North Korea), but most of the
# >> dropped CONFLICT years are poor countries at war: Yemen (19), South Sudan
# >> (12), Afghanistan (11). Dropping them makes the poorest bar a little
# >> LOWER, so here it works against the claim, not for it. That is worth one
# >> sentence in the caption.
cat("Rows removed for missing GDP or conflict status:",
    nrow(country_years) - nrow(analysis_data), "\n")
cat("Rows in the analysis:", nrow(analysis_data), "\n")

# Rank country-years into four equally sized GDP-per-capita groups. Equal-sized
# groups make conflict rates directly comparable from the poorest to richest.
analysis_data <- analysis_data %>%
  mutate(gdp_quartile = ntile(gdppc, 4))
# >> ntile() over the whole table: the cut-offs are $4,283, $11,885 and $30,177
# >> for ALL years. A country with $4,000 in 1995 and $5,000 in 2020 moves up a
# >> quartile even if every other country grew faster. To compare countries
# >> with their contemporaries, add group_by(year) before mutate() and
# >> ungroup() after. And for income in the PREVIOUS year: arrange by country
# >> and year, then group_by(iso3c) and mutate(gdppc_lag = lag(gdppc)).
cat("Rows after assigning GDP quartiles:", nrow(analysis_data), "\n")

# Calculate each quartile's GDP range and percentage of country-years in conflict.
plot_data <- analysis_data %>%
  group_by(gdp_quartile) %>%
  summarise(
    n_country_years = n(),
    n_conflict_years = sum(conflict == 1),
    conflict_rate = mean(conflict == 1),
    min_gdppc = min(gdppc),
    max_gdppc = max(gdppc),
    .groups = "drop"
  ) %>%
  mutate(
    wealth_label = factor(
      c("Poorest 25%", "Lower-middle 25%", "Upper-middle 25%", "Richest 25%"),
      levels = c("Poorest 25%", "Lower-middle 25%", "Upper-middle 25%", "Richest 25%")
    ),
    range_label = paste0(
      scales::dollar(min_gdppc, accuracy = 1), "–",
      scales::dollar(max_gdppc, accuracy = 1)
    ),
    bar_label = paste0(scales::percent(conflict_rate, accuracy = 0.1),
                       "\n", scales::comma(n_conflict_years), " conflict years")
  )
cat("Rows in the plotted summary:", nrow(plot_data), "\n")
print(plot_data %>% select(wealth_label, n_country_years, n_conflict_years, conflict_rate))

# Plot the observed conflict rate in each equally sized GDP group.
step5_plot <- ggplot(plot_data, aes(x = wealth_label, y = conflict_rate, fill = gdp_quartile)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = bar_label), vjust = -0.45, lineheight = 0.95,
            size = 3.7, fontface = "bold") +
  scale_fill_gradient(low = "#9B2226", high = "#005F73", guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 0.38),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "Internal conflict becomes less common as GDP per capita rises",
    subtitle = "Conflict occurred in 31.7% of the poorest quartile, compared with 4.6% of the richest",
# >> Typed by hand. If you change the quartiles (see above), the subtitle will
# >> be wrong and nothing will warn you. Build it from plot_data, as you
# >> already do for the caption, e.g.
# >>   scales::percent(plot_data$conflict_rate[1], accuracy = 0.1)
    x = "GDP per capita group (PPP-adjusted 2021 international dollars)",
    y = "Country-years with internal conflict (%)",
    caption = stringr::str_wrap(
      paste0(
        "Test design: unit = country-year; years = 1990–2025; outcome = any internal conflict; ",
        "comparison = conflict rates across four equal-sized GDP-per-capita groups. ",
        "GDP ranges, poor to rich: ", paste(plot_data$range_label, collapse = "; "), ". ",
        "N = ", scales::comma(nrow(analysis_data)), "; ", missing_gdppc,
        " rows missing GDP per capita excluded. Sources: UCDP/PRIO Armed Conflict Dataset and ",
        "World Bank World Development Indicators."
      ),
      width = 135
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 17, colour = "#17252A"),
    plot.subtitle = element_text(size = 11.5, margin = margin(b = 12)),
    plot.caption = element_text(hjust = 0, size = 8.4, colour = "grey35",
                                margin = margin(t = 12)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x = element_text(face = "bold", colour = "grey20"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the finished figure at a presentation-ready size and resolution.
ggsave("output/step5.png", step5_plot, width = 9, height = 6.5, dpi = 200, bg = "white")
# >> FOR SESSION 4: your figure is the test in its simplest form: 31.7% vs 4.6%,
# >> a gap of 27 points. Write down, before class, how big a gap you think
# >> could come out BY CHANCE if income had nothing to do with conflict.
