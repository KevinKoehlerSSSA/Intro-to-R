# =============================================================================
# FIGURE A (reader appendix) -- ANNOTATED COPY of 03_chart_conflict_by_income.R (Lab 2, Step 5 figure,
# sent 7 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I ran the script on the course table lab2_full_table.csv
# (renaming iso3c -> iso3 and gdppc -> gdp_pc_ppp, and changing the three
# paths). Your own table has 203 countries and current-price GDP, so some of
# your numbers differ from mine; the points below do not depend on that.
#
# THE SHORT VERSION
#   + Panel A is the right comparison: the SHARE of country-years in conflict,
#     within each group, with the peaceful years in the denominator.
#   + The notes at the top say openly that the income groups are today's
#     World Bank classification. Saying what your figure assumes is exactly
#     the habit we want.
#   - The headline "the gap widened over time" depends on that classification.
#     Syria, Yemen, South Sudan, the Central African Republic, Mali and
#     Burkina Faso count as "Low income" in EVERY year because they are poor
#     NOW, and they are poor now partly because of their wars. Measured with
#     income in the previous year, the gap between the poorest and the
#     richest quarter of countries is 28 points in 1991-2005 and 32 points
#     in 2011-2025. Next session we test whether a change of 4 points is more
#     than chance. (It is not.)
#   - Panel B draws COUNTS of country-years, not shares, and the percentage
#     labels sit in the wrong block (a small bug, see below).
#   - Panel C applies the World Bank's GNI thresholds to GDP PPP, and today's
#     thresholds to 1990 incomes. It also answers a different question.
#     I would drop it.
#   - Two facts in the comments are wrong: what UCDP "type 4" is, and the GDP
#     series named in the caption.
# =============================================================================

# =============================================================================
# Lab 2 - Chart: has internal conflict concentrated in poorer countries?
# =============================================================================
#
# THE QUESTION
#   After the Cold War, did internal conflict concentrate in poorer countries?
#
# THE THREE PANELS
#   Panel A (line chart): the conflict rate in each World Bank income group
#   year by year, 1990-2025. This carries the "after the Cold War" part: it
#   shows the poor-country line sitting high and the rich-country line near zero.
#
#   Panel B (stacked bars): the same comparison in 5-year spans, but now each
#   bar is split into PEACEFUL country-years (the large block) and CONFLICT
#   country-years (the slice). Before, the peaceful years were only implied
#   as "the rest of the bar"; now you can see them. Across the whole dataset
#   86.3% of country-years are peaceful.
#
#   Panel C (mover chart): the World Bank income group is the CURRENT
#   classification, held constant across all years - so a country that was
#   poor in 1990 but is now richer appears as "Low income" for the whole
#   period. This panel makes that visible: it counts how many countries
#   changed income group over the period, and whether conflict followed.
#
# WHAT "PROBABILITY" MEANS IN PANEL B
#   For a given 5-year span and income group:
#       probability = country-years in conflict / country-years in the group
#   Read it as: "a country in this income group had roughly this percentage
#   chance of being in conflict in any given year during this span."
#   Every country contributes the same number of years to a span, so this is
#   also the average of the individual countries' own probabilities.
#
# NOTES ON THE DATA (read before interpreting)
#   - The `conflict` flag marks UCDP/PRIO conflicts of type 3 or 4, as
#     specified. Type 4 is "extra-state conflict with internationalized
# >> Not quite. In the UCDP/PRIO codebook, type 2 is EXTRASTATE conflict (a
# >> state against a non-state group outside its territory, e.g. colonial wars).
# >> Type 4 is INTERNATIONALIZED INTERNAL conflict: a civil war in which other
# >> states send troops to one side. Did you check this in the codebook, or
# >> did the assistant write it? The high-income country-years in the course
# >> table (Israel 31 years, Russia 30, the US 18, the UK 3) are conflicts
# >> that UCDP LOCATES in those countries (gwno_loc), not wars fought abroad.
#     factions", so a few high-income countries appear (the USA, UK, China,
#     France, Australia) in years when they were fighting wars abroad. That is
#     why the high-income segments are not exactly zero.
#   - Income groups are the current World Bank classification. Panel C shows
#     what that hides: 145 of 203 countries changed group over the period.
#   - The last span, 2020-2025, covers 6 years; the others cover 5. The
#     probability is still comparable because it is a rate, not a count.
#
# Run from the project folder (notes-lab2.md):
#     Rscript scripts/03_chart_conflict_by_income.R
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---------------------------------------------------------------------------
# PROJECT PATH (hardcoded on purpose, so the script runs from anywhere)
# ---------------------------------------------------------------------------
# >> Hardcoded paths ("on purpose, so the script runs from anywhere") have
# >> the opposite effect: the script runs only on your laptop. Open the
# >> project in RStudio and use relative paths such as "output/conflict_income.csv".
# >> The course AGENTS.md asks the assistant to do this.
PANEL_CSV   <- "C:/Users/<you>/<project folder>/output/conflict_income.csv"
OUT_DIR     <- "C:/Users/<you>/<project folder>/output"
PROJECT_DIR <- "C:/Users/<you>/<project folder>"

setwd(PROJECT_DIR)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(PANEL_CSV)) {
  cat("ERROR: cannot find", PANEL_CSV, "\n")
  stop("Missing input file.")
}

# Colours: a clear poor -> rich gradient. The palette is colour-blind safe.
group_cols <- c(
  "Low income"           = "#B2182B",  # deep red   - poorest
  "Lower middle income"  = "#EF8A62",  # salmon
  "Upper middle income"  = "#67A9CF",  # light blue
  "High income"          = "#2166AC"   # deep blue  - richest
)

# The order we want the income groups to appear in, everywhere.
income_levels <- c("Low income", "Lower middle income",
                   "Upper middle income", "High income")

# The 5-year spans. The last one covers 2020-2025 (6 years).
span_labels <- c("1990-94", "1995-99", "2000-04", "2005-09",
                 "2010-14", "2015-19", "2020-25")

# ---------------------------------------------------------------------------
# 1. Read the merged panel
# ---------------------------------------------------------------------------
out <- read.csv(PANEL_CSV, stringsAsFactors = FALSE)

cat("Rows read:", nrow(out), "\n")
cat("Countries:", length(unique(out$iso3)), "\n")
cat("Years:", min(out$year), "-", max(out$year), "\n")
cat("Peaceful country-years:", sum(out$conflict == 0),
    sprintf("(%.1f%%)\n", 100 * mean(out$conflict == 0)), "\n\n")

# ---------------------------------------------------------------------------
# 2. Split the years into 5-year spans
# ---------------------------------------------------------------------------
out$span <- cut(
  out$year,
  breaks = c(1989, 1994, 1999, 2004, 2009, 2014, 2019, 2025),
  labels = span_labels,
  right = TRUE
)

cat("Spans created:", nlevels(out$span), "\n")
cat("Country-years per span:\n")
print(table(out$span))

# ===========================================================================
# PANEL A: conflict rate by income group over time (the line chart)
# ===========================================================================
# >> Good: the share in conflict WITHIN each group and year. The denominator
# >> includes the peaceful country-years. This is the comparison the claim needs.
rate <- out %>%
  group_by(year, income_group) %>%
  summarise(
    n_conf = sum(conflict),
    n      = n(),
    conflict_rate = 100 * n_conf / n,
    .groups = "drop"
  ) %>%
  mutate(income_group = factor(income_group, levels = income_levels))

cat("\nPANEL A rows:", nrow(rate), "(4 groups x 36 years =", 4 * 36, "expected)\n")

overall <- out %>%
  group_by(income_group) %>%
  summarise(
    n_conf = sum(conflict), n = n(),
    rate = round(100 * sum(conflict) / n(), 1),
    .groups = "drop"
  )

pA <- ggplot(rate, aes(x = year, y = conflict_rate, colour = income_group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.9) +
  scale_colour_manual(values = group_cols, name = "Income group") +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.05))) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  labs(
    title = "A. Conflict rate by income group, 1990-2025",
    subtitle = "Share of countries in each World Bank income group in conflict",
    x = NULL, y = "Countries in conflict (% of group)",
    colour = "Income group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11.5),
    plot.subtitle = element_text(size = 9, colour = "grey30"),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92")
  )

# ===========================================================================
# PANEL B: peaceful vs conflict, stacked, by 5-year span and income group
# ===========================================================================
# We build the data in LONG form: one row per (span, income group, status).
# status is either "At peace" or "In conflict". Stacking the two segments
# gives a bar that is 100% of that group's country-years, so the large
# peaceful block and the small conflict slice are both visible.
prob_long <- out %>%
  group_by(span, income_group) %>%
  summarise(
    n_countries = n_distinct(iso3),
    n_cy        = n(),
    n_conf      = sum(conflict),
    n_peace     = n() - sum(conflict),
    .groups = "drop"
  ) %>%
  mutate(income_group = factor(income_group, levels = income_levels)) %>%
  # pivot_longer turns the two counts into rows ready for stacking
  pivot_longer(cols = c(n_peace, n_conf),
               names_to = "status", values_to = "n") %>%
  mutate(
    status = recode(status, n_peace = "At peace", n_conf = "In conflict"),
    status = factor(status, levels = c("At peace", "In conflict"))
  )

cat("PANEL B rows:", nrow(prob_long), "(7 spans x 4 groups x 2 statuses =",
    7 * 4 * 2, "expected)\n\n")

# Colours for the two states. Neutral blue-grey for peace, strong red for
# conflict, so the eye goes straight to the conflict slices.
state_cols <- c("At peace" = "#BFC9D4", "In conflict" = "#B2182B")

# We work out the total for each (span, income group) first, so the label can
# just read the percentage of the group that was in conflict.
prob_long <- prob_long %>%
  group_by(span, income_group) %>%
  mutate(n_total = sum(n)) %>%
  ungroup() %>%
  mutate(pct_conflict = 100 * n / n_total)

# We draw one facet per income group. Within each facet the bars are 100%
# stacked: the whole bar is that group's country-years, split into the
# peaceful block and the conflict slice. Faceting avoids the clutter of
# dodging four groups side by side on one axis.
# >> The y axis here is the NUMBER of country-years, not the share. The bars
# >> have different heights because the groups have different numbers of
# >> countries (25 low-income, 55 high-income), so the eye compares group
# >> sizes, not conflict rates. The subtitle says "share". For shares, use
# >> geom_col(position = "fill") and a percent axis.
pB <- ggplot(prob_long, aes(x = span, y = n, fill = status)) +
  geom_col(width = 0.72, position = position_stack(vjust = 1)) +
  # Label the conflict slice with its percentage of the group. We place the
  # text at the MIDDLE of the red block by computing where that block sits:
  # the red block runs from (peaceful count) up to (total count), so its
  # middle is halfway between the two. We pass that y directly to geom_text.
  geom_text(
    data = prob_long %>% filter(status == "In conflict") %>%
      mutate(y_label = n_total - n / 2),
# >> Bug: ggplot stacks the FIRST factor level on top. "At peace" is the first
# >> level, so the red "In conflict" block sits at the bottom, from 0 to n.
# >> Its middle is n / 2, not n_total - n / 2. Run it and look: the white
# >> labels sit in the grey block. Did you look at the figure after this
# >> change? The assistant cannot see it unless you show it.
    aes(y = y_label, label = sprintf("%.0f%%", pct_conflict)),
    colour = "white", fontface = "bold", size = 2.9
  ) +
  scale_fill_manual(values = state_cols, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ income_group, nrow = 1) +
  labs(
    title = "B. Peaceful and conflict country-years, in 5-year spans",
    subtitle = paste0(
      "Each bar is one period for that income group: the pale block is the ",
      "share of country-years at peace, the red slice the share in conflict."
    ),
    x = "Period", y = "Country-years"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11.5),
    plot.subtitle = element_text(size = 9, colour = "grey30"),
    legend.position = "bottom",
    legend.text = element_text(size = 8.5),
    axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_rect(fill = "grey93", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92")
  )

# ===========================================================================
# PANEL C: countries that changed income group
# ===========================================================================
# The income group in the panel above is the CURRENT classification, applied
# to all years. To show what that hides, we recompute each country's income
# group year by year from its own GDP, using the World Bank's thresholds:
#   Low income      <= 1,145
#   Lower middle    1,146 - 4,495
#   Upper middle    4,496 - 14,005
#   High income     > 14,005
out$group_by_year <- cut(
  out$gdp_pc_ppp,
  breaks = c(-Inf, 1145, 4495, 14005, Inf),
# >> These are the World Bank's thresholds for GNI per capita, Atlas method,
# >> in CURRENT US dollars (fiscal year 2026). Your variable is GDP per capita
# >> at PPP. PPP values for poor countries are two to three times their Atlas
# >> values, so almost nobody is "low income" under this rule. And a 2026
# >> threshold is applied to 1990 incomes. The "moved group" count is mostly
# >> an artefact of the mismatch.
  labels = c("Low income", "Lower middle income",
             "Upper middle income", "High income")
)

movers <- out %>%
  filter(!is.na(group_by_year)) %>%
  group_by(iso3, country) %>%
  summarise(
    n_years      = n(),
    group_start  = as.character(group_by_year[which.min(year)]),
    group_end    = as.character(group_by_year[which.max(year)]),
    n_conf_years = sum(conflict),
    .groups = "drop"
  ) %>%
  mutate(moved = group_start != group_end)

cat("PANEL C: countries compared:", nrow(movers), "\n")
cat("PANEL C: countries that changed income group:", sum(movers$moved), "\n")

# Which transitions happened, and how many of those countries had conflict.
mover_tbl <- movers %>%
  filter(moved) %>%
  count(group_start, group_end, name = "n_countries")

cat("\n--- the transitions ---\n")
print(as.data.frame(mover_tbl), row.names = FALSE)

# For the chart: the share of countries in each group that saw conflict,
# split by whether they changed group or not.
mover_plot <- movers %>%
  mutate(status = ifelse(moved, "Changed income group", "Stayed in same group")) %>%
  group_by(status) %>%
  summarise(
    n_countries = n(),
    pct_with_conflict = 100 * sum(n_conf_years > 0) / n(),
    .groups = "drop"
  )

cat("\n--- conflict experience by mover status ---\n")
print(as.data.frame(mover_plot), row.names = FALSE)

pC <- ggplot(mover_plot, aes(x = status, y = pct_with_conflict, fill = status)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%.0f%%", pct_with_conflict)),
            vjust = -0.5, size = 3, colour = "grey20") +
  scale_fill_manual(values = c("Changed income group" = "#B2182B",
                               "Stayed in same group" = "#8FAFD1"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
# >> Panel C also answers a different question: did countries that moved
# >> between groups have more conflict? That is not part of the claim, and it
# >> invites a causal reading ("moving causes conflict"?). Your instinct, to
# >> show what the fixed classification hides, is good. A cleaner way: draw
# >> Panel A again with income in the PREVIOUS year, split into four groups
# >> within each year, and compare the two pictures.
    title = "C. Most countries changed income group over the period",
    subtitle = paste0(
      "Share of countries that saw any conflict, by whether they changed group. ",
      sum(movers$moved), " of ", nrow(movers), " moved."
    ),
    x = NULL, y = "Countries with at least one conflict year (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11.5),
    plot.subtitle = element_text(size = 9, colour = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92")
  )

# ===========================================================================
# 3. Combine the panels
# ===========================================================================
# In ggplot2 4.x the "+" operator no longer merges two plots, so we use
# patchwork::wrap_plots() instead.
combined <- wrap_plots(pA, pB, pC, ncol = 1, heights = c(1, 1, 0.85))

# Add the overall caption to the whole figure.
combined <- combined +
  plot_annotation(
    title = "Internal conflict after the Cold War has concentrated in poorer countries",
# >> Two claims in one title and subtitle. "Concentrated in poorer countries":
# >> yes, the figure supports it. "The gap widened over time": the figure shows
# >> a pattern, which is a hypothesis, not yet a finding. See the note at the top.
    subtitle = paste0(
      "Low-income countries: ", overall$rate[overall$income_group == "Low income"],
      "% of country-years in conflict  vs  ",
      overall$rate[overall$income_group == "High income"], "% for high-income countries.\n",
      "Overall, ", sprintf("%.1f", 100 * mean(out$conflict == 0)),
      "% of all country-years are peaceful; the gap between poor and rich countries widened over time."
    ),
    caption = paste0(
      "Source: UCDP/PRIO Armed Conflict Dataset v26.1 (conflict types 3 and 4) and World Bank WDI, ",
      "GDP per capita PPP (NY.GDP.PCAP.PP.CD).\n",
# >> PP.CD is PPP in CURRENT international dollars: not adjusted for inflation.
# >> Fine for comparing countries within one year. Not fine for Panel C, which
# >> compares 1990 with 2025. The constant-price series is NY.GDP.PCAP.PP.KD.
      "Note: type 4 is extra-state conflict with internationalized factions, so some high-income countries ",
      "appear in years of wars fought abroad.\n",
      "Panels A and B use the current World Bank income classification, held constant across all years; ",
      "Panel C shows how many countries changed group under their own year-by-year income.\n",
      "Panel B groups years into 5-year periods; 2020-25 covers 6 years."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 9.5, colour = "grey25"),
      plot.caption = element_text(size = 8, colour = "grey35", hjust = 0),
      plot.caption.position = "plot"
    )
  )

# ---------------------------------------------------------------------------
# 4. Save the chart
# ---------------------------------------------------------------------------
png_file <- file.path(OUT_DIR, "chart_conflict_by_income.png")
pdf_file <- file.path(OUT_DIR, "chart_conflict_by_income.pdf")

ggsave(png_file, plot = combined, width = 11, height = 12, dpi = 300)
cat("\nWrote", png_file, "\n")

ggsave(pdf_file, plot = combined, width = 11, height = 12)
cat("Wrote", pdf_file, "\n")

# Save the underlying numbers, so the chart can be checked or reused.
write.csv(rate, file.path(OUT_DIR, "table_conflict_rate_by_income.csv"), row.names = FALSE)
write.csv(prob_long, file.path(OUT_DIR, "table_peace_conflict_by_span.csv"), row.names = FALSE)
write.csv(movers, file.path(OUT_DIR, "table_income_group_movers.csv"), row.names = FALSE)
cat("Wrote table_conflict_rate_by_income.csv\n")
cat("Wrote table_peace_conflict_by_span.csv\n")
cat("Wrote table_income_group_movers.csv\n\n")

print(combined)

cat("\nDone.\n")
# >> FOR SESSION 4: your figure shows that low-income countries were in
# >> conflict in 45% of their country-years and high-income countries in 4%.
# >> Two questions for Friday. (1) Could a gap this size arise by chance if
# >> income had nothing to do with conflict? (2) Could the widening you see in
# >> Panel A arise by chance? Write down your guess for each before class.
