# =============================================================================
# FIGURE B (reader appendix) -- ANNOTATED COPY of 04_visualise_gdp_conflict.R (Lab 2, Step 5 figure,
# sent 7 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I ran it on lab2_full_table.csv; it runs as it is.
#
# THE SHORT VERSION
#   + The most ambitious script. A binned scatter on a log axis is a good
#     answer to "how do I show a 0/1 outcome against income?". It shows
#     uncertainty, and Step 4b asks exactly the right question: is it GDP,
#     or something that goes with it?
#   - The confidence intervals and p-values treat the 6,228 country-years as
#     6,228 independent observations. They are not. A country in conflict
#     this year is in conflict next year 85% of the time (a country at peace:
#     3%). Afghanistan, Colombia, India, Myanmar and the Philippines are in
#     conflict in every year 1991-2025. The honest intervals are about FOUR
#     times wider. This is the main topic of Session 4.
#   - Imputing missing GDP with the median of the income group invents data:
#     287 country-years get an income they never had, and they go into the
#     bins and into the logistic curve.
#   - Figure 3 gets the UCDP coding wrong: intensity 2 IS war (1,000+ battle
#     deaths). There is no level 3. The subtitle "no country-year reached
#     war level" is false: there are 285 war country-years.
#   - Figure 2's headline "the gap is widening" rests on today's World Bank
#     groups (see the note there).
#   - 528 lines, a helper function, list-columns, a loop over models, sink().
#     The course rule is code you can explain line by line. Could you explain
#     sapply(ci, `[`, "lower") to the class?
# =============================================================================

# =============================================================================
# Visualise: GDP per capita (PPP) vs armed conflict
#
# Purpose
#   Decide which visualisation method best shows the relationship between
#   GDP per capita and armed conflict, and produce the figures that support it.
#
# Input (read-only, never modified):
#   /home/<you>/<project folder>/output/lab2_full_table.csv   (one row per country-year, 1990-2025)
#
# Outputs:
#   output/fig1_gdp_conflict_binned.png   <- MAIN figure
#   output/fig2_conflict_rate_by_decade.png
#   output/fig3_gdp_conflict_intensity.png
#   output/fig1_gdp_conflict_binned.csv    (the numbers behind figure 1)
#   output/fig2_conflict_rate_by_decade.csv
#   output/fig3_gdp_conflict_intensity.csv
#   output/visualisation_diagnostics.txt   (everything printed to the console)
#
# >> "Agreed with the student" is the assistant writing about you. Read this
# >> block as its summary of the conversation, and check each item: did you
# >> ask for imputation? For Wilson intervals? For three figures?
# Design decisions (agreed with the student):
#   - The main figure is a BINNED scatter (conflict rate per GDP bin with 95%
#     Wilson confidence intervals) rather than a raw scatter of all 6,228
#     country-years. A raw scatter is a hairball: only 17.6% of the rows are
#     conflict years, so the two groups overlap almost completely.
#   - The x-axis is LOG GDP per capita. On the raw scale GDP spans 511 to
#     149,025 (a factor of 292) and is right-skewed (skew 0.39); on the log
#     scale it is symmetric (skew -0.05), so the bins are comparable.
#   - Missing GDP (287 rows, 4.6%) is IMPUTED with the income-group median,
#     as the student asked. The imputed rows are kept visually separate
#     (hollow points) so the figure never hides which points are measured
#     and which are estimated.
#   - No extra packages: Wilson intervals are computed by hand and panels are
#     combined with gridExtra-free base grid, because `binom`, `patchwork` and
#     `maps` are not installed in this environment.
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# STEP 0 - A small helper: Wilson score interval for a binomial proportion
# -----------------------------------------------------------------------------
# For a bin with x conflicts out of n country-years, the naive "rate +- 1.96
# sqrt(p(1-p)/n)" breaks down for small n or rates near 0 and 1 (it can go
# below 0). The Wilson interval always stays inside [0, 1] and stays close to
# the true value when n is small, so it is the right choice here.
#
# z = 1.96 gives a 95% confidence interval.
# >> The Wilson interval is a good formula, correctly coded. The problem is not
# >> the formula but its assumption: every country-year is an independent draw.
# >> That assumption fails here (see the note at the top). A correct formula
# >> fed with dependent data gives a wrong interval.
wilson_ci <- function(x, n, z = 1.96) {
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(lower = max(0, centre - half), upper = min(1, centre + half))
}

# A shared colour palette for the income groups, used in every figure so the
# same colour always means the same group.
income_levels <- c("Low income", "Lower middle income",
                   "Upper middle income", "High income")
income_cols <- c("Low income"         = "#d7301f",  # red   = most conflict
                 "Lower middle income" = "#fc8d59",  # orange
                 "Upper middle income" = "#91cf60",  # light green
                 "High income"        = "#2166ac")  # blue  = least conflict

# -----------------------------------------------------------------------------
# STEP 1 - Read the table
# -----------------------------------------------------------------------------
lab2 <- read_csv("output/lab2_full_table.csv", show_col_types = FALSE)

cat("STEP 1 - Rows read:", nrow(lab2), "\n")
cat("         Countries:", n_distinct(lab2$country),
    "| Years:", min(lab2$year), "-", max(lab2$year), "\n")
cat("         Country-years with conflict = 1:", sum(lab2$conflict == 1),
    "(", round(100 * mean(lab2$conflict), 2), "% of rows)\n")
cat("         Country-years with conflict = 0:", sum(lab2$conflict == 0), "\n\n")

# -----------------------------------------------------------------------------
# STEP 2 - Impute the missing GDP with the income-group median
# -----------------------------------------------------------------------------
# >> Partly true. 5.2% of conflict country-years have no GDP, against 4.5% of
# >> peaceful ones: a small difference. The missing conflict years are mostly
# >> Yemen (19), South Sudan (12) and Afghanistan (11). Dropping them is not
# >> "quietly removing" them if you count and report them, as the script for Figure D does.
# 287 rows have no GDP value. They are concentrated in low-income and
# conflict-affected states (Cuba, Eritrea, North Korea, South Sudan, Yemen,
# Venezuela), so simply dropping them would quietly remove exactly the
# country-years that are most interesting for this question.
#
# We fill them with the MEDIAN GDP of their own income group. The median
# (rather than the mean) is used because GDP is right-skewed, so the median is
# the more typical value of the group.
#
# We also create a flag `gdp_imputed` so that:
#   - the console output reports exactly how many rows were affected
#   - the figure can draw imputed points differently from measured ones
median_gdp_by_group <- lab2 %>%
  filter(!is.na(gdppc)) %>%
  group_by(income_group) %>%
  summarise(med_gdp = median(gdppc), .groups = "drop")

lab2 <- lab2 %>%
  left_join(median_gdp_by_group, by = "income_group") %>%
  mutate(gdp_imputed = is.na(gdppc),
         gdppc = ifelse(is.na(gdppc), med_gdp, gdppc))
# >> This is the step I would undo. The median of a group defined by TODAY's
# >> income is not an estimate of Yemen's income in 1995. The 287 imputed rows
# >> sit on just four values (one per group), so they pile up in a few bins
# >> and pull those bins' conflict rates toward their own. Then
# >> the logistic curve is fitted to invented x values. Safer: drop the rows,
# >> report how many and which countries, and show (as a check) that the
# >> result does not change much without them. Hollow points were planned
# >> but never drawn: the figure does not mark the imputed rows.

cat("STEP 2 - GDP imputation (income-group median)\n")
cat("         Rows WITH a measured GDP value :", sum(!lab2$gdp_imputed), "\n")
cat("         Rows IMPUTED (were NA)         :", sum(lab2$gdp_imputed),
    "(", round(100 * mean(lab2$gdp_imputed), 2), "% of the table)\n")
cat("         Of the imputed rows, how many are conflict years:",
    sum(lab2$gdp_imputed & lab2$conflict == 1), "\n")
cat("         Median GDP used for each income group:\n")
print(as.data.frame(median_gdp_by_group))
cat("\n         Countries whose GDP was imputed (rows):\n")
print(as.data.frame(lab2 %>% filter(gdp_imputed) %>% count(country, income_group) %>%
                      arrange(desc(n)) %>% head(20)))
cat("\n")

# -----------------------------------------------------------------------------
# STEP 3 - Build the binned dataset for the MAIN figure
# -----------------------------------------------------------------------------
# We cut GDP into 30 equal-count bins (quantile bins). Equal counts matter
# because GDP is very unevenly distributed: a few rich countries sit at the
# top, so fixed-width bins would leave the right-hand side of the chart
# nearly empty and the left-hand bins very uncertain.
#
# Each bin then gets:
#   rate   = share of country-years in the bin that had a conflict
#   lo, hi = 95% Wilson confidence interval around that share
#   n      = number of country-years in the bin
#   n_conf = number of conflict country-years in the bin
#   med_gdp = median GDP in the bin, used as the x position (so the point sits
#             at the typical GDP of the bin, not at an arbitrary edge)
N_BINS <- 30
# >> 30 bins = 30 estimates, each with its own interval. With 30 intervals,
# >> one or two will miss by chance even if everything is right. Fewer,
# >> larger bins (10) would be easier to read and to defend.

binned <- lab2 %>%
  filter(!is.na(gdppc)) %>%
  mutate(bin = ntile(gdppc, N_BINS)) %>%
  group_by(bin) %>%
  summarise(
    n        = n(),
    n_conf   = sum(conflict == 1),
    rate     = n_conf / n,
    med_gdp  = median(gdppc),
    lo_gdp   = min(gdppc),
    hi_gdp   = max(gdppc),
    # The most common income group inside the bin. A bin usually sits inside
    # one income group, because the groups are defined by income, so this
    # label tells the reader which group the bin belongs to.
    top_group = names(sort(table(income_group), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(ci = list(wilson_ci(n_conf, n))) %>%
  ungroup() %>%
  mutate(lo = sapply(ci, `[`, "lower"),
         hi = sapply(ci, `[`, "upper")) %>%
  select(-ci) %>%
  # Put the group in a sensible factor order so the legend reads low -> high
  mutate(top_group = factor(top_group, levels = income_levels))

cat("STEP 3 - Binned dataset built:", nrow(binned), "bins\n")
cat("         Rows per bin: min", min(binned$n), "max", max(binned$n), "\n")
cat("         Conflict rate by bin (first 5 and last 5 bins):\n")
print(as.data.frame(bind_rows(head(binned, 5), tail(binned, 5)) %>%
                      select(bin, n, n_conf, rate, lo, hi, med_gdp, top_group)))
cat("\n         Is the trend monotonic? rate decreasing across all bins:",
    all(diff(binned$rate) <= 0), "\n")
cat("         Spearman correlation between bin order and conflict rate:",
    round(cor(binned$bin, binned$rate, method = "spearman"), 3), "\n")
# >> Right instinct: a bump that falls within the noise is not a finding. But
# >> this conclusion is printed whatever the data say: it is hard-coded text,
# >> not a result. And the intervals it relies on are too narrow (see top).
cat("         (Not strictly monotonic: a few bins tick up, but every one of\n")
cat("          those upticks falls inside overlapping confidence intervals,\n")
cat("          so they are sampling noise rather than real structure.)\n\n")

# -----------------------------------------------------------------------------
# STEP 4 - Fit the logistic trend line (logistic regression on log GDP)
# -----------------------------------------------------------------------------
# We use a logistic regression because the outcome is a 0/1 indicator, so a
# straight line on the probability scale could predict values below 0 or
# above 1. The logistic curve stays inside [0, 1] for any GDP value.
#
# We fit it on log GDP because that is the scale on which the relationship is
# closest to linear, and it lets us read the curve off the log x-axis.
trend_fit <- glm(conflict ~ log(gdppc), data = lab2, family = binomial)
# >> Logistic regression is Session 8. The model is reasonable; its p-values
# >> (e.g. 2.77e-106) assume independent observations again. With standard
# >> errors that allow for the same country appearing 36 times, the evidence
# >> is still strong, but nowhere near 1 in 10^106.

cat("STEP 4 - Logistic trend: conflict ~ log(GDP per capita)\n")
print(summary(trend_fit)$coefficients)
cat("         Odds ratio for a doubling of GDP:",
    round(exp(coef(trend_fit)[2] * log(2)), 3), "\n")
cat("         => each doubling of GDP multiplies the odds of conflict by",
    round(exp(coef(trend_fit)[2] * log(2)), 2), "\n\n")

# Predicted probability on a grid of GDP values, for drawing the smooth curve.
grid_gdp <- exp(seq(log(min(lab2$gdppc)), log(max(lab2$gdppc)), length.out = 400))
grid_pred <- data.frame(
  gdppc = grid_gdp,
  pred  = predict(trend_fit, newdata = data.frame(gdppc = grid_gdp),
                  type = "response")
)

# -----------------------------------------------------------------------------
# STEP 4b - Robustness: is the pooled GDP effect real, or is it the income
#           group doing the work?
# -----------------------------------------------------------------------------
# This matters for how the main figure should be read, so we check it
# explicitly instead of assuming.
#
# The concern: the income groups are DEFINED by income, so a pooled
# cross-section could be showing "poor countries have more conflict" simply
# because low income and conflict go together, without GDP per se being the
# driver. We test this by:
#   1. adding YEAR to the model (GDP rises over time, and conflict rates also
#      shift over time, so year could be confounding the relationship);
#   2. adding INCOME GROUP to the model (does GDP still matter once we know
#      which group a country is in?);
#   3. correlating log GDP with conflict WITHIN each income group (if the
#      effect is real and graded, it should still be visible inside a group).
trend_year   <- glm(conflict ~ log(gdppc) + year, data = lab2, family = binomial)
trend_full   <- glm(conflict ~ log(gdppc) + year + income_group,
                    data = lab2, family = binomial)

cat("STEP 4b - Robustness of the GDP-conflict relationship\n")
cat("   Model                              coef(log GDP)   odds ratio per doubling   p-value\n")
for (pair in list(list("GDP only", trend_fit),
                  list("GDP + year", trend_year),
                  list("GDP + year + income group", trend_full))) {
  nm <- pair[[1]]; mm <- pair[[2]]
  cat(sprintf("   %-33s %+8.4f      %8.3f                %.2e\n",
              nm, coef(mm)[2], exp(coef(mm)[2] * log(2)),
              summary(mm)$coefficients[2, 4]))
}
cat("\n   Correlation between log GDP and conflict WITHIN each income group:\n")
within_group <- lab2 %>%
  group_by(income_group) %>%
  summarise(n = n(),
            rate = round(100 * mean(conflict), 1),
            cor_loggdp_conflict = round(cor(log(gdppc), conflict,
                                           use = "complete.obs"), 3),
            .groups = "drop")
print(as.data.frame(within_group))
# >> Careful with this conclusion. Inside one income group, incomes vary much
# >> less than across the whole sample, and any correlation computed on a
# >> narrow range is weaker. So "close to zero within groups" does not show
# >> that the effect is a step between groups. It is also hard-coded text,
# >> written before anyone saw the numbers.
cat("\n   Interpretation: the pooled effect is strong and survives controlling\n")
cat("   for year. But inside each income group the correlation is close to\n")
cat("   zero, and adding income group to the model roughly halves the GDP\n")
cat("   coefficient. So the headline difference is mainly BETWEEN groups\n")
cat("   (poor vs rich countries), not a smooth gradient WITHIN a group.\n")
cat("   This is why figure 1 colours each bin by income group: it shows the\n")
cat("   reader that the drop happens as we move from one group to the next.\n\n")

# -----------------------------------------------------------------------------
# STEP 5 - MAIN FIGURE: binned scatter with confidence intervals + trend line
# -----------------------------------------------------------------------------
# Why this chart and not a raw scatter:
#   - 6,228 points in one panel, 82% of them non-conflict, would be an
#     unreadable hairball where the two groups blend into one cloud.
#   - Binning turns the question ("is the conflict rate lower when GDP is
#     higher?") into a directly readable curve with an honest uncertainty band.
#   - The point size grows with the number of observations in the bin, so the
#     reader can see which parts of the curve are estimated precisely and
#     which rest on few country-years.
fig1 <- ggplot(binned, aes(x = med_gdp, y = rate)) +
  # Confidence interval around each bin's conflict rate, drawn as a thick bar
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.08,
                colour = "grey35", linewidth = 0.9, alpha = 0.9) +
  # The binned conflict rate, coloured by income group and sized by the
  # number of CONFLICT country-years in the bin.
  #
  # We size by the number of conflicts, not the number of observations. Every
  # bin holds almost exactly the same number of country-years (207-208),
  # because they are equal-count bins, so sizing by n would carry no
  # information at all. Sizing by the number of conflicts tells the reader
  # something true: the biggest markers are the bins where the most conflict
  # actually happened.
  #
  # Colouring by income group is what makes this figure honest. The pooled
  # curve looks like a smooth dose-response curve, but the colour shows that
  # most of the drop happens BETWEEN the income groups (red -> blue), not
  # gradually within them.
  geom_point(aes(size = n_conf, fill = top_group), shape = 21,
# >> Look at the size legend in the saved figure: white circles with a white
# >> outline on a white background, so it shows only text. Add
# >> guides(size = guide_legend(override.aes = list(fill = "grey50"))).
             colour = "white", stroke = 0.6) +
  # The logistic trend fitted to ALL 6,228 rows (not just the bins)
  geom_line(data = grid_pred, aes(x = gdppc, y = pred),
            colour = "#2166ac", linewidth = 1.1) +
  scale_fill_manual(values = income_cols, drop = FALSE,
                    labels = c("Low income", "Lower middle income",
                               "Upper middle income", "High income")) +
  # Mark the income-group boundaries so the reader can connect the curve to
  # the group labels used in the other figures
  scale_x_log10(
    breaks = c(1000, 3000, 10000, 30000, 100000),
    labels = scales::label_dollar(scale = 1, suffix = "", accuracy = 1)
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, 1)) +
  scale_size_continuous(range = c(2.5, 8),
                        breaks = c(25, 50, 75, 100),
                        labels = scales::label_number(scale = 1,
                                                     suffix = " conflicts")) +
  labs(
    title = "Poor countries are far more likely to be in armed conflict",
    subtitle = paste0("Conflict rate by GDP per capita, ", N_BINS,
                      " equal-count bins, 173 countries 1990-2025 (6,228 country-years)\n",
                      "Bars are 95% Wilson confidence intervals; point size = number of conflicts in the bin; colour = income group"),
    x = "GDP per capita (PPP, US$, log scale)",
    y = "Share of country-years with an internal armed conflict",
    fill = "Income group",
    size = "Conflicts in bin",
    caption = paste0("Source: UCDP/PRIO Armed Conflict Dataset v26.1 and World Bank GDP per capita PPP.\n",
                     "Conflict = internal (type 3) or internationalized internal (type 4).",
                     " Blue line = logistic fit on all country-years.",
                     " Missing GDP imputed with income-group median (287 rows).")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 9.5, colour = "grey30", lineheight = 1.25),
    plot.caption    = element_text(size = 8, colour = "grey40", hjust = 0),
    legend.position = "bottom",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8.5),
    legend.box      = "vertical",
    panel.grid.minor = element_blank()
  )

ggsave("output/fig1_gdp_conflict_binned.png", fig1,
       width = 10, height = 6.5, dpi = 150)
write_csv(binned, "output/fig1_gdp_conflict_binned.csv")

cat("STEP 5 - Written: output/fig1_gdp_conflict_binned.png\n")
cat("         Written: output/fig1_gdp_conflict_binned.csv\n\n")

# -----------------------------------------------------------------------------
# STEP 6 - FIGURE 2: conflict rate by income group over time
# -----------------------------------------------------------------------------
# The main figure shows LEVEL differences between rich and poor countries.
# It cannot show DIRECTION over time, because a pooled cross-section mixes
# 36 years together. This figure does: it splits the data by decade and by
# income group, so each line is a decade's snapshot of one group.
#
# This is where the most important dynamic finding appears: the conflict rate
# of LOW-INCOME countries RISES over time (40% in the 1990s to 60% in the
# 2020s) while the rate for high-income countries FALLS.
decade_data <- lab2 %>%
  mutate(decade = 10 * (year %/% 10)) %>%
  group_by(decade, income_group) %>%
  summarise(n = n(),
            n_conf = sum(conflict == 1),
            rate = n_conf / n,
            .groups = "drop")

cat("STEP 6 - Conflict rate by income group and decade:\n")
print(as.data.frame(decade_data %>%
                      filter(decade %in% c(1990, 2020)) %>%
                      select(decade, income_group, n, rate) %>%
                      mutate(rate = round(100 * rate, 1))))
cat("\n")

fig2 <- ggplot(decade_data, aes(x = decade, y = rate, colour = income_group,
                                 group = income_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  # Label each line at its right-hand end instead of using a legend, so the
  # reader does not have to match colours back and forth. The label sits just
  # INSIDE the last point (hjust = 1.1) because outside the panel the text
  # would be clipped by the edge of the device.
  geom_text(data = decade_data %>% filter(decade == max(decade)),
            aes(label = income_group),
            hjust = 1.15, vjust = -0.4, size = 3.1, show.legend = FALSE) +
  scale_colour_manual(values = income_cols,
                       breaks = income_levels,
                       labels = c("Low income", "Lower middle income",
                                  "Upper middle income", "High income")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, 0.65),
                     expand = expansion(mult = c(0, 0.08))) +
  # Leave room on the right for the line labels, so they are not clipped.
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     limits = c(1988, 2026),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "The income gap in conflict is widening, not closing",
# >> This rests on income_group, the World Bank's CURRENT classification.
# >> Syria, Yemen, South Sudan, the Central African Republic, Mali and Burkina
# >> Faso are "low income" today partly because of wars that started after
# >> 2010; the classification puts them in the poor line for all years. With
# >> income in the previous year (quartiles within each year), the gap between
# >> the poorest and the richest quarter goes from 28 points (1991-2005) to
# >> 32 (2011-2025). We will test on Friday whether 4 points is more than
# >> chance.
    subtitle = paste0("Share of country-years in internal armed conflict, by income group and decade\n",
                      "The low-income line rises while the high-income line falls"),
    x = "Decade",
    y = "Share of country-years with conflict",
    caption = paste0("Note: the 2020s decade covers 2020-2025 only, so it is not yet complete.\n",
                     "Source: UCDP/PRIO Armed Conflict Dataset v26.1 and World Bank GDP per capita PPP.")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 9.5, colour = "grey30", lineheight = 1.25),
    plot.caption  = element_text(size = 8, colour = "grey40", hjust = 0),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave("output/fig2_conflict_rate_by_decade.png", fig2,
       width = 10, height = 6, dpi = 150)
write_csv(decade_data, "output/fig2_conflict_rate_by_decade.csv")

cat("STEP 6 - Written: output/fig2_conflict_rate_by_decade.png\n\n")

# -----------------------------------------------------------------------------
# STEP 7 - FIGURE 3: is conflict only rarer in rich countries, or also milder?
# -----------------------------------------------------------------------------
# The main figure only shows WHETHER a conflict happened. A fair question is
# whether, once a conflict happens, its severity also depends on income.
#
# max_intensity follows UCDP/PRIO:
#   1 = minor conflict      (fewer than 1,000 battle deaths)
#   2 = intermediate conflict
# >> Wrong. In the UCDP/PRIO codebook intensity_level is 1 = minor (25-999
# >> battle-related deaths in the year) and 2 = war (1,000 or more). There is
# >> no 3. So "Intermediate conflict" below is war, and the subtitle "no
# >> country-year reached UCDP's war level" is false: 285 country-years are
# >> wars. Where did "intermediate" come from? (The old PRIO coding had three
# >> levels.) Check codebook facts in the codebook, not with the assistant.
#   (the source also defines 3 = war, but no country-year in this window
#    reaches it after the internal-conflict filter)
#
# We show the share of conflict years at each severity, by GDP quintile.
# A stacked bar per quintile answers the question directly.
# The label must be built AFTER summarising, otherwise min()/max() are
# evaluated inside the group and every quintile would show the whole range.
quintile_data <- lab2 %>%
  mutate(q = ntile(gdppc, 5)) %>%
  group_by(q) %>%
  summarise(n = n(),
            n_conf = sum(conflict == 1),
            minor = sum(conflict == 1 & max_intensity == 1),
            intermediate = sum(conflict == 1 & max_intensity == 2),
            lo_gdp = min(gdppc),
            hi_gdp = max(gdppc),
            .groups = "drop") %>%
  mutate(q_label = paste0("Q", q, "\n(", scales::comma(round(lo_gdp)),
                          " - ", scales::comma(round(hi_gdp)), ")"))

cat("STEP 7 - Conflict severity by GDP quintile (conflict years only):\n")
print(as.data.frame(quintile_data %>%
                      mutate(across(c(minor, intermediate),
                                    ~ round(100 * .x / n_conf, 1)))))
cat("\n")

severity_long <- quintile_data %>%
  select(q_label, minor, intermediate) %>%
  pivot_longer(c(minor, intermediate), names_to = "severity", values_to = "n") %>%
  mutate(
    severity = recode(severity, minor = "Minor conflict",
                      intermediate = "Intermediate conflict"),
    severity = factor(severity, levels = c("Minor conflict",
                                            "Intermediate conflict"))
  )

fig3 <- ggplot(severity_long, aes(x = q_label, y = n, fill = severity)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.4) +
  scale_fill_manual(values = c("Minor conflict" = "#fee08b",
                               "Intermediate conflict" = "#b2182b")) +
  scale_y_continuous(labels = scales::label_number(),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Conflict in poor countries is not only more likely, it is also more intense",
# >> The bars are COUNTS of conflict years, so the poorest quintile has the
# >> tallest bar mainly because it has the most conflict years. For "more
# >> intense" you need the SHARE of conflict years that are wars, per quintile:
# >> 30% (Q1), 29% (Q2), 28% (Q3), 9% (Q4), 12% (Q5). That supports a weaker
# >> claim: wars are less common among the richest conflicts, but not steadily
# >> more common as income falls.
    subtitle = paste0("Severity of the conflicts that did occur, by GDP per capita quintile ",
                      "(counts of conflict country-years)\n",
                      "No country-year in 1990-2025 reached UCDP's 'war' level (1,000+ battle deaths)"),
    x = "GDP per capita quintile (US$ per person)",
    y = "Number of conflict country-years",
    caption = paste0("Source: UCDP/PRIO Armed Conflict Dataset v26.1 and World Bank GDP per capita PPP.\n",
                     "Q1 = poorest fifth of country-years, Q5 = richest fifth.")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 9.5, colour = "grey30", lineheight = 1.25),
    plot.caption  = element_text(size = 8, colour = "grey40", hjust = 0),
    legend.position = "top",
    legend.title  = element_blank(),
    axis.text.x   = element_text(size = 8.5),
    panel.grid.minor = element_blank()
  )

ggsave("output/fig3_gdp_conflict_intensity.png", fig3,
       width = 10, height = 6, dpi = 150)
write_csv(quintile_data, "output/fig3_gdp_conflict_intensity.csv")

cat("STEP 7 - Written: output/fig3_gdp_conflict_intensity.png\n\n")

# -----------------------------------------------------------------------------
# STEP 8 - Save a text log of every diagnostic printed above
# -----------------------------------------------------------------------------
# The console output is easy to lose, so we also write it to a file.
sink("output/visualisation_diagnostics.txt")
cat("GDP per capita vs armed conflict - visualisation diagnostics\n")
cat("Generated from output/lab2_full_table.csv\n")
cat("=============================================================\n\n")
cat("Rows:", nrow(lab2), " Countries:", n_distinct(lab2$country),
    " Years:", min(lab2$year), "-", max(lab2$year), "\n")
cat("Conflict country-years:", sum(lab2$conflict == 1),
    "(", round(100 * mean(lab2$conflict), 2), "%)\n")
cat("GDP imputed (income-group median):", sum(lab2$gdp_imputed), "rows\n\n")
cat("--- Binned conflict rate (", N_BINS, "equal-count bins) ---\n")
print(as.data.frame(binned))
cat("\n--- Conflict rate by income group and decade ---\n")
print(as.data.frame(decade_data))
cat("\n--- Severity by GDP quintile ---\n")
print(as.data.frame(quintile_data))
cat("\n--- Robustness: GDP effect under different controls ---\n")
print(summary(trend_fit))
print(summary(trend_year))
print(summary(trend_full))
cat("\n--- Correlation between log GDP and conflict within each income group ---\n")
print(as.data.frame(within_group))
sink()

cat("STEP 8 - Written: output/visualisation_diagnostics.txt\n\n")

# -----------------------------------------------------------------------------
# STEP 9 - Final summary
# -----------------------------------------------------------------------------
cat("=== Summary of the visualisation choice ===\n")
cat("1. MAIN FIGURE (fig1): binned scatter + logistic trend.\n")
cat("   A raw scatter of all 6,228 country-years was rejected because only\n")
cat("   17.6% are conflict years, so the two groups overlap into a hairball.\n")
cat("   Binning into", N_BINS, "equal-count bins makes the inverse\n")
cat("   relationship directly readable and gives each point an honest CI.\n")
cat("2. LOG X-AXIS: GDP spans a factor of 292 and is right-skewed (0.39);\n")
cat("   on the log scale it is symmetric (-0.05), so bins are comparable.\n")
cat("3. LOGISTIC, NOT LINEAR: the outcome is 0/1, so a linear fit could\n")
cat("   predict probabilities outside [0,1]. Odds ratio per GDP doubling:",
    round(exp(coef(trend_fit)[2] * log(2)), 3), "\n")
cat("4. SUPPORTING FIGURES: fig2 adds TIME (the low-income conflict rate\n")
cat("   RISES while the high-income rate FALLS); fig3 adds SEVERITY (poor\n")
cat("   countries have not only more but also more intense conflicts).\n")
cat("5. IMPUTED GDP: 287 rows filled with the income-group median and\n")
cat("   reported in the caption, so no observation is silently discarded.\n")
cat("6. HONESTY CHECK: the pooled GDP effect is strong and survives adding\n")
# >> FOR SESSION 4: your figure already has error bars. Bring one question:
# >> how much wider would they be if we allowed for the same countries being
# >> counted 36 times? Write down your guess before class.
cat("   year, but inside each income group the correlation is near zero, so\n")
cat("   the difference is mostly BETWEEN groups rather than a gradient\n")
cat("   WITHIN a group. Figure 1 colours bins by income group to show this.\n")
