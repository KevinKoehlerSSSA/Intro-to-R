# =============================================================================
# FIGURE C (reader appendix) -- ANNOTATED COPY of 04_conflict_income_figure.R (Lab 2, Step 5 figure,
# sent 6 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I ran it on lab2_full_table.csv; it runs as it is.
#
# THE SHORT VERSION
#   + The most careful script of the five. It checks every assumption before
#     using it and STOPS if one fails: missing values, countries that change
#     group, group sizes that change from year to year. It prints the years
#     that break the pattern instead of just printing FALSE. Keep doing this.
#   + The legend shows how many countries each line averages over.
#   - The y axis is the average NUMBER of conflicts per country, not the share
#     of countries in conflict. The claim is about WHERE conflict happens.
#     Counting conflicts gives extra weight to countries with several at once
#     (DR Congo had 4 in 2020; Afghanistan, Ethiopia, Mali, Somalia, Syria,
#     Yemen 2 each). 27% of conflict country-years have more than one.
#     A defensible choice, but say why you made it.
#   - "Poorer" here means today's World Bank group, applied back to 1990.
#     Countries impoverished by recent wars (Syria, Yemen) count as poor in
#     1995. No GDP figure is used at all.
#   - "Low income is on top in every year" is a test with a pass mark of 36
#     out of 36. It fails (33 of 36). Does that make the claim false? That
#     question is Session 4.
# =============================================================================

# =============================================================================
# 04_conflict_income_figure.R
#
# GOAL
#   Test whether internal conflict since 1990 is concentrated in poorer
#   countries, and save the figure as output/step5.png.
#
# THE TEST
#   For every year from 1990 to 2025 and for every income group we calculate
#   the AVERAGE NUMBER OF INTERNAL CONFLICTS PER COUNTRY:
#
# >> This is a clear statement of the measure: good. Compare it with the claim.
# >> "Internal conflict is concentrated in poorer countries" is about how many
# >> countries (or country-years) are in conflict. Your measure mixes that with
# >> how many conflicts each country has. In the low-income group, the share of
# >> country-years in conflict rises from 38% (1990-2010) to 54% (2011-2025);
# >> your average rises from 0.43 to 0.82 conflicts. Part of your line's rise
# >> is more conflicts in the SAME countries, not more countries in conflict.
#       average = (number of internal conflicts in the whole group that year)
#                 / (number of countries in that income group)
#
#   The table has one row per country and per year. The column "n_conflicts"
#   holds how many internal conflicts (UCDP type 3 or 4) that country had that
#   year: 0 if it had none, 1 if it had one, and more if it had several
#   separate internal conflicts. The average of that column over the countries
#   of an income group is therefore exactly "the average number of internal
#   conflicts in a country".
#
#   Because a country can have more than one conflict in the same year, the
#   average can be larger than 1. That means "on average every country in the
#   group had one internal conflict AND some of them had a second one".
#
#   If internal conflict is concentrated in poorer countries, the line of the
#   LOW income group stays on top, the line of the HIGH income group stays at
#   the bottom, and the two lines in between keep their order.
#
# INPUT
#   data/lab2_full_table.csv   (173 countries x 36 years = 6228 rows)
#
# OUTPUTS
#   output/step5.png           the figure
#   output/step5_data.csv      the table behind the figure (4 groups x 36 years)
# =============================================================================

library(tidyverse)


# --------------------------------------------------------------- settings ----
IN_FILE  <- "data/lab2_full_table.csv"
OUT_PNG  <- "output/step5.png"
OUT_CSV  <- "output/step5_data.csv"

YEAR_FROM <- 1990
YEAR_TO   <- 2025

# The order of the income groups, from poorest to richest. This order also
# decides the order of the legend and of the colours.
INCOME_ORDER <- c("Low income", "Lower middle income",
                  "Upper middle income", "High income")

# Colours for the four groups. They come from the "Okabe-Ito" palette, which
# was built for people with colour blindness: the four colours stay easy to
# tell apart also when they are printed in grey. The colours run from warm
# (poor) to cool (rich).
INCOME_COLOURS <- c("Low income"           = "#D55E00",  # vermillion
                     "Lower middle income"  = "#E69F00",  # orange
                     "Upper middle income"  = "#009E73",  # bluish green
                     "High income"          = "#0072B2")  # blue


# =============================================================== STEP 1 ======
# Read the table.
lab2 <- read_csv(IN_FILE, show_col_types = FALSE)

cat("STEP 1 - table read from", IN_FILE, "\n")
cat("         rows   :", nrow(lab2), "\n")
cat("         columns:", paste(names(lab2), collapse = ", "), "\n")
cat("         countries:", n_distinct(lab2$country),
    "| years:", min(lab2$year), "-", max(lab2$year), "\n")


# =============================================================== STEP 2 ======
# Check the two columns that the figure needs before we use them.
# The average of a column can only be calculated if the column has no missing
# values, and we want to know that BEFORE we average instead of finding out
# afterwards.
cat("\nSTEP 2 - checking the two columns we need\n")
cat("         rows with a missing 'n_conflicts'   :", sum(is.na(lab2$n_conflicts)), "\n")
cat("         rows with a missing 'income_group' :", sum(is.na(lab2$income_group)), "\n")

# We do NOT delete any rows. We only continue if there is nothing to delete,
# so that a silent change of the data cannot change the result.
# >> Excellent. A check that stops the script is worth more than a check that
# >> prints a number nobody reads.
if (any(is.na(lab2$n_conflicts)) | any(is.na(lab2$income_group))) {
  stop("There are missing values in 'n_conflicts' or 'income_group' - stopping, \
because the average would silently be calculated on fewer countries.")
}
cat("         -> both columns are complete, so no row is dropped and\n",
    "            no na.rm = TRUE is needed anywhere in this script.\n", sep = "")

# The income group must be the same for a country in all 36 years, otherwise
# a country would move between the lines and the comparison would be unfair.
check_group <- lab2 |>
  distinct(country, income_group) |>
  count(country)

# >> This check passes because income_group is ONE value per country: the World
# >> Bank's classification today. That is why no country "moves between the
# >> lines". It also means that 1990s Syria (a lower-middle-income country
# >> before 2011) sits in the low-income line. Is a fixed group what you want,
# >> or is it a side effect of the data you had?
cat("\nSTEP 3 - checking that the income group of a country does not change\n")
cat("         countries with more than one income group:", sum(check_group$n > 1), "\n")
if (any(check_group$n > 1)) {
  stop("At least one country changes its income group over the years - stopping.")
}
cat("         -> every country keeps the same income group in all 36 years.\n")


# =============================================================== STEP 4 ======
# The heart of the script: one row per income group and per year, with the
# average number of internal conflicts per country.
#
#   mean(n_conflicts) = the average number of internal conflicts per country
#   n_countries       = how many countries the average is based on (the
#                       denominator, useful to check that it does not change)
#   total_conflicts   = how many internal conflicts the whole group had
#   n_with_conflict   = how many of those countries had at least one conflict
yearly <- lab2 |>
  filter(year >= YEAR_FROM, year <= YEAR_TO) |>
  group_by(income_group, year) |>
  summarise(
    avg_conflicts_per_country = mean(n_conflicts),
    n_countries                = n(),
    total_conflicts            = sum(n_conflicts),
    n_with_conflict            = sum(conflict),
    .groups = "drop"
  ) |>
  mutate(income_group = factor(income_group, levels = INCOME_ORDER)) |>
  arrange(income_group, year)

cat("\nSTEP 4 - averaged per income group and per year\n")
cat("         rows in the result:", nrow(yearly), "(4 groups x 36 years = 144)\n")
cat("         countries per group (the denominator of the average):\n")
print(yearly |> distinct(income_group, n_countries) |>
        as.data.frame() |> setNames(c("income_group", "n_countries")))

# The denominator has to be the same in every year, otherwise a line that
# moves could just be a line whose denominator moved.
denominator_constant <- yearly |>
  group_by(income_group) |>
  summarise(same_every_year = n_distinct(n_countries) == 1, .groups = "drop")

cat("\n         is the number of countries the same in every year?\n")
print(as.data.frame(denominator_constant))
if (!all(denominator_constant$same_every_year)) {
  stop("The number of countries per group changes over the years - stopping.")
}
cat("         -> yes, so the four lines can be compared directly.\n")

# The same check for the group sizes themselves.
cat("\n         groups and their number of countries:\n")
print(as.data.frame(yearly |> distinct(income_group, n_countries)))


# =============================================================== STEP 5 ======
# The result of the test, printed as a table: the average number of internal
# conflicts per country for every group, over the whole period 1990-2025.
summary_table <- yearly |>
  group_by(income_group) |>
  summarise(
    avg_over_1990_2025  = mean(avg_conflicts_per_country),
    min_yearly_avg     = min(avg_conflicts_per_country),
    max_yearly_avg     = max(avg_conflicts_per_country),
    n_countries        = first(n_countries),   # the same in all years (STEP 4)
    .groups = "drop"
  )

cat("\nSTEP 5 - the test in numbers: average number of internal conflicts\n",
    "         per country, 1990-2025 (the mean of the yearly averages)\n", sep = "")
print(as.data.frame(summary_table))

# If the conflict is concentrated in poor countries, the poorest group must
# have the highest average and the richest group the lowest one, in every
# single year.
# >> Nice idea: a test you decided on before looking. Notice what kind of test
# >> it is: 36 separate comparisons, all of which must go one way. With 25
# >> low-income countries, one or two conflicts more or less in a group moves
# >> a line a lot, so a strict "every year" rule will fail by chance even if
# >> the claim is true. In your data it fails in 1993, 1995 and 2004.
# >> Next session we compare this with a single test on all years together.
rank_each_year <- yearly |>
  group_by(year) |>
  summarise(highest = income_group[which.max(avg_conflicts_per_country)],
            lowest  = income_group[which.min(avg_conflicts_per_country)],
            .groups = "drop")

cat("\n         is 'Low income' the group with the highest average in every year? ",
    all(rank_each_year$highest == "Low income"), "\n", sep = "")
cat("         is 'High income' the group with the lowest average in every year? ",
    all(rank_each_year$lowest == "High income"), "\n", sep = "")
cat("         how often is each group the highest one? (36 years)\n")
print(as.data.frame(sort(table(rank_each_year$highest), decreasing = TRUE)))

# The years in which the poorest group is NOT on top are printed with their
# numbers, so that the exception can be judged and not just read as "FALSE".
exceptions <- rank_each_year |> filter(highest != "Low income")
if (nrow(exceptions) > 0) {
  cat("\n         the years in which the poorest group is NOT on top:\n")
# >> A loop to print three years. It works, but the course rule (AGENTS.md) is
# >> the simplest code you can explain: print(exceptions) would show the same
# >> information. Can you say what seq_len() returns when there are no rows?
  for (i in seq_len(nrow(exceptions))) {
    yy <- exceptions$year[i]
    cat("           year", yy, "-> highest group:", as.character(exceptions$highest[i]), "\n")
    print(as.data.frame(yearly |> filter(year == yy) |>
           select(income_group, avg_conflicts_per_country, n_countries,
                  total_conflicts, n_with_conflict)))
  }
}
cat("\n         -> the poorest group is on top in ", nrow(rank_each_year) - nrow(exceptions),
    " of the ", nrow(rank_each_year), " years and the richest group is at the bottom in ",
    sum(rank_each_year$lowest == "High income"), " of them.\n", sep = "")

# The order of the four groups over the whole period: from the highest
# average to the lowest one.
order_by_avg <- summary_table |>
  arrange(desc(avg_over_1990_2025)) |>
  pull(income_group)

cat("         order of the groups, from the highest to the lowest average:\n")
cat("           ", paste(order_by_avg, collapse = "  >  "), "\n", sep = "")

# If the averages fall at every step of the income ladder, the conflict is
# concentrated in poor countries in a strict sense. We put the four groups in
# the order from poor to rich on purpose, so that "the next step" is always
# the next richer group.
avg_by_poor_to_rich <- summary_table |>
  arrange(match(as.character(income_group), INCOME_ORDER)) |>
  pull(avg_over_1990_2025)

monotone <- all(diff(avg_by_poor_to_rich) < 0)
# >> TRUE here (0.59 > 0.34 > 0.22 > 0.05). A good summary. Next question:
# >> how much could these four numbers move by chance alone?
cat("         do the averages fall at every step from poor to rich? ", monotone, "\n", sep = "")
if (monotone) {
  cat("         -> yes: the poorer the group, the more internal conflicts per country.\n")
}


# =============================================================== STEP 6 ======
# Draw the figure.
#
#   x axis : the year, from 1990 to 2025
#   y axis : the average number of internal conflicts in a country
#   colour : the income group
#   one line per income group, with one dot for every year

# How many countries each group has. We take them from the data instead of
# writing them by hand, so the legend cannot go out of date.
n_per_group <- yearly |>
  distinct(income_group, n_countries) |>
  arrange(income_group) |>
  pull(n_countries)

# The top of the y axis. A country can have more than one internal conflict
# in the same year, so the average can be larger than 1. We take the largest
# value from the data and round it up to the next 0.2, so that the axis always
# fits and the ticks stay simple numbers.
y_top <- ceiling(max(yearly$avg_conflicts_per_country) * 5) / 5

cat("         largest value in the figure:", max(yearly$avg_conflicts_per_country),
    "-> the y axis goes to", y_top, "\n")

fig <- ggplot(
  yearly,
  aes(x = year, y = avg_conflicts_per_country, colour = income_group, group = income_group)
) +
  geom_line(linewidth = 0.9) +          # the line that connects the years
  geom_point(size = 1.6) +              # one dot per year and per group
  scale_colour_manual(
    values   = INCOME_COLOURS,
    breaks   = INCOME_ORDER,
    # The number of countries is written in the legend because the four
    # averages are based on a different number of countries (25 to 55).
    labels   = paste0(INCOME_ORDER, " (", n_per_group, " countries)")
  ) +
  scale_x_continuous(
    breaks = seq(1990, 2025, 5),        # a label every 5 years, not every year
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(0, y_top, 0.2),
    limits = c(0, y_top),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = "Internal conflict is concentrated in poorer countries",
# >> A title that states the finding: what we asked for. But the figure shows
# >> conflicts per country, so the precise claim is "poorer countries have more
# >> internal conflicts per country". Small difference, worth getting right.
    subtitle = paste0(
      "Average number of internal conflicts per country, by income group, 1990-2025.\n",
      "0 = no internal conflict at all in the group that year, ",
      "1 = one internal conflict per country on average."
    ),
    x = "Year",
    y = "Average number of internal conflicts per country",
    colour = "Income group"
  ) +
  guides(colour = guide_legend(title = "Income group", override.aes = list(size = 3, linewidth = 1))) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 10, colour = "grey30"),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    axis.title       = element_text(size = 11)
  )

cat("\nSTEP 6 - the figure is ready\n")


# =============================================================== STEP 7 ======
# Save the figure and the table behind it.
ggsave(OUT_PNG, fig, width = 11, height = 6.5, dpi = 300)

write_csv(yearly |> mutate(income_group = as.character(income_group)), OUT_CSV)

cat("\nSTEP 7 - written\n")
cat("         ", OUT_PNG, "\n")
cat("         ", OUT_CSV, "\n")
cat("\nThe first and the last year of every group:\n")
print(as.data.frame(yearly |> filter(year %in% c(1990, 2025))))
# >> FOR SESSION 4: write down, before class, what you think would happen to
# >> this figure if income had NOTHING to do with conflict. Would the four lines
# >> lie exactly on top of each other? How far apart could they be by chance?
