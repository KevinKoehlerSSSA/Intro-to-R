# =============================================================================
# FIGURE G (reader appendix) -- ANNOTATED COPY of 05_conflict_by_income.R (Lab 2, Step 5 figure,
# sent 11 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I ran it on lab2_full_table.csv, saved as
# output/conflict_income.csv; it runs as it is. My numbers: 36.6% of
# country-years in conflict in the low income group, 21.7% lower middle,
# 14.5% upper middle, 4.7% high income. Gap low vs high: 31.9 points.
#
# THE SHORT VERSION
#   + Income from the PREVIOUS year, and the comment says why.
#   + The groups come from each country-year's own income, not from today's
#     World Bank label, so a country moves group when its income moves
#     (95 countries do). And you checked how often your groups differ from
#     the label: a good habit.
#   + Shares with the peaceful years in the denominator, and the overall
#     share is pooled correctly (sum / sum, not a mean of yearly means).
#   + The claim in the title is checked by a line of code (35 of 35 years).
#   + Short and readable, with nrow() after every step. You could explain
#     every line of it, which is the course rule.
#   - The cut points are fixed in dollars for 35 years while incomes rose.
#     The low income group shrinks from 43 countries (1995) to 20 (2025),
#     and the countries left in it are the ones that did not grow, often
#     because of war. The rise of the red line after 2015 is partly that.
#   - Yearly shares for groups of 20 to 60 countries jump around: in the
#     low income group in 2022, one country moves the line by 4.5 points.
#   - The legend is cut off at 9 inches wide ("High inco..."), and the
#     red-green palette is hard for colour-blind readers.
# =============================================================================

# Lab 2, step 5: is internal conflict concentrated in poorer countries?
#
# What this script does:
#   1. reads the country-year table built in the first steps (output/conflict_income.csv)
#   2. computes the GDP per capita of the previous year of every country, so that the
#      income that classifies a country-year comes before the conflict
#   3. builds the four income groups out of that previous year's income, with the cut
#      points taken from the income groups of the table
#   4. computes the share of country-years with a conflict for every group, year by year
#   5. draws the figure and saves it as output/step5.png

library(tidyverse)


# ---- 1. Read the country-year table ----

d <- read_csv("output/conflict_income.csv")
nrow(d)


# ---- 2. The income of the previous year ----

# The rows are already sorted by country and year, so lag() gives the previous year
# of the same country. We use it to avoid the reverse causality: a conflict can lower
# the income of the year in which it happens, so the income we compare has to come
# from before.
d <- d %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(gdppc_prev = lag(gdppc)) %>%
  ungroup()
# >> lag() takes the previous ROW, not the previous year. That is right here
# >> only because the table is balanced (173 countries x 36 years, no gaps).
# >> If a year were missing, lag() would silently give the year before the
# >> gap. Say in a comment that you checked this.
# >> Also think about what the lag can do. For a conflict that STARTS this
# >> year, last year's income was not yet affected. For a war in its fifth
# >> year, last year's income was already lowered by four years of war, and
# >> most conflict country-years are continuing conflicts. The clean version
# >> compares income in the year before ONSET.
nrow(d)

# How many country-years have an income of the previous year?
sum(!is.na(d$gdppc_prev))
sum(is.na(d$gdppc_prev) & d$year == 1990)
# >> 5,779 rows with a previous-year income, so 449 are lost: all 173 rows of
# >> 1990 (no 1989 in the table) and 276 where last year's income is missing.
# >> Write the two numbers in a comment, so a reader knows what was dropped
# >> without running the script.


# ---- 3. The four income groups, built from the previous year's income ----

# The income group of the table is a label of the country, the same in every year.
# We take the median income of each of the four groups and put the cut points in the
# middle of two groups, so that the cut points are the ones of the table.
medians <- d %>%
  filter(!is.na(gdppc)) %>%
  group_by(income_group) %>%
  summarise(median_gdppc = median(gdppc)) %>%
  arrange(median_gdppc)
nrow(medians)
medians
# >> A neat idea: you borrow the World Bank labels but classify every
# >> country-year by its own income. Two things to know. The World Bank
# >> groups are defined on GNI per capita (Atlas method) with thresholds such
# >> as $1,145, not on GDP per capita at PPP. So "Low income" here means
# >> "previous-year GDP per capita (PPP) below $3,016", not the World Bank
# >> group. Give the cut points in the caption, so the labels cannot mislead.

cut_low <- (medians$median_gdppc[1] + medians$median_gdppc[2]) / 2
cut_middle <- (medians$median_gdppc[2] + medians$median_gdppc[3]) / 2
cut_upper <- (medians$median_gdppc[3] + medians$median_gdppc[4]) / 2
round(c(cut_low, cut_middle, cut_upper))
# >> 3016, 8560, 27322. These are fixed for all 35 years. That is fine for
# >> the dollars (the series is in constant 2021 prices), but incomes grew:
# >> the low income group has 43 countries in 1995 and 20 in 2025. Run
# >>   d %>% filter(!is.na(income_group_year)) %>% count(year, income_group_year)
# >> to see it. Your groups answer "is conflict more common below $3,016?",
# >> which is a fair question, but the group is not the same set of countries
# >> over time. Quartiles cut within each year (the Session 4 reader) keep the
# >> groups the same size every year.

# Now every country-year belongs to the group of its income of the previous year.
d <- d %>%
  mutate(income_group_year = case_when(
    is.na(gdppc_prev) ~ NA_character_,
    gdppc_prev <= cut_low ~ "Low income",
    gdppc_prev <= cut_middle ~ "Lower middle income",
    gdppc_prev <= cut_upper ~ "Upper middle income",
    TRUE ~ "High income"
  ))
nrow(d)

# The groups, from the poorest to the richest
d <- d %>%
  mutate(income_group_year = factor(income_group_year,
                                    levels = c("Low income", "Lower middle income",
                                               "Upper middle income", "High income")))

# How many country-years land in the same group as the income group of the table?
sum(d$income_group_year == d$income_group, na.rm = TRUE)
sum(!is.na(d$income_group_year) & d$income_group_year != d$income_group)
# >> 4,336 the same, 1,443 different: a quarter of the country-years land in
# >> a different group from the World Bank label. Good check; write the
# >> result next to it.

# How many countries change group over the years?
d %>%
  filter(!is.na(income_group_year)) %>%
  group_by(iso3c) %>%
  summarise(n_groups = n_distinct(income_group_year)) %>%
  summarise(n_countries_moving = sum(n_groups > 1))
# >> 95 countries change group at least once. That is the reason to classify
# >> by income year by year rather than by today's label.


# ---- 4. The share of country-years with a conflict ----

# The share is the number of country-years with a conflict divided by the number of
# country-years of the group and year.
by_year <- d %>%
  filter(!is.na(income_group_year)) %>%
  group_by(year, income_group_year) %>%
  summarise(n = n(), n_conflict = sum(conflict), conflict_share = mean(conflict) * 100,
            .groups = "drop")
# >> Good: mean(conflict) is the share, with the peaceful years in the
# >> denominator. You keep n, but you never look at it. The low income group
# >> has 20 to 43 countries per year, so each yearly point rests on very few
# >> countries. Print n, or put it in the figure.
nrow(by_year)

# The same numbers side by side, one column per income group
by_year_wide <- by_year %>%
  select(year, income_group_year, conflict_share) %>%
  pivot_wider(names_from = income_group_year, values_from = conflict_share)
nrow(by_year_wide)
by_year_wide

# How many of the years have more conflict in the low income group than in the high income group?
sum(by_year_wide$`Low income` > by_year_wide$`High income`)
# >> Good: the claim in the title is counted, not eyeballed. 35 of 35 for
# >> low vs high. The full order (low > lower middle > upper middle > high)
# >> holds in 33 years; 2004 and 2007 break it. Each year is a separate
# >> comparison, so a few exceptions say little (Reader, Section 8).

# The share over all the years together
by_year %>%
  group_by(income_group_year) %>%
  summarise(n_country_years = sum(n), n_conflict = sum(n_conflict),
            conflict_share = round(sum(n_conflict) / sum(n) * 100, 2))
# >> Correct pooling: sum(n_conflict) / sum(n) weights each year by its size.
# >> 36.6 / 21.7 / 14.5 / 4.7%. The gap (31.9 points) is a bit larger than the
# >> reader's 28.5 because your groups are not equal: your low income group
# >> is the poorest 18% of country-years, not the poorest quarter.


# ---- 5. The figure ----

# One line for every income group. If the claim holds, the line of the low income
# group stays above the line of the high income group in every year.
# >> The red line rises from about 30% to 55-64% after 2018. It is tempting
# >> to read that as "conflict in poor countries is rising" or "the gap is
# >> widening". Part of it is who is left in the group. Of the 43 countries
# >> in it in 1995, 22 grew out of it by 2022 (China, India, Bangladesh,
# >> Ghana, Tanzania, ...). Those 22 had a conflict in 21% of their years
# >> 1991-2025; the 21 that stayed poor, in 38%. Conflict is one reason a
# >> country stays poor. With your groups the gap is 29.9 points in 1991-2005
# >> and 38.6 in 2011-2025; with quartiles cut within each year it changes by
# >> 4.3 points, well within chance (Reader, Section 7).
ggplot(by_year, aes(x = year, y = conflict_share, colour = income_group_year)) +
  geom_line() +
  geom_point(size = 1.5) +
  scale_colour_brewer(palette = "RdYlGn") +
# >> Red-Yellow-Green: red and green are the pair colour-blind readers
# >> confuse most, and red = poor, green = rich reads like a judgement. An
# >> ordered palette that avoids this: scale_colour_brewer(palette = "RdYlBu")
# >> or scale_colour_viridis_d().
  labs(
    title = "Internal conflict is concentrated in the poorer countries in every year since 1991",
    x = "Year (calendar year)",
    y = "Country-years with an internal conflict\n(% of the country-years of the group)",
    colour = "Income group (previous year's GDP per capita)",
# >> At width = 9 this legend does not fit: the saved figure cuts it off at
# >> "High inco...". Shorten the title (e.g. colour = "Income group") and
# >> put the definition in the caption, or add
# >>   guides(colour = guide_legend(nrow = 2))
    caption = "Sources: UCDP/PRIO Armed Conflict Dataset v26.1; World Bank World Development Indicators (GDP per capita, PPP)"
# >> Add the definition of the groups here: previous-year GDP per capita,
# >> PPP, constant 2021 $; below 3,016 / 3,016-8,560 / 8,560-27,322 / above.
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 9, hjust = 0))

ggsave("output/step5.png", width = 9, height = 6)
# >> FOR SESSION 4: your test is the gap between the low and the high income
# >> group, 36.6% against 4.7%. Before class, write down how you would find
# >> out whether a gap that size could come from chance, given that the same
# >> countries appear 35 times each.
