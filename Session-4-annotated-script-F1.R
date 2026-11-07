# =============================================================================
# ANNOTATED COPY of 06_step6_figure.R (Lab 2, Step 6 figure, sent 7 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. Your input file (output/conflict_income_full.csv) was not in what
# you sent, so I rebuilt it: 1990-2025 from lab2_full_table.csv (with income
# lagged by one year), 1946-1989 from UCDP/PRIO v26.1 (internal conflicts,
# types 3 and 4). The script ran without changes. My numbers: poorest fifth of
# country-years 34.2% with conflict, richest fifth 2.9%. Yours may differ a
# little if you built the table differently.
#
# THE SHORT VERSION
#   + Income from the PREVIOUS year: the only script that deals with reverse
#     causation, and the comment says why.
#   + Shares with the peaceful years in the denominator, in two units
#     (country-years and countries), plus wars separately.
#   + Honest about the data: the years without income get their own panel
#     instead of being forced onto the income axis. The note computes its
#     numbers from the data instead of typing them.
#   + Careful details: include.lowest explained, thin bands set to NA rather
#     than dropped, a minimum of 50 country-years per band.
#   - "The conflict rate falls as income rises, in every view" is typed by
#     hand and is not quite true (see below).
#   - Lagging by one year does not remove reverse causation for conflicts
#     that have lasted for years.
#   - Four panels for one claim. Panel 1 answers the question; the others
#     dilute it.
# =============================================================================

# Figure: GDP per capita PPP and internal conflict, 1946-2025.
# Reads output/conflict_income_full.csv and draws output/step6.png.
#
# Design (decided with the student):
#   - The figure is about the income-conflict relationship, so the main
#     scatter uses the years where both exist (1990-2025).
#   - Conflict = any internal conflict (UCDP types 3 and 4); the high
#     intensity subset (war) is highlighted separately.
#   - Income is GDP per capita PPP on a log scale.
#   - Form: scatter of every country-year plus a binned conflict-rate curve.
#
# The GDP per capita PPP file has no values before 1990, so the 1946-1989
# conflict years cannot be placed on an income axis. They are shown in a
# separate panel, so the full conflict history is visible without
# pretending it can be compared with income.

library(tidyverse)
library(patchwork)

# ---- Read the full panel ----
# >> Your script starts from a file made by an earlier script that you did not
# >> send. Send the whole pipeline (or the file), so someone else can run it.
panel <- read_csv("output/conflict_income_full.csv", show_col_types = FALSE)

nrow(panel)
range(panel$year)

# ---- The years that have both income and conflict ----
# GDP per capita PPP starts in 1990, so this is where the two can be compared.
with_gdp <- panel %>%
  filter(!is.na(gdp_per_capita))

nrow(with_gdp)

# The conflict years in this window
sum(with_gdp$conflict == 1)
sum(with_gdp$conflict_war == 1)


# ---- Panel 1 data: every country-year with a GDP value ----
# We use LAST year's GDP per capita, so the income we are testing was already
# there before the conflict. That avoids conflict having lowered the income
# in the same year and making poverty look like a cause when it is partly
# an effect.
# >> Previous-year income: good, and well explained. But think about what it
# >> can and cannot do. For a conflict that STARTS this year, last year's
# >> income was not yet affected by it. For a conflict in its fifth year,
# >> last year's income was already lowered by four years of war. Most
# >> conflict country-years are continuing conflicts, so the lag removes
# >> only part of the problem. The clean version compares income before
# >> ONSET: keep only the first year of each conflict episode (and the
# >> peaceful years), and use income in the year before.
country_years <- panel %>%
  filter(!is.na(gdp_per_capita_lag)) %>%
  mutate(log_income = log(gdp_per_capita_lag))

nrow(country_years)

# >> A correlation between a 0/1 variable and log income (-0.29) is hard to
# >> read. Your shares (34% vs 3%) say the same thing in a way anyone can
# >> understand, and they are what you put in the note. The correlation adds
# >> little; you could drop it from the subtitle.
# The correlation between conflict and log income
cor(country_years$conflict, country_years$log_income)

# The same, but counting only wars
cor(country_years$conflict_war, country_years$log_income)

# Split into five income groups, so we can report the poorest and richest group
country_year_bins <- country_years %>%
  mutate(income_bin = ntile(log_income, 5)) %>%
  group_by(income_bin) %>%
  summarise(n_rows = n(),
            pct_conflict = 100 * mean(conflict),
            pct_war = 100 * mean(conflict_war),
            median_income = median(gdp_per_capita_lag),
            .groups = "drop")

country_year_bins

# ---- The binned curve: the share with a conflict as income rises ----
# The income range is cut into 20 bands on the log scale, so the bands are
# equal widths on the x-axis.
# include.lowest = TRUE: without it the smallest log_income falls outside the
# first band (5.54, 5.87] and is put in an NA band, which is dropped later.
country_years <- country_years %>%
  mutate(band = cut(log_income,
                    breaks = seq(min(log_income), max(log_income), length.out = 21),
                    include.lowest = TRUE))
# >> Good comment: you found the problem and explained the fix. (On my
# >> rebuilt table the first band starts at 6.24, not 5.54, because the
# >> lowest incomes differ; the comment will go stale if the data change.)

curve_country_years <- country_years %>%
  group_by(band) %>%
  summarise(n_rows = n(),
            log_income = mean(log_income),
            pct_conflict = 100 * mean(conflict),
            pct_war = 100 * mean(conflict_war),
            .groups = "drop")

# Bands with few country-years are set to NA rather than dropped, so the
# line breaks there instead of jumping across the gap.
curve_country_years <- curve_country_years %>%
# >> Good: a band with 18 country-years would give a share that jumps around
# >> by chance. Setting it to NA rather than dropping it breaks the line
# >> honestly. Say in the figure that bands under 50 are not drawn: the red
# >> line starts at $1,000 while the dots start at $500.
  mutate(pct_conflict = ifelse(n_rows >= 50, pct_conflict, NA),
         pct_war = ifelse(n_rows >= 50, pct_war, NA))

curve_country_years


# ---- Panel 2 data: countries ----
# Each country appears once, with its average GDP per capita and the share
# of its years that had a conflict. We use last year's GDP here too, so the
# income was there before the conflict.
# >> Panel 2 changes the unit: one value per country, so Luxembourg and
# >> India count the same, and each country gets its average income over 35
# >> years. That is a legitimate second view, but it answers a different
# >> question ("are poor COUNTRIES more conflict-prone?"). Say which one the
# >> claim is about.
countries <- panel %>%
  filter(!is.na(gdp_per_capita_lag)) %>%
  group_by(iso3c) %>%
  summarise(country = first(country),
            avg_income = mean(gdp_per_capita_lag),
            pct_years_conflict = 100 * mean(conflict),
            pct_years_war = 100 * mean(conflict_war),
            .groups = "drop") %>%
  mutate(log_income = log(avg_income))

nrow(countries)

# The correlation at the country level
cor(countries$pct_years_conflict, countries$log_income)

# Divide the countries into five income groups
countries <- countries %>%
  mutate(income_group = ntile(avg_income, 5))

country_bins <- countries %>%
  group_by(income_group) %>%
  summarise(n_countries = n(),
            mean_pct_conflict = mean(pct_years_conflict),
            mean_pct_war = mean(pct_years_war),
            median_income = median(avg_income),
            .groups = "drop")

country_bins


# ---- Panel 3 data: the conflict history, 1946-2025 ----
# This uses every year, including the ones with no GDP.
# >> Panel 3 counts countries in conflict per year. The number of independent
# >> countries rose from about 70 in 1946 to almost 200 after decolonisation
# >> and 1991, so part of the rise is just more countries. That is the
# >> "compared with what?" question from the Session 4 reader: a count needs
# >> a denominator. Plot the SHARE of countries in conflict, or drop the panel:
# >> it does not test the claim about income.
per_year <- panel %>%
  group_by(year) %>%
  summarise(n_conflict = sum(conflict),
            n_war = sum(conflict_war),
            .groups = "drop")

nrow(per_year)

# The years before 1990, which have conflict but no income
pre_gdp <- per_year %>% filter(year < 1990)

nrow(pre_gdp)
sum(pre_gdp$n_conflict)


# ---- The x-axis labels, in dollars ----
income_breaks <- c(log(1000), log(3000), log(10000), log(30000), log(100000))
income_labels <- c("1,000", "3,000", "10,000", "30,000", "100,000")


# ---- Panel 1: every country-year ----
# The dots are the country-years (1 = conflict, 0 = no conflict).
# The red line is the share with a conflict and the orange line the share
# with a war. Both are percentages, so they go on a second y-axis.
# The left axis runs from -0.15 to 1.15 so the two sets of dots are not
# stuck on the edge; the right axis is the same scale multiplied by 100, so
# its breaks must also start a little below 0 and end a little above 100.
panel1 <- ggplot() +
  geom_point(data = country_years,
             aes(x = log_income, y = conflict),
# >> Almost 6,000 dots on two lines: they show where the incomes are, but you
# >> cannot see the share of conflict from them. The red line does that job.
# >> Two y-axes (0/1 on the left, % on the right) make the reader work hard;
# >> a single axis with the share in % and a rug of the dots below would be
# >> cleaner.
             size = 0.7, alpha = 0.08, colour = "steelblue") +
  scale_x_continuous(breaks = income_breaks, labels = income_labels) +
  scale_y_continuous(name = "Internal conflict that year",
                     breaks = c(0, 1), labels = c("No conflict", "Conflict"),
                     limits = c(-0.15, 1.15),
                     sec.axis = sec_axis(~ . * 100,
                                         breaks = c(0, 20, 40, 60, 80, 100),
                                         name = "Share with conflict (%)")) +
  labs(title = "Panel 1: Country-years (each dot is one country in one year)",
       subtitle = paste0("Correlation between conflict and log income: ",
                          round(cor(country_years$conflict, country_years$log_income), 3)),
       x = "GDP per capita PPP, previous year (log scale)",
       y = "Internal conflict that year") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, colour = "grey30"))

# Add the two curves, which show the share with a conflict on the right axis.
# na.rm = TRUE: the thin bands are set to NA on purpose, to break the line
# where there is not enough data, so they must not raise a warning.
panel1 <- panel1 +
  geom_line(data = curve_country_years,
            aes(x = log_income, y = pct_conflict / 100),
            colour = "darkred", linewidth = 1.1, na.rm = TRUE) +
  geom_line(data = curve_country_years,
            aes(x = log_income, y = pct_war / 100),
            colour = "darkorange", linewidth = 1.1, na.rm = TRUE) +
  annotate("text", x = log(560), y = 0.95,
           label = "Red = any internal conflict (right axis)", hjust = 0,
           size = 3.1, colour = "darkred") +
  annotate("text", x = log(560), y = 0.85,
           label = "Orange = high-intensity war (right axis)", hjust = 0,
           size = 3.1, colour = "darkorange")


# ---- Panel 2: countries by income group ----
# Two bars side by side for each income group: any internal conflict, and
# high-intensity war only.
# The bars are put side by side by giving the group a dodge position.
panel2_data <- country_bins %>%
  tidyr::pivot_longer(c(mean_pct_conflict, mean_pct_war),
                       names_to = "measure", values_to = "pct")

panel2_data <- panel2_data %>%
  mutate(measure = recode(measure,
                           mean_pct_conflict = "any internal conflict",
                           mean_pct_war = "war only"))

# The country count goes above the taller of the two bars in each group
country_bins <- country_bins %>%
  mutate(top_bar = pmax(mean_pct_conflict, mean_pct_war))

panel2 <- ggplot(panel2_data, aes(x = factor(income_group), y = pct, fill = measure)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.36) +
  geom_text(data = country_bins,
            aes(x = factor(income_group), y = top_bar,
                label = paste0(n_countries, " countries")),
            inherit.aes = FALSE, vjust = -1.2, size = 3.1) +
  scale_fill_manual(values = c("any internal conflict" = "darkred",
                               "war only" = "darkorange"),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  labs(title = "Panel 2: Countries by income group",
       subtitle = "1 = poorest fifth of countries, 5 = richest fifth",
       x = "Income group",
       y = "Average share of years with conflict (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, colour = "grey30"),
        legend.position = "top",
        legend.text = element_text(size = 8.5))


# ---- Panel 3: the conflict history, 1946-2025 ----
# The grey area marks the years with no GDP data, so the reader can see
# where the income comparison is and is not possible.
# The rectangle uses the same 0 to 50 range as the y-axis below, so it fills
# the panel from the bottom to the top.
panel3 <- ggplot() +
  annotate("rect", xmin = 1946, xmax = 1989.5, ymin = 0, ymax = 50,
           fill = "grey85") +
  annotate("text", x = 1967, y = 26,
           label = "No GDP per capita PPP data\nbefore 1990", size = 3.2,
           colour = "grey40") +
  geom_line(data = per_year, aes(x = year, y = n_conflict),
            colour = "darkred", linewidth = 0.9) +
  geom_line(data = per_year, aes(x = year, y = n_war),
            colour = "darkorange", linewidth = 0.9) +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  labs(title = "Panel 3: The conflict history, 1946-2025",
       subtitle = "Countries with an internal conflict in each year",
       x = "Year",
       y = "Number of countries") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, colour = "grey30")) +
  annotate("text", x = 2003, y = 46, label = "dark red = any internal conflict", hjust = 0,
           size = 3.1, colour = "darkred") +
  annotate("text", x = 2003, y = 41.5, label = "orange = high-intensity war", hjust = 0,
           size = 3.1, colour = "darkorange")


# ---- Panel 4: a note with the headline numbers ----
# How many countries in the richest fifth have no conflict at all
richest_no_conflict <- sum(countries$income_group == 5 & countries$pct_years_conflict == 0)

# The richest-fifth countries that do have conflict
# >> Good to look at the exceptions. Check them: the United States has
# >> conflict in 18 of 35 years because UCDP codes the conflict between the
# >> US government and al-Qaida (from 2001) as located in the United States
# >> (gwno_loc 2). The UK is Northern Ireland (to 1998), Spain is ETA (1991),
# >> Israel is the conflict with Palestinian groups. The exceptions are
# >> coding facts worth one sentence in the note.
richest_with_conflict <- countries %>%
  filter(income_group == 5, pct_years_conflict > 0) %>%
  arrange(desc(pct_years_conflict))

richest_with_conflict

# The lowest and the highest income in the data, read from the data
min_income <- min(country_years$gdp_per_capita_lag)
max_income <- max(country_years$gdp_per_capita_lag)

note_lines <- c(
  "Conflict data covers 1946-2025; GDP per capita PPP covers 1990-2025.",
  paste0("The income comparison uses ", nrow(country_years), " country-years in ",
         nrow(countries), " countries, with last year's income."),
  "",
  paste0("Poorest fifth of country-years: ", round(country_year_bins$pct_conflict[1], 1),
         "% with conflict; richest fifth: ",
         round(country_year_bins$pct_conflict[5], 1), "%."),
  paste0("Poorest fifth of countries: ", round(country_bins$mean_pct_conflict[1], 1),
         "% of years with conflict; richest fifth: ",
         round(country_bins$mean_pct_conflict[5], 1), "%."),
  paste0("Counting wars only, poorest fifth: ", round(country_bins$mean_pct_war[1], 1),
         "%; richest fifth: ", round(country_bins$mean_pct_war[5], 1), "%."),
  "",
# >> Typed by hand, and not quite what your own figure shows. In Panel 2,
# >> groups 2 and 3 are level (19.4% and 19.7%); in Panel 1 the red line goes
# >> up again between $3,000 and $5,000 and between $10,000 and $15,000.
# >> "Falls overall" is true; "in every view" is not. More importantly: how
# >> much of the fall could come from chance? That is the question for
# >> Session 4 (and with countries observed for 35 years, the country-years
# >> are not independent).
  "The conflict rate falls as income rises, in every view.",
  paste0(richest_no_conflict, " of the ", country_bins$n_countries[5],
         " countries in the richest fifth have no conflict, but ",
         paste(richest_with_conflict$country, collapse = ", "), " do."),
  "",
  paste0("A log scale is used because income runs from $", round(min_income),
         " to $", format(round(max_income), big.mark = ","), " per person.")
)

# Wrap each line so the long ones fit inside the panel
note_lines <- unlist(lapply(note_lines, function(x) {
  if (x == "") return("")
  strwrap(x, width = 52)
}))

note_text <- paste(note_lines, collapse = "\n")

panel4 <- ggplot() +
  annotate("text", x = 0, y = 1, label = note_text, hjust = 0, vjust = 1,
           size = 3.1, lineheight = 1.35, colour = "grey20") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))


# ---- Put the panels together ----
# The top row holds the two charts and the bottom row holds the history
# chart and the note, so the bottom row is given a little more height.
combined <- (panel1 | panel2) / (panel3 | panel4) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
# >> The title says 1946-2025, but the income comparison covers 1990-2025.
# >> Put the period of the test in the title, and the history in its panel.
    title = "GDP per capita PPP and internal conflict, 1946-2025",
# >> "so conflict has not already lowered it": too strong (see the comment
# >> on the lag). "so this year's conflict has not lowered it" is accurate.
    subtitle = "Income is measured in the year before the conflict, so conflict has not already lowered it",
    theme = theme(plot.title = element_text(face = "bold", size = 15),
                  plot.subtitle = element_text(size = 11, colour = "grey30"))
  )

ggsave("output/step6.png", combined, width = 15, height = 12, dpi = 120)
# >> FOR SESSION 4: Panel 1 is your test: 34% of the poorest fifth of
# >> country-years in conflict against 3% of the richest. Before class, write
# >> down how you would find out whether a gap that size could come from
# >> chance, given that the same countries appear 35 times each.

print("Figure saved to output/step6.png")

