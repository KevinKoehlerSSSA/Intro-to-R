# =============================================================================
# ANNOTATED COPY of 15_bubble_figure.R (bubble chart, sent 7 November 2026)
#
# Lines starting with  # >>  are my comments. Everything else is your code,
# unchanged. I could NOT run this one: it reads output/bubble_panel.csv,
# built by your script 14 from World Bank, UCDP and UN files that you did not
# send. The comments are from reading the code.
#
# THE SHORT VERSION
#   + A finished, publication-style figure: sources, definitions, dropped
#     rows and limitations are all in the caption; log scales; population
#     as bubble area; Pearson AND Spearman.
#   + The caption says the correlation does not establish causation.
#   - It changes the question. The claim is about WHERE conflict happens
#     (incidence). Deaths per 100,000 mix incidence with intensity and are
#     dominated by a few catastrophic country-years.
#   - The death variable is probably not "battle-related deaths": Rwanda
#     1994 at 11,372 per 100,000 is the genocide (one-sided violence).
#   - The p-value treats every country-year as independent.
#   - Several numbers and all label positions are typed by hand.
# =============================================================================

# Draw output/figure_gdp_conflict_bubble.png
# A single static bubble chart: GDP per capita against battle-related deaths,
# for every country and every year from 1990 to 2025.
#
# The script reads output/bubble_panel.csv, which script 14 built from the
# World Bank, UCDP and UN population files.

library(tidyverse)
library(scales)
library(ggrepel)

# ---- Read the panel ----
# >> Send script 14 and the input files too: without them nobody (including
# >> me) can reproduce the figure.
panel <- read_csv("output/bubble_panel.csv", show_col_types = FALSE)

nrow(panel)

# ---- The colours of the six regions ----
# Muted but distinct, so the regions read clearly without shouting.
region_colors <- c(
  "Africa" = "#C1654A",
  "Middle East" = "#D9A441",
  "Asia" = "#5B8C85",
  "Europe" = "#6D7FA3",
  "North America" = "#8E7BA8",
  "Latin America" = "#A8765E"
)

region_order <- c("Africa", "Middle East", "Asia", "Europe",
                  "North America", "Latin America")

# ---- Prepare the data for the plot ----
plot_data <- panel %>%
  filter(!is.na(gdp_per_capita), !is.na(deaths_per_100k), !is.na(region)) %>%
  mutate(region = factor(region, levels = region_order))

nrow(plot_data)

# The x and y variables are both logged, so plot the logged versions.
plot_data <- plot_data %>%
  mutate(x = log10(gdp_per_capita),
         y = log10(deaths_per_100k + 1))
# >> log10(x + 1): the +1 is needed for the zeros, but it is arbitrary (with
# >> +0.1 or +10 the picture and the correlation change). Most country-years
# >> have zero deaths, so a large block of points sits on the bottom line and
# >> drives the correlation. One alternative: first the share of country-
# >> years with ANY deaths by income (incidence), then the death rate only
# >> among the country-years with conflict (intensity). Two questions, two
# >> answers.

# ---- The years, used to shade each point from pale to dark ----
# The colour of a point shows how far through the period it falls.
plot_data <- plot_data %>%
  mutate(year_scaled = (year - 1990) / (2025 - 1990))

# ---- The regression line ----
# The line is the ordinary least squares fit of the logged death rate on the
# logged GDP, over the same rows the chart shows.
# >> A straight line through a cloud with a wall of zeros at the bottom.
# >> R-squared will be small; that is fine, but the slope depends on the +1
# >> above. The binned share (as in your step 6 figure) shows the relationship
# >> without assuming a straight line.
model <- lm(y ~ x, data = plot_data)

coef(model)

summary(model)$r.squared

# The correlation on the chart, between the two logged variables
# >> Note: the line uses log10, the correlation uses natural log. For a
# >> correlation that makes no difference (it does not depend on the base),
# >> but use one throughout so readers are not puzzled.
cor_test <- cor.test(log(plot_data$gdp_per_capita),
                     log(plot_data$deaths_per_100k + 1))

cor_value <- cor_test$estimate

cor_value

# The rank correlation, which does not depend on the scale
cor_rank <- cor(log(plot_data$gdp_per_capita),
                plot_data$deaths_per_100k, method = "spearman")

cor_rank

# The two ends of the regression line, so the line can be drawn on the plot
line_data <- data.frame(
  x = c(min(plot_data$x), max(plot_data$x))
)
line_data$y <- coef(model)[1] + coef(model)[2] * line_data$x

line_data

# ---- The countries to label ----
# Rwanda 1994 is the largest outlier of all. The others are the highest death
# rates, and the countries with the biggest change in income.
labels <- plot_data %>%
  filter(
    (country_deaths == "Bosnia and Herzegovina" & year == 1992) |
    (country_deaths == "Syria" & year == 2014) |
    (country_deaths == "Palestine" & year == 2023) |
    (country_deaths == "Ukraine" & year == 2022) |
    (country_deaths == "Congo" & year == 1997) |
    (country_deaths == "Burundi" & year == 1996) |
    (country_deaths == "China" & year == 2025) |
    (country_deaths == "Afghanistan" & year == 2001) |
    (country_deaths == "Haiti" & year == 2024)
  ) %>%
  mutate(label = paste0(country_deaths, " ", year))

labels %>% select(country_deaths, year, gdp_per_capita, deaths_per_100k)

# Where each label should sit, in the same log units as the axes. Written out
# one country at a time so the placement can be checked by eye.
# >> Every label position is typed by hand and checked by eye. That works
# >> for one figure, but it breaks as soon as the data change. ggrepel (you
# >> load it but do not use it) places labels automatically:
# >> geom_text_repel(). Choose WHICH points to label by a rule (e.g. the ten
# >> highest death rates), not by hand: hand-picked labels tell the reader
# >> what to see.
label_positions <- tribble(
  ~country_deaths, ~year, ~label_x, ~label_y, ~hjust,
  "Bosnia and Herzegovina", 1992,  log10(2900), log10(750),  0,
  "Syria",                 2014,  log10(3300), log10(200),  1,
  "Palestine",             2023,  log10(6300), log10(560),  0,
  "Ukraine",               2022,  log10(19000), log10(310), 0,
  "Congo",                 1997,  log10(9500), log10(380),  0,
  "Burundi",               1996,  log10(850),  log10(300), 1,
  "China",                 2025,  log10(26000), log10(2.4), 1,
  "Afghanistan",           2001,  log10(2600), log10(33),  0,
  "Haiti",                 2024,  log10(3100), log10(14),  0
)

# The labels are put on their own layer, so the point they belong to is found
# by matching the country and the year.
label_layer <- labels %>%
  select(country_deaths, year, label, x, y) %>%
  left_join(label_positions, by = c("country_deaths", "year"))

label_layer %>% select(country_deaths, x, y, label_x, label_y)

# The p-value of the correlation, worked out by hand so it can be written
# with the right number of zeroes.
t_value <- cor_test$statistic
df_value <- cor_test$parameter
# >> cor_test$p.value already gives this; no need to compute it by hand.
# >> The real problem is elsewhere: cor.test() assumes several thousand
# >> INDEPENDENT country-years. The same country appears up to 36 times, and
# >> conflict persists from year to year. Session 4 shows that with this
# >> dependence the honest uncertainty is several times larger. "p < 0.001"
# >> is not wrong here, but it is not the evidence it looks like.
p_value <- 2 * pt(-abs(t_value), df_value)

p_value

# The p-value as text, with the smallest number that still means something
p_text <- if (p_value < 0.001) "p < 0.001" else paste0("p = ", round(p_value, 3))

p_text

# Rwanda is the one point that sits far away from everything else, so it gets
# a ring around it to make it stand out.
rwanda_point <- plot_data %>%
  filter(country_deaths == "Rwanda", year == 1994)

rwanda_point %>% select(country_deaths, year, gdp_per_capita, deaths_per_100k)

# ---- Draw the chart ----
figure <- ggplot(plot_data, aes(x = x, y = y, colour = region)) +
# >> Five things are encoded at once: income (x), deaths (y), region
# >> (colour), population (size) and year (transparency). Transparency for
# >> time is very hard to read, especially where bubbles overlap. Ask which of
# >> these the claim needs. For the claim in Lab 2: income and conflict.
  geom_point(aes(size = population, alpha = year_scaled), shape = 16) +
  scale_colour_manual(values = region_colors, name = "Region") +
  scale_size_area(max_size = 13, name = "Population",
                   breaks = c(1e7, 1e8, 5e8, 1e9),
                   labels = c("10 million", "100 million", "500 million", "1 billion")) +
  scale_alpha_continuous(range = c(0.12, 0.85), name = "Year",
                         breaks = c(0, 1),
                         labels = c("1990", "2025")) +
  scale_x_continuous(
    breaks = log10(c(500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)),
    labels = c("500", "1,000", "2,000", "5,000", "10,000", "20,000", "50,000", "100,000"),
    limits = c(log10(420), log10(125000)),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    breaks = log10(c(0, 1, 10, 100, 1000, 10000)),
    labels = c("0", "1", "10", "100", "1,000", "10,000+"),
    limits = c(-0.15, 4.35),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  geom_line(data = line_data, aes(x = x, y = y), inherit.aes = FALSE,
            colour = "#FFFFFF", linewidth = 2.1) +
  geom_line(data = line_data, aes(x = x, y = y), inherit.aes = FALSE,
            colour = "#2F2F2F", linewidth = 0.9, linetype = "22") +
  geom_segment(data = label_layer,
               aes(x = x, y = y, xend = label_x, yend = label_y),
               inherit.aes = FALSE, colour = "#8A8A8A", linewidth = 0.3) +
  geom_text(data = label_layer,
            aes(x = label_x, y = label_y, label = label, hjust = hjust),
            inherit.aes = FALSE, size = 3.4, family = "Helvetica Neue",
            colour = "#1A1A1A", vjust = 0.5) +
  geom_point(data = rwanda_point, aes(x = x, y = y),
             inherit.aes = FALSE, shape = 21, size = 4.2,
             fill = "#C1654A", colour = "#FFFFFF", stroke = 0.9) +
  annotate("label", x = log10(780), y = log10(6000),
# >> 11,372 per 100,000 is more than 11% of the population. That is the
# >> genocide: UCDP counts it as ONE-SIDED violence, not battle-related deaths.
# >> So your death variable (probably "Deaths in armed conflicts", which adds
# >> state-based, non-state and one-sided violence) is not what the axis title
# >> and caption call it. Check the codebook and rename the variable. The
# >> number is also typed by hand: take it from rwanda_point.
           label = "Rwanda 1994 — genocide and civil war\n11,372 battle-related deaths per 100,000",
           size = 2.9, family = "Helvetica Neue", colour = "#1A1A1A",
           fill = "#FFFFFF", label.size = 0.3, label.padding = unit(0.22, "cm"),
           label.r = unit(0.08, "cm"), hjust = 0, vjust = 0.5) +
  annotate("segment", x = log10(597), y = log10(11372 + 1),
           xend = log10(700), yend = log10(6000),
           colour = "#8A8A8A", linewidth = 0.3) +
  annotate("text", x = log10(32000), y = log10(120),
# >> This is the finding, but it is about INTENSITY. The Lab 2 claim was that
# >> conflict is concentrated in poor countries. Say clearly which of the two
# >> the figure tests.
           label = "High conflict intensity is concentrated\nat low income; rich states record almost none",
           size = 2.8, family = "Helvetica Neue", colour = "#5A5A5A",
           hjust = 0, lineheight = 1.25) +
  labs(
    title = "Prosperity and Armed Conflict Worldwide, 1990–2025",
    subtitle = paste0(
      "Battle-related deaths per 100,000 inhabitants against GDP per capita (PPP, constant international $)\n",
      "Each bubble is one country in one year; bubble area is population.  ",
      "Pearson r = ", round(cor_value, 3), " on the two logged variables (",
      p_text, ");  Spearman ρ = ", round(cor_rank, 3), "."
    ),
    x = "GDP per capita (PPP, constant 2021 international $)  —  log scale",
    y = "Battle-related deaths per 100,000 per year  —  log scale",
    caption = paste0(
      "Sources: World Bank, GDP per capita PPP, constant 2021 international $ (NY.GDP.PCAP.PP.KD);\n",
      "UCDP/PRIO, “Deaths in armed conflicts” (best estimate); UN population (actual to 2023, projected 2024–25).\n",
# >> 7,092 is typed by hand while nrow(plot_data) next to it is computed.
# >> Compute both: nrow(panel).
      "Notes: 7,092 country-years of conflict records, of which ", format(nrow(plot_data), big.mark = ","),
      " have GDP, population and region data; the rest are dropped, never estimated. ",
      "Country-years with no battle-related deaths are plotted at 0.\n",
      "Both axes are logarithmic; the dashed line is an OLS fit of the logged rate on logged GDP (R² = ",
      round(summary(model)$r.squared, 3), ").\n",
      "Limitations: the correlation is cross-sectional and does not establish that low income causes conflict. ",
      "UCDP counts battle-related deaths only, not indirect deaths from disease or famine.\n",
      "Nine countries have no World Bank GDP series for any year and are omitted. ",
      "2024–25 population denominators are UN projections."
    )
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 4, alpha = 0.9), order = 1),
    size = guide_legend(order = 2),
    alpha = guide_legend(order = 3, override.aes = list(size = 3))
  ) +
# >> "Helvetica Neue" exists on a Mac, not on most Windows or Linux
# >> computers: there R falls back to another font (with warnings), and the
# >> hand-placed labels may no longer fit. Use a font everyone has, or none.
  theme_minimal(base_family = "Helvetica Neue", base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background = element_rect(fill = "#FCFCFC", colour = NA),
    panel.grid.major = element_line(colour = "#E8E8E8", linewidth = 0.3),
    panel.grid.minor = element_line(colour = "#F2F2F2", linewidth = 0.2),
    panel.border = element_rect(colour = "#D5D5D5", fill = NA, linewidth = 0.4),
    plot.title = element_text(face = "bold", size = 19, colour = "#1A1A1A",
                              margin = margin(b = 8)),
    plot.subtitle = element_text(size = 10.5, colour = "#4A4A4A", lineheight = 1.35,
                                  margin = margin(b = 14)),
    plot.caption = element_text(size = 8.2, colour = "#6E6E6E", hjust = 0,
                                lineheight = 1.35, margin = margin(t = 14)),
    axis.title = element_text(size = 11.5, colour = "#2F2F2F", margin = margin(t = 8)),
    axis.text = element_text(size = 10, colour = "#4A4A4A"),
    axis.ticks = element_line(colour = "#B5B5B5", linewidth = 0.3),
    legend.title = element_text(size = 10, face = "bold", colour = "#2F2F2F"),
    legend.text = element_text(size = 9.5, colour = "#4A4A4A"),
    legend.position = "right",
    legend.box = "vertical",
    legend.key.size = unit(0.42, "cm"),
    legend.margin = margin(l = 14),
    plot.margin = margin(18, 22, 12, 16)
  )

# ---- Save the figure ----
ggsave("output/figure_gdp_conflict_bubble.png", figure,
# >> FOR SESSION 4: your caption already says the correlation is not causal.
# >> The next question is whether it is distinguishable from chance once you
# >> take into account that the country-years are not independent. Before
# >> class, think about what "shuffling" would mean for your figure.
       width = 16, height = 10, dpi = 300, bg = "white")

cat("Saved output/figure_gdp_conflict_bubble.png\n")
