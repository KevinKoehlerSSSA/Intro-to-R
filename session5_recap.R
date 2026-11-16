# =====================================================================
# Data Analysis in R -- Session 5 --- Recap
# Monday 16 November 2026
#
#   Part 1: Hypothesis testing, step by step
#           Did the income gap between conflict and peaceful country-years
#           grow after the end of the Cold War?
#           Data: data/lab2_full_table.csv (the Lab 2 table)
#
#
# Run it line by line (Ctrl/Cmd + Enter), not all at once.
# =====================================================================

library(dplyr)
library(ggplot2)


# #####################################################################
# PART 1: DID THE GAP GROW AFTER THE COLD WAR?
# #####################################################################

d <- read.csv("data/lab2_full_table.csv")


# ---- Set-up ----

d <- d %>% filter(year >= 1991)

d$period <- NA
d$period[d$year <= 2000] <- "early"   # 1991-2000
d$period[d$year >= 2016] <- "late"    # 2016-2025

d2 <- d %>% filter(!is.na(period))
nrow(d2)

sum(is.na(d2$gdppc))
sum(is.na(d2$gdppc) & d2$conflict == 1)

d2 %>% filter(is.na(gdppc), conflict == 1) %>% count(country)

d2 <- d2 %>% filter(!is.na(gdppc))
nrow(d2)

# ---- Step 2: what would the world look like? ----

set.seed(16)

shuffled_changes <- numeric(1000)

for (i in 1:1000) {
  # 1. shuffle the labels
  d_shuffled <- d2
  d_shuffled$period <- sample(d_shuffled$period)
  # 2. recompute both gaps and the change
  e <- d_shuffled %>% filter(period == "early")
  l <- d_shuffled %>% filter(period == "late")
  r_early <- median(e$gdppc[e$conflict == 0]) /
    median(e$gdppc[e$conflict == 1])
  r_late <- median(l$gdppc[l$conflict == 0]) /
    median(l$gdppc[l$conflict == 1])
  # 3. keep the change, then repeat
  shuffled_changes[i] <- r_late - r_early
}

sd(shuffled_changes)                          
hist(shuffled_changes, main = "Changes produced by shuffling country-years")

# ---- Step 3: what does the world look like? ----

d2 %>%
  group_by(period, conflict) %>%
  summarise(country_years = n(),
            median_income = median(gdppc),
            .groups = "drop")

early <- d2 %>% filter(period == "early")
late  <- d2 %>% filter(period == "late")

ratio_early <- median(early$gdppc[early$conflict == 0]) /
               median(early$gdppc[early$conflict == 1])
ratio_late  <- median(late$gdppc[late$conflict == 0]) /
               median(late$gdppc[late$conflict == 1])
ratio_early
ratio_late

change <- ratio_late - ratio_early
change

by_year <- d %>%
  filter(!is.na(gdppc)) %>%
  group_by(year) %>%
  summarise(ratio = median(gdppc[conflict == 0]) /
                    median(gdppc[conflict == 1]))

ggplot(by_year, aes(x = year, y = ratio)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "Peaceful median / conflict median")


# ---- Step 4: could the change be chance?  ----


hist(shuffled_changes, main = "Changes produced by shuffling country-years")
abline(v = change, col = "red")


# ---- Step 5: what varies? Shuffle whole countries ----

d_persist <- d %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(conflict_last_year = lag(conflict)) %>%
  ungroup() %>%
  filter(!is.na(conflict_last_year))

d_persist %>%
  group_by(conflict_last_year) %>%
  summarise(share_conflict = mean(conflict))

d2 %>%
  filter(conflict == 1) %>%
  group_by(period) %>%
  summarise(conflict_years = n(),
            countries = n_distinct(iso3c))


countries <- unique(d2$iso3c)

set.seed(17)
shuffled_changes_countries <- numeric(1000)

for (i in 1:1000) {
  coins <- data.frame(iso3c = countries,
                      swap = sample(c(TRUE, FALSE), length(countries), replace = TRUE))
  d_shuffled <- d2 %>% left_join(coins, by = "iso3c")
  d_shuffled$period[d_shuffled$swap] <- ifelse(d_shuffled$period[d_shuffled$swap] == "early",
                                               "late", "early")
  e <- d_shuffled %>% filter(period == "early")
  l <- d_shuffled %>% filter(period == "late")
  r_early <- median(e$gdppc[e$conflict == 0]) / median(e$gdppc[e$conflict == 1])
  r_late  <- median(l$gdppc[l$conflict == 0]) / median(l$gdppc[l$conflict == 1])
  shuffled_changes_countries[i] <- r_late - r_early
}

sd(shuffled_changes_countries)                          
mean(abs(shuffled_changes_countries) >= abs(change))    

hist(shuffled_changes_countries, main = "Changes produced by shuffling countries")
abline(v = change, col = "red")

# A 95% interval for the change: estimate +/- 1.96 standard errors
change - 1.96 * sd(shuffled_changes_countries)
change + 1.96 * sd(shuffled_changes_countries)


