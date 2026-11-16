# Session 5: who supported 25 July, and what does age tell us?
# (worksheet and fallback script)

# Data: data/tunisia_survey.csv. Codebook: data/codebook.md.
#
# Save this file in scripts/. Run Section 1 first, then the section you need:
# select its lines and press Ctrl+Enter (Mac: Cmd+Enter). To run everything
# at once, press Ctrl+Alt+R (Mac: Cmd+Option+R).
#
# The code is complete: it runs as it is. Your job is to read what it prints
# and to answer the questions. Type each answer after "# ANSWER:", on the
# same line, and keep the # at the start of the line.

# Name(s):


# ---- 1. Read the data and count the answers ----

library(tidyverse)

# Read the survey: one row is one respondent
survey <- read_csv("data/tunisia_survey.csv")
nrow(survey)

# july25: what statement best characterizes the July 25 events?
# The codebook explains the codes. How often was each one given?
count(survey, july25)

# The same as percentages. Denominator: all respondents
survey %>%
  count(july25) %>%
  mutate(percent = 100 * n / sum(n))

# QUESTION 1. Which code of july25 means "a necessary correction"? (a number)
# ANSWER:

# QUESTION 2. What percentage of all respondents gave that answer? (a number)
# ANSWER:


# ---- 2. Two groups, and two histograms of age ----

# Our decision: code 1 is "Support" and code 2 is "Oppose" (see the codebook).
# Codes 98 and 99 are answers, but not positions: they get no group.
survey <- survey %>%
  mutate(group = case_when(
    july25 == 1 ~ "Support",
    july25 == 2 ~ "Oppose"
  ))

# How many respondents are in each group, and how many have no group?
count(survey, group)

# Keep the respondents who took a positionxx
position <- survey %>% filter(!is.na(group))
nrow(position)

# How many of them have no age, and in which group?
position %>%
  group_by(group) %>%
  summarise(no_age = sum(is.na(age)))

# Keep the respondents who also gave their age: the people we compare
compare <- position %>% filter(!is.na(age))
nrow(compare)
count(compare, group)

# QUESTION 3. How many respondents took a position? (a number)
# ANSWER:

# QUESTION 4. How many of them also gave their age? (a number)
# ANSWER:

# QUESTION 5. In which group are the missing ages? (Support or Oppose)
# ANSWER:

# The ages of each group, as two plain lists of numbers
supporters_age <- compare$age[compare$group == "Support"]
opponents_age <- compare$age[compare$group == "Oppose"]
length(supporters_age)
length(opponents_age)

# Histogram 1: everyone we compare.
# breaks: the bars start at 15, 20, 25 and so on, up to 85.
# right = FALSE: a 20-year-old goes into the bar 20-24, not 15-20.
hist(compare$age, breaks = seq(15, 85, 5), right = FALSE, main = "Age of the respondents who took a position", xlab = "Age in years", ylab = "Number of people")

# QUESTION 6. How many people are in the tallest bar? (a number; the table
# further down gives the exact counts)
# ANSWER:

# Histogram 2: one histogram for each group, with the same bars.
# freq = FALSE draws density instead of numbers of people: the groups differ
# in size, so numbers of people could not be compared.
# ylim gives both plots the same vertical scale.
hist(supporters_age, breaks = seq(15, 85, 5), right = FALSE, freq = FALSE, ylim = c(0, 0.035), main = "Age of supporters", xlab = "Age in years")
hist(opponents_age, breaks = seq(15, 85, 5), right = FALSE, freq = FALSE, ylim = c(0, 0.035), main = "Age of opponents", xlab = "Age in years")

# The numbers behind the bars: people of each group in each five-year bar
compare <- compare %>% mutate(age_bar = cut(age, breaks = seq(15, 85, 5), right = FALSE))
table(compare$age_bar, compare$group)

# Under 30: how many people, and what share of each group?
# Denominator: everyone in the group
compare %>%
  group_by(group) %>%
  summarise(people = n(), under_30 = sum(age < 30), share_under_30 = mean(age < 30))

# The other way round: of the people under 30, what share gave each answer?
# Denominator: people under 30 who took a position
under_30 <- compare %>% filter(age < 30)
nrow(under_30)
under_30 %>%
  count(group) %>%
  mutate(share = n / sum(n))

# QUESTION 7. What percentage of the opponents are under 30? (a number)
# ANSWER:

# QUESTION 8. What percentage of the people under 30 are supporters? (a number)
# ANSWER:

# QUESTION 9. Which of the two numbers answers "did young people oppose the
# July events?", and what is the answer? (one sentence)
# ANSWER:

# The whole table, in ten-year age bands: people of each group in each band
compare <- compare %>% mutate(band = cut(age, breaks = c(18, 30, 40, 50, 60, 70, Inf), right = FALSE))
people <- table(compare$band, compare$group)
people

# Of each group, the share in each age band: every column adds up to 100
round(100 * prop.table(people, margin = 2))

# Of each age band, the share giving each answer: every row adds up to 100
round(100 * prop.table(people, margin = 1))


# ---- 3. Mean ages and one test ----

# Number of people, mean age and median age in each group
compare %>%
  group_by(group) %>%
  summarise(people = n(), mean_age = mean(age), median_age = median(age))

# The middle half of each group: a quarter of the group is younger than the
# first number, and a quarter is older than the second
quantile(supporters_age, c(0.25, 0.75))
quantile(opponents_age, c(0.25, 0.75))

# The gap in mean age: supporters minus opponents
mean(supporters_age) - mean(opponents_age)

# Two-sided Welch t-test: the default of t.test().
# The gap it reports is the first group minus the second.
welch <- t.test(supporters_age, opponents_age)
welch

# The standard error of the gap
welch$stderr

# How many standard errors is the gap away from zero?
(mean(supporters_age) - mean(opponents_age)) / welch$stderr

# QUESTION 10. The gap in mean age, supporters minus opponents, in years? (a number)
# ANSWER:

# QUESTION 11. The t-statistic of the Welch test? (a number)
# ANSWER:

# QUESTION 12. The lower and the upper end of the 95% confidence interval? (two numbers)
# ANSWER:

# QUESTION 13. Your result in one sentence, with the number, its unit and the
# order of subtraction.
# ANSWER:


# ---- 4. More pictures of the same people ----

# Histogram 3: one bar for each year of age. Which ages stand out?
hist(compare$age, breaks = seq(15, 85, 1), right = FALSE, main = "Age, one bar per year", xlab = "Age in years", ylab = "Number of people")

# How many people give an age that ends in 0 or 5? (%% gives the remainder of a division)
compare %>%
  summarise(people = n(), round_age = sum(age %% 5 == 0), share = mean(age %% 5 == 0))

# QUESTION 14. How many people give an age that ends in 0 or 5? (a number)
# ANSWER:

# QUESTION 15. Why are the bars at 40 and 60 so tall? (one sentence)
# ANSWER:

# Scatterplot 1: one dot for each age.
# For each age: the number of people, and the share of them who are supporters
by_age <- compare %>%
  group_by(age) %>%
  summarise(people = n(), share_supporters = mean(group == "Support"))
by_age

ggplot(by_age, aes(x = age, y = share_supporters, size = people)) +
  geom_point() +
  ylim(0, 1) +
  labs(title = "Share of supporters at each age", x = "Age in years", y = "Share of supporters")

# Is the share of supporters below one half at any age?
by_age %>% filter(share_supporters < 0.5)

# QUESTION 16. At how many ages is the share of supporters below one half? (a number)
# ANSWER:

# Women and men: the codebook says female = 1 for women and 0 for men
compare <- compare %>% mutate(gender = if_else(female == 1, "Women", "Men"))
count(compare, gender, group)

# Histogram 4: the two groups again, among women only and among men only.
# The four plots have the same bars and the same vertical scale.
women <- compare %>% filter(gender == "Women")
men <- compare %>% filter(gender == "Men")
hist(women$age[women$group == "Support"], breaks = seq(15, 85, 5), right = FALSE, freq = FALSE, ylim = c(0, 0.04), main = "Women: supporters", xlab = "Age in years")
hist(women$age[women$group == "Oppose"], breaks = seq(15, 85, 5), right = FALSE, freq = FALSE, ylim = c(0, 0.04), main = "Women: opponents", xlab = "Age in years")
hist(men$age[men$group == "Support"], breaks = seq(15, 85, 5), right = FALSE, freq = FALSE, ylim = c(0, 0.04), main = "Men: supporters", xlab = "Age in years")
hist(men$age[men$group == "Oppose"], breaks = seq(15, 85, 5), right = FALSE, freq = FALSE, ylim = c(0, 0.04), main = "Men: opponents", xlab = "Age in years")

# Mean age in the four panels, and the share of supporters among women and men
compare %>%
  group_by(gender, group) %>%
  summarise(people = n(), mean_age = mean(age))
compare %>%
  group_by(gender) %>%
  summarise(people = n(), share_supporters = mean(group == "Support"))

# QUESTION 17. The gap in mean age, supporters minus opponents, among women
# and among men? (two numbers: work them out from the table of mean ages)
# ANSWER:

# Scatterplot 2: one dot for each governorate.
# For each governorate: respondents, and the mean age of its supporters and opponents
by_governorate <- compare %>%
  group_by(region) %>%
  summarise(people = n(),
            supporters = sum(group == "Support"),
            opponents = sum(group == "Oppose"),
            mean_supporters = mean(age[group == "Support"]),
            mean_opponents = mean(age[group == "Oppose"]))
by_governorate <- by_governorate %>% mutate(gap = mean_supporters - mean_opponents)
by_governorate %>% arrange(people) %>% print(n = 24)

# One governorate has no opponents among its respondents, so it has no gap
by_governorate %>% filter(opponents == 0)
with_gap <- by_governorate %>% filter(opponents > 0)
nrow(with_gap)

ggplot(with_gap, aes(x = people, y = gap)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "The gap in mean age in each governorate", x = "Number of respondents in the governorate", y = "Gap in mean age, supporters minus opponents (years)")

# In how many governorates is the gap positive, and in how many negative?
sum(with_gap$gap > 0)
sum(with_gap$gap < 0)

# QUESTION 18. In how many governorates are the opponents the older group? (a number)
# ANSWER:

# QUESTION 19. Why do the gaps differ so much between governorates?
# (one sentence)
# ANSWER:


# ---- 5. 2,000 shuffles: how big a gap can chance alone produce? ----

# The real gap
gap <- mean(supporters_age) - mean(opponents_age)
gap

# Fix the random numbers so every run gives the same results
set.seed(2022)

# One shuffle: everybody keeps their age, the answers are dealt out at random
shuffled_group <- sample(compare$group)
mean(compare$age[shuffled_group == "Support"]) - mean(compare$age[shuffled_group == "Oppose"])

# 2,000 shuffles: keep the gap of each one
set.seed(2022)
shuffled_gaps <- numeric(2000)
for (i in 1:2000) {
  shuffled_group <- sample(compare$group)
  shuffled_gaps[i] <- mean(compare$age[shuffled_group == "Support"]) - mean(compare$age[shuffled_group == "Oppose"])
}

# Histogram 5: the 2,000 shuffled gaps, with a red line at the real gap
hist(shuffled_gaps, breaks = seq(-5, 5, 0.5), right = FALSE, xlim = c(-5, 8), main = "2,000 shuffled gaps", xlab = "Mean age of supporters minus mean age of opponents (years)")
abline(v = gap, col = "red")

# The spread of the shuffled gaps, next to the standard error from the formula
sd(shuffled_gaps)
welch$stderr

# How many shuffles are at least as far from zero as the real gap?
sum(abs(shuffled_gaps) >= gap)

# QUESTION 20. The standard deviation of the 2,000 shuffled gaps, in years? (a number)
# ANSWER:

# QUESTION 21. How many shuffles are at least as far from zero as the real gap? (a number)
# ANSWER:


# ---- 6. A second t-test: share of supporters, under 40 and 40 or older ----

# A yes/no measure: 1 for supporters, 0 for opponents. Its mean is a share
compare <- compare %>% mutate(supporter = if_else(group == "Support", 1, 0))
mean(compare$supporter)

# Cut age in two at 40
older_support <- compare$supporter[compare$age >= 40]
younger_support <- compare$supporter[compare$age < 40]
length(younger_support)
length(older_support)

# The share of supporters in each half
mean(younger_support)
mean(older_support)

# Two-sided Welch t-test. The gap is the older half minus the younger half
shares <- t.test(older_support, younger_support)
shares

# The standard error, and the number of standard errors from zero
shares$stderr
(mean(older_support) - mean(younger_support)) / shares$stderr

# QUESTION 22. The percentage of supporters among people under 40, and among
# people aged 40 or older? (two numbers)
# ANSWER:

# QUESTION 23. The t-statistic of this second test? (a number)
# ANSWER:

# QUESTION 24. The t-statistic was larger for the mean ages (question 11).
# What did cutting age in two throw away? (one sentence)
# ANSWER:

# QUESTION 25. One political claim that this survey cannot establish.
# (one sentence)
# ANSWER:
