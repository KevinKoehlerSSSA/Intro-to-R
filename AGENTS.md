# Instructions for Coding Agent: Data Analysis in R

This is a course project for a class on data analysis in R. The student is learning to direct, read and check your code, and most have no prior programming experience. Help them understand, not just finish.

## How to work
- Use R. Base R is fine for simple steps, as in Example 1 at the end of this file; use the tidyverse for data manipulation (filtering, recoding, summarising tables).
- Put scripts in `scripts/`, outputs (figures, tables) in `output/`. Never modify files in `data/`.
- The data are `data/tunisia_survey.csv`. The codebook is `data/codebook.md` (the same content as `data/Codebook-TUN-Survey.pdf`).
- Comment every step in plain language: what it does and why.
- Before running a script, say in one or two sentences what it will do.

## Keep the code simple
The students are first-time R users. They must be able to read every script top to bottom and explain each line. Clarity and simplicity always come before elegance, brevity or generality.
- Do not write your own functions (`function()`) unless the student explicitly asks for one. If the same steps are needed twice (e.g. for two variables), write the block out again and change only what differs.
- Prefer longer, simpler code to shorter, clever code. One operation per line; save intermediate results under clear names instead of nesting function calls inside each other. Keep pipes (`%>%`) to a few steps.
- Avoid advanced constructs: `lapply()`/`sapply()`/`purrr::map()`, `do.call()`, `Reduce()`, `repeat`/`while`, nested loops, lists of lists, list-columns, `tryCatch()`, `sink()`, tidy evaluation.
- Assume the script is run from the project folder and use simple relative paths (e.g. `"data/tunisia_survey.csv"`). Do not write code to locate folders, check that files exist or handle errors unless asked.
- Do only what the prompt asks. No extra logs, options, formatting or analyses; suggest extras in the chat instead.
- Keep output and plots plain: `print()` or `round()` rather than elaborate `sprintf()` formatting; one plot type with a title and axis labels, nothing more unless asked.
- Keep comments short: one or two plain-language lines per step, not long blocks of text.
- Handle one variable at a time. Never loop over variables or over settings such as sample sizes, and never store results in lists. Write out the steps for each variable and each setting separately, even if this repeats code. Plain variables with clear names (`means_10`, `means_40`) are better than any container. Do not reshape data (`pivot_longer()`, `pivot_wider()`) just to process several variables in one go.
- See the two example scripts at the end of this file: write code like the first, never like the second.
- Use one pipe (`%>%`) and one way of doing each thing throughout a script, e.g. one way of attaching value labels.
- Explanations and teaching points belong in the chat, not in the script.

## Report what happens to the data
- After every step that can change the number of rows (reading, filtering, joining, dropping missing values), print the row count.
- Never use `na.rm = TRUE`, `na.omit()`, `drop_na()` or similar without saying how many cases are dropped and from which variables.
- If a variable has codes that look like missing values (e.g. 98, 99), flag them; do not decide on your own how to treat them.
- Once the student has decided how to treat missing-value codes, recode them to `NA` once, near the top of the script, one visible line per variable, and print how many were recoded. Use the cleaned variables from then on.
- Never type a number taken from the data into a comment or printed message; compute it and print it.
- Do not say why values are missing (not asked, skipped, refused) unless the codebook says so. If you assume a skip pattern, show it with a cross-tabulation, e.g. `table(pres2019_1_voted, is.na(pres2019_1), useNA = "ifany")`.

## Statistics
- Report means and standard deviations only where an average is meaningful. For categories, report counts and percentages; for yes/no variables, report the share.
- For every percentage, state the denominator and whether missing values are included in it.

## When unsure
- Ask rather than guess about analytical choices. Offer the options and their consequences; let the student decide.
- If you are not sure what a function does, say so and suggest checking its help page (`?function_name`).

## Examples: the style we want
Both scripts below answer the same prompt, a lab from the PhD Quantitative Methods course (simulating the standard error of a mean with a different dataset). The dataset and statistics differ from this course; what matters is the style. Write code in the style of Example 1. Do not write code in the style of Example 2.

### Example 1 (write code like this)
Each variable has its own block; each step is one line whose result R prints; results are stored in plain variables with clear names; comments are short.

```r
# Lab A: standard errors two ways
# Treat the observed states as the whole population, draw many random
# samples, and compare the spread of the sample averages with sd / sqrt(n).

library(haven)

# Fix the random numbers so every run gives the same results
set.seed(42)

# Read the data
gh <- read_dta("Data/data_2011-07-26.dta")
nrow(gh)


# ---- Part 1: unvoting ----

# How many states have a value, and how many are missing?
sum(!is.na(gh$unvoting))
sum(is.na(gh$unvoting))

# Which states are missing? (ccode = Correlates of War country code)
gh$ccode[is.na(gh$unvoting)]

# The population: every state with a value for unvoting
unvoting <- gh$unvoting[!is.na(gh$unvoting)]

# The true population mean and standard deviation
mean(unvoting)
sd(unvoting)

# 2,000 samples of 10 states; keep the average of each sample.
# replace = FALSE: a state cannot appear twice in the same sample.
means_10 <- replicate(2000, mean(sample(unvoting, size = 10, replace = FALSE)))

# The same with samples of 40 states
means_40 <- replicate(2000, mean(sample(unvoting, size = 40, replace = FALSE)))

# Are the averages centred on the population mean?
mean(means_10)
mean(means_40)

# Plot the averages, both on the same x-axis so the widths can be compared
hist(means_10, xlim = range(means_10), main = "unvoting: averages of 10 states", xlab = "Sample average")
hist(means_40, xlim = range(means_10), main = "unvoting: averages of 40 states", xlab = "Sample average")

# Simulated spread: the standard deviation of the 2,000 averages
sd(means_10)
sd(means_40)

# Formula: population standard deviation divided by the square root of n
sd(unvoting) / sqrt(10)
sd(unvoting) / sqrt(40)


# ---- Part 2: article98 (1 = signed, 0 = did not sign) ----

# How many states have a value, and how many are missing?
sum(!is.na(gh$article98))
sum(is.na(gh$article98))

# Which states are missing?
gh$ccode[is.na(gh$article98)]

# The population: every state with a value for article98
article98 <- gh$article98[!is.na(gh$article98)]

# The true share of states that signed, and the standard deviation
mean(article98)
sd(article98)

# 2,000 samples of 10 states, and 2,000 samples of 40 states.
# The average of 0/1 values is the share of the sample that signed.
shares_10 <- replicate(2000, mean(sample(article98, size = 10, replace = FALSE)))
shares_40 <- replicate(2000, mean(sample(article98, size = 40, replace = FALSE)))

# Are the shares centred on the population share?
mean(shares_10)
mean(shares_40)

# Plot the shares, both on the same x-axis
hist(shares_10, xlim = range(shares_10), main = "article98: shares in samples of 10", xlab = "Share that signed")
hist(shares_40, xlim = range(shares_10), main = "article98: shares in samples of 40", xlab = "Share that signed")

# Simulated spread
sd(shares_10)
sd(shares_40)

# Formula
sd(article98) / sqrt(10)
sd(article98) / sqrt(40)
```

### Example 2 (do not write code like this)
Loops over variables and sample sizes, results stored in lists of lists, `sprintf()` formatting, a long design-note header, folder checks, and interpretation printed by the script instead of explained in the chat. It runs and is correct, but a first-time R user cannot follow it.

```r
###############################################################################
## lab_se.R
##
## LAB: The standard error of the mean, by simulation.
##
## Question
##   Treat the observed values of `unvoting` in data_2011-07-26.dta as a
##   population. Draw 2,000 random samples of 10 states, and 2,000 of 40.
##   Plot the distribution of the sample averages and report how spread out
##   they are (the standard deviation of the 2,000 averages). Compare that
##   simulated spread with the textbook formula  SD / sqrt(n).
##   Then repeat the whole exercise for `article98`, the share of countries
##   signing an Article 98 agreement (a 0/1 indicator, so its mean is a
##   share and its average is an average share).
##
## Inputs  : Data/data_2011-07-26.dta
## Outputs : output/lab_se_unvoting_means.png
##           output/lab_se_article98_means.png
##
## DESIGN NOTES
##   * POPULATION. We use every row of the file that has a non-missing
##     value for the variable in question (N = 190 for unvoting, N = 216 for
##     article98). We deliberately ignore the `keep` variable: `keep` is 1
##     for only the 58 countries in the Goldsmith & Horiuchi regressions
##     and NA for the other 167 rows. It is a regression-sample flag, not
##     the sampling frame for this lab.
##     NB: `keep` is NA -- not 0 -- for 167 rows, so `d$keep == 1` yields
##     NA for those rows. Always use which() or na.rm = TRUE.
##   * SAMPLING WITHOUT REPLACEMENT. Each replicate draws n DISTINCT
##     countries, which is the standard simple random sample without
##     replacement (SRSWOR) from a finite population. Because the
##     population is finite (190 or 216), the true SD of the sample mean
##     is slightly SMALLER than SD / sqrt(n); the finite-population
##     correction (FPC) sqrt((N - n) / N) accounts for this. We report
##     both the naive formula and the corrected one.
##   * The 2,000 replicates are independent of each other.
##   * Seed fixed so the numbers are identical on every run.
###############################################################################

## ---- 0. Packages ---------------------------------------------------------
## Base R plus haven (reads Stata .dta files). No tidyverse needed.
suppressPackageStartupMessages(library(haven))

## ---- 1. Reproducibility --------------------------------------------------
## Fix the random-number seed so the numbers are identical on every run.
## Seed chosen: 2026 (arbitrary but documented).
set.seed(2026)

## ---- 2. Read data --------------------------------------------------------
dta_path <- "Data/data_2011-07-26.dta"
d <- read_dta(dta_path)
cat(sprintf("Read %s: %d rows, %d columns.\n", dta_path, nrow(d), ncol(d)))

## `keep` marks the 58 countries in the Goldsmith & Horiuchi regressions;
## here we want the whole observed population instead.
cat(sprintf("Rows with keep == 1 (ignored for this lab): %d. Rows used: %d.\n",
            sum(d$keep == 1, na.rm = TRUE), nrow(d)))

## The two outcomes: UN voting in 2003 (a percentage of UN resolutions
## voted against the US) and Article 98 signing (0/1).
outcomes <- list(
  unvoting  = d$unvoting,
  article98 = d$article98
)

## ---- 3. Design of the simulation ----------------------------------------
n_reps <- 2000   # replications per sample size
sizes  <- c(10, 40)   # sample sizes to compare

## results[[var]][[as.character(n)]] holds the 2,000 sample averages.
results <- list()

## ---- 4. Simulate the sampling distribution for each variable -------------
for (var_name in names(outcomes)) {

  raw <- outcomes[[var_name]]
  pop <- raw[!is.na(raw)]        # observed values = the population
  n_total <- length(raw)
  n_pop   <- length(pop)
  pop_sd  <- sd(pop)
  pop_mn  <- mean(pop)

  cat(sprintf("\n=== %s ===\n", var_name))
  cat(sprintf("Rows in file: %d. Missing values dropped: %d. Population size N = %d.\n",
              n_total, n_total - n_pop, n_pop))
  cat(sprintf("Population mean = %.4f. Population SD = %.4f.\n", pop_mn, pop_sd))

  results[[var_name]] <- list(
    N = n_pop, pop_mean = pop_mn, pop_sd = pop_sd, means = list()
  )

  for (s in sizes) {
    stopifnot(s <= n_pop)   # cannot draw more without replacement than exist

    ## sample(pop, s, replace = FALSE) draws s DISTINCT countries.
    ## mean() is sum(x)/s, the arithmetic average.
    ## replicate() runs this n_reps times, giving independent draws.
    m <- replicate(n_reps, mean(sample(pop, size = s, replace = FALSE)))
    results[[var_name]]$means[[as.character(s)]] <- m

    ## The formula we are testing:
    ##   standard error of the mean = SD / sqrt(n)
    ## With sampling WITHOUT replacement from a finite population, the true
    ## SD is slightly smaller because later draws cannot repeat earlier
    ## ones:
    ##   SE = (SD / sqrt(n)) * sqrt((N - n) / N)
    se_formula <- pop_sd / sqrt(s)
    se_fpc     <- se_formula * sqrt((n_pop - s) / n_pop)

    cat(sprintf(
      "  n = %2d : mean of averages = %.4f | SD of averages = %.4f | sd/sqrt(n) = %.4f | with fpc = %.4f\n",
      s, mean(m), sd(m), se_formula, se_fpc
    ))
  }
}

## ---- 5. Plot the distributions of the sample averages ---------------------
out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## --- 5a. unvoting ---
png(filename = file.path(out_dir, "lab_se_unvoting_means.png"),
    width = 900, height = 450)
par(mfrow = c(1, 2))
for (s in sizes) {
  m <- results$unvoting$means[[as.character(s)]]
  hist(m, breaks = 30,
       main = sprintf("unvoting averages, n = %d (2,000 reps)", s),
       xlab = "Sample average of unvoting",
       col = "grey80", border = "white")
  abline(v = results$unvoting$pop_mean, col = "red", lwd = 2)  # population mean
}
dev.off()
cat(sprintf("\nHistogram saved to %s.\n", file.path(out_dir, "lab_se_unvoting_means.png")))

## --- 5b. article98 ---
png(filename = file.path(out_dir, "lab_se_article98_means.png"),
    width = 900, height = 450)
par(mfrow = c(1, 2))
for (s in sizes) {
  m <- results$article98$means[[as.character(s)]]
  hist(m, breaks = 30,
       main = sprintf("article98 signing shares, n = %d (2,000 reps)", s),
       xlab = "Share of sample signing an Article 98 agreement",
       col = "grey80", border = "white")
  abline(v = results$article98$pop_mean, col = "red", lwd = 2)  # population mean
}
dev.off()
cat(sprintf("Histogram saved to %s.\n", file.path(out_dir, "lab_se_article98_means.png")))

## ---- 6. Side-by-side summary table ---------------------------------------
cat("\n---- Summary: simulation vs formula ----\n")
cat(sprintf("%-11s %4s %10s %10s %10s %10s\n",
            "variable", "n", "sim SD", "sd/sqrt(n)", "with fpc", "centre pop"))
for (var_name in names(results)) {
  for (s in sizes) {
    r <- results[[var_name]]
    m <- r$means[[as.character(s)]]
    cat(sprintf("%-11s %4d %10.4f %10.4f %10.4f %10.4f\n",
                var_name, s, sd(m), r$pop_sd / sqrt(s),
                (r$pop_sd / sqrt(s)) * sqrt((r$N - s) / r$N), r$pop_mean))
  }
}

## ---- 7. Interpretation ---------------------------------------------------
cat("\n---- What the exercise shows ----\n")
cat(sprintf("unvoting:  SD of averages goes %.4f (n=10) -> %.4f (n=40).\n",
            sd(results$unvoting$means[["10"]]),
            sd(results$unvoting$means[["40"]])))
cat("            Quadrupling n divides the spread by about 2 (sqrt(4)),\n")
cat("            and the average of the averages stays at the population mean.\n")
cat(sprintf("article98: same pattern, %.4f -> %.4f.\n",
            sd(results$article98$means[["10"]]),
            sd(results$article98$means[["40"]])))
cat("            The simulated SD sits just BELOW sd/sqrt(n) because we\n")
cat("            sample without replacement from a finite population (FPC).\n")

cat("\nDone.\n")
```
