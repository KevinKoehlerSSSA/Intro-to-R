install.packages("haven")
install.packages("psych")
library(haven)
library(psych)

data <- read_dta("/Users/kevin/Dropbox (Personal)/Sant\'Anna/Teaching/2025 - 1 - Data Analysis in R/data_2011-07-26.dta")

sumstats <- data %>%
  select(-country, -iso3166) %>%
  describe() %>%
  select(n, mean, sd, min, max) %>%
  mutate(mean=round(mean,2),
         sd=round(sd,2),
         min=round(min,2),
         max=round(max,2))

write_csv(sumstats, "summary_statistic.csv")
