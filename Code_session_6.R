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


data <- data %>%
  mutate(
    positive=rowMeans(cbind(pos1, pos2, pos3), na.rm = T),
    negative=rowMeans(cbind(neg1, neg2, neg3), na.rm = T),
    fp_views=(positive-negative)/100)

data <- data %>%
  mutate(s1=pos1-neg1,
         s2=pos2-neg2,
         s3=pos3-neg3,
         fp_views=rowMeans(cbind(s1,s2,s3), na.rm=T))

