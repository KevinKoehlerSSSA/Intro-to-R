library(rvest)
library(tidyverse)

# 2014

# URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/2014_Tunisian_parliamentary_election"

# Read the page content
page <- read_html(url)

# Extract all tables
tables <- page %>% html_nodes("table.wikitable")

# Convert the relevant table to a data frame
election_results <- tables[[2]] %>% 
  html_table(fill = TRUE) %>% 
  as.data.frame()

election_results <- election_results[-1,2:5]
  
# Rename columns to meaningful names (adjust as needed)
colnames(election_results) <- c("Party", "Votes2014", "Percentage2014", "Seats2014")

res2014 <- election_results

## 2019

# URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/2019_Tunisian_parliamentary_election"

# Read the page content
page <- read_html(url)

# Extract all tables
tables <- page %>% html_nodes("table.wikitable")

# Convert the relevant table to a data frame
election_results <- tables[[1]] %>% 
  html_table(fill = TRUE) %>% 
  as.data.frame()

election_results <- election_results[-1,2:5]


# Rename columns to meaningful names (adjust as needed)
colnames(election_results) <- c("Party", "Votes2019", "Percentage2019", "Seats2019")

res2019 <- election_results

write_csv(res2014, "res2014.csv")
write_csv(res2019, "res2019.csv")
