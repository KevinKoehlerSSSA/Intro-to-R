library(tidyverse)
url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"
html <- read_html(url)

us_table <- html %>% html_elements(".wikitable") %>% html_table()
