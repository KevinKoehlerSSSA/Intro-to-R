library(tidyverse)
url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"
html <- read_html(url)

us_table <- html %>% html_elements(".wikitable") %>% html_table() %>% as.data.frame()

us_table <- us_table %>% 
  select(Name.birth.death., Term.16., Party.b..17..1) %>%
  rename(name=Name.birth.death.,
         term=Term.16,
         party=Party.b..17..1)

str_extract(us_table$name[1], ".+(?=\\()")

us_table$name[1]
