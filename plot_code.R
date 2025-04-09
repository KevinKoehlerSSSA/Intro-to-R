library(tidyverse)
library(vdemdata)

tun22 <- read_csv("https://raw.githubusercontent.com/KevinKoehlerSSSA/Intro-to-R/refs/heads/main/tunisia_survey.csv")

plot <- ggplot(data=tun22[!is.na(tun22$mps),]) +
  geom_bar(aes(x=mps)) +
  theme_classic() +
  xlab("MPs quickly lose touch") +
  ylab("Number of respondents")
plot

plot <- tun22 %>%
  select(mps) %>%
  filter(!is.na(mps)) %>%
  group_by(mps) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)*100) %>%
  ggplot() +
  geom_bar(aes(x=mps, y=perc), 
           stat="identity",
           fill="blue") +
  theme_classic() +
  xlab("MPs quickly lose touch") +
  ylab("Percentage of respondents")
plot

plot <- tun22 %>%
  select(mps, female) %>%
  filter(!is.na(mps)) %>%
  group_by(female, mps) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)*100) %>%
  ggplot() +
  geom_bar(aes(x=female, y=perc, fill=mps), 
           stat="identity") +
  theme_minimal() +
  ylab("Percent") +
  xlab("Gender") +
  labs(fill = "MPs lose touch")
plot  

plot <- tun22 %>%
  select(mps, leg2019_voted) %>%
  mutate(leg2019_voted=factor(leg2019_voted,
                              levels=c(1,2,98),
                              labels=c("voted","did not vote","don't remember"))) %>%
  filter(!is.na(mps) & leg2019_voted!="don't remember") %>%
  group_by(leg2019_voted, mps) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)*100) %>%
  ggplot() +
  geom_bar(aes(x=leg2019_voted, y=perc, fill=mps), 
           stat="identity") +
  theme_minimal() +
  ylab("Percent") +
  xlab("2019 legislative elections") +
  labs(fill = "MPs lose touch")
plot  

data <- vdem  

data <- data %>%
  dplyr::select(v2x_polyarchy,
                e_gdppc,
                year,
                country_name,
                e_regionpol)


plot <- data %>%
  mutate(dem=ifelse(v2x_polyarchy>=0.5,1,0)) %>%
  group_by(year) %>%
  summarize(perc_dem=mean(dem,na.rm=T)*100) %>%
  ggplot() +
  geom_line(aes(x=year, y=perc_dem)) +
  theme_classic() +
  xlab("") +
  ylab("Percentage of democracies")
plot

plot <- data %>%
  mutate(dem=ifelse(v2x_polyarchy>=0.5,1,0),
         region = factor(e_regionpol,
                         levels = 1:10,
                         labels = c("Eastern Europe",
                                    "Latin America",
                                    "MENA",
                                    "Sub–Saharan Africa",
                                    "WEIRD",
                                    "Eastern Asia",
                                    "South–Eastern Asia",
                                    "Southern Asia",
                                    "The Pacific",
                                    "The Caribbean")),
         reg_binary=ifelse(region=="WEIRD","WEIRD","non-WEIRD")) %>%
  group_by(year, reg_binary) %>%
  summarize(perc_dem=mean(dem,na.rm=T)*100) %>%
  ggplot() +
  geom_line(aes(x=year, y=perc_dem, group=reg_binary, color=reg_binary),
            size=1) +
  theme_classic() +
  xlab("") +
  ylab("Percentage of democracies by region") +
  labs(color="Region") +
  theme(legend.position = "bottom")
plot

plot <- data %>% 
  filter(year>1969 & year<1980) %>%
  rename(democracy=v2x_polyarchy) %>%
  mutate(gdp=log(e_gdppc)) %>%
  ggplot() +
  geom_point(aes(
    x=gdp,
    y=democracy
  ), alpha=0.6, size=1) +
  geom_smooth(aes(
    x=gdp,
    y=democracy),
    method = "lm") +
  theme_classic() +
  labs(x="log(GDP/capita)",
       y="V-Dem polyarchy score")
plot 

plot <- data %>%
  mutate(
    region = factor(e_regionpol,
                    levels = 1:10,
                    labels = c(
                      "Eastern Europe",
                      "Latin America",
                      "MENA",
                      "Sub–Saharan Africa",
                      "WEIRD",
                      "Eastern Asia",
                      "South–Eastern Asia",
                      "Southern Asia",
                      "The Pacific",
                      "The Caribbean"
                    )),
    reg_binary=ifelse(region=="WEIRD","WEIRD","non-WEIRD")) %>%
  filter(year > 1969 & year < 1980) %>%
  rename(democracy = v2x_polyarchy) %>%
  mutate(gdp = log(e_gdppc)) %>%
  ggplot(aes(x = gdp, y = democracy)) +
  geom_point(alpha=0.3, size=1, show.legend = F) +
  geom_smooth(method = "lm", show.legend = FALSE) +
  theme_classic() +
  labs(
    x = "log(GDP/capita)",
    y = "V-Dem polyarchy score"
  ) +
  facet_wrap(~ reg_binary, ncol = 3)
plot
