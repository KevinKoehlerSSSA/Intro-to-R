library(tidyverse)
library(haven)
library(stargazer)
library(psych)
library(knitr)
library(kableExtra)
library(sandwich)


### Get data
data <- read_dta("data_2011-07-26.dta")


### Summary stats
sumstats <- data %>%
  select(-country, -iso3166) %>%
  describe()  %>%
  as.data.frame() %>%
  select(n, mean, sd, min, max) %>%
  mutate(mean=round(mean,2),
         sd=round(sd,2),
         min=round(min,2),
         max=round(max,2))

kable(sumstats, 
      longtable = TRUE, 
      booktabs = TRUE,
      caption = "Summary statistics", label = "tab:sum_stats") %>%
  kable_styling(latex_options = c("repeat_header", 
                                  "striped",
                                  "condensed"))

### Variables

data <- data %>%
  mutate(
    positive=rowMeans(cbind(pos1, pos2, pos3), na.rm = T),
    negative=rowMeans(cbind(neg1, neg2, neg3), na.rm = T),
    fp_views=(positive-negative)/100,
    iraq_dummy=ifelse(troops_iraq>0,1,0),
    afg_dummy=ifelse(troops_afgh>0,1,0),
    fh=pr+cl,
    nonnat=ifelse(nonnat1+nonnat2+nonnat3>0,1,0)
  )

### OLS models

ols1 <- lm(unvoting~
             fp_views,
           data=data)


ols2 <- lm(unvoting~
             fp_views +
             afg_dummy +
             icc +
             s_lead +
             nato +
             aid_m +
             aid_e +
             lntrade +
             lngdppc +
             fh +
             muslimpct +
             europe,
           data=data)

robust_se_ols1 <- sqrt(diag(vcovHC(ols1, type = "HC1")))
robust_se_ols2 <- sqrt(diag(vcovHC(ols2, type = "HC1")))

stargazer(ols1, ols2,
          se = list(robust_se_ols1, robust_se_ols2),
          dep.var.caption = "",
          dep.var.labels = "UN Voting with US in 2003",
          title = "Regression Results (OLS only, Robust SEs)",
          covariate.labels = c("Opinion on US FP",
                               "Troops in AFG",
                               "ICC member",
                               "Alliance portfolio",
                               "NATO",
                               "US military aid",
                               "US economic aid",
                               "Trade with US",
                               "GDP per capita",
                               "Democracy score",
                               "Muslim population",
                               "Europe",
                               "Constant"),
          type = "text",
          no.space = T,
          header = F, 
          digits=2,
          table.placement = "H")

### Probit regressions

probit1 <- glm(iraq_dummy~
                fp_views,
              data=data,
              family=binomial(link="probit"))

probit2 <- glm(iraq_dummy~
                 fp_views +
                 afg_dummy +
                 icc +
                 s_lead +
                 nato +
                 aid_m +
                 aid_e +
                 lntrade +
                 lngdppc +
                 fh +
                 muslimpct +
                 europe,
               data=data,
               family=binomial(link="probit"))

probit3 <- glm(article98~
                 fp_views,
               data=data,
               family=binomial(link="probit"))

probit4 <- glm(article98~
                 fp_views +
                 afg_dummy +
                 icc +
                 s_lead +
                 aid_m +
                 aid_e +
                 lntrade +
                 lngdppc +
                 fh +
                 muslimpct,
               data=data,
               family=binomial(link="probit"))

robust_se_probit1 <- sqrt(diag(vcovHC(probit1, type = "HC3")))
robust_se_probit2 <- sqrt(diag(vcovHC(probit2, type = "HC0")))
robust_se_probit3 <- sqrt(diag(vcovHC(probit3, type = "HC3")))
robust_se_probit4 <- sqrt(diag(vcovHC(probit4, type = "HC0")))

stargazer(probit1,probit2,probit3,probit4,ols1,ols2,
          se = list(robust_se_probit1,
                    robust_se_probit2,
                    robust_se_probit3,
                    robust_se_probit4,
                    robust_se_ols1, 
                    robust_se_ols2),
          dep.var.caption = "",
          dep.var.labels = c("Troops to Iraq","BIA in force"),
          title = "Regression Results",
          covariate.labels = c("Opinion on US FP",
                               "Troops in AFG",
                               "ICC member",
                               "Alliance portfolio",
                               "NATO",
                               "US military aid",
                               "US economic aid",
                               "Trade with US",
                               "GDP per capita",
                               "Democracy score",
                               "Muslim population",
                               "Europe",
                               "Constant"),
          type = "text",
          no.space = T,
          header = F, 
          digits=2)
