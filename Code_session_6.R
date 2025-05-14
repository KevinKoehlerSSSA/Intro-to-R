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


data <- data %>%
  mutate(
    iraq_dummy=ifelse(troops_iraq>0,1,0),
    afg_dummy=ifelse(troops_afgh>0,1,0),
    fh=pr+cl,
    nonnat=ifelse(nonnat1+nonnat2+nonnat3>0,1,0)
  )


#### Models 

install.packages("stargazer")
library(stargazer)

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


stargazer(ols1, ols2,
          dep.var.labels = "UN Voting with US in 2003",
          title = "Regression Results (OLS only)",
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


stargazer(ols1, ols2,
          dep.var.labels = "UN Voting with US in 2003",
          title = "Regression Results (OLS only)",
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
          type = "html",
          out = "models.html",
          no.space = T,
          header = F, 
          digits=2)

### Heteroscedasticity robust standard errors

library(stargazer)
library(sandwich)

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
