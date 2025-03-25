library(tidyverse)

## example function
greater5 <- function(x) {
  if (!is.numeric(x)) {
    stop(paste0("Argument must be numeric.\n",
                "You provided an object of class: ", 
                class(x)[1],
                ". You moron."))
  } # check if input is numeric, return error if not
  result <- ifelse(x > 5, 
                   paste(x, "is greater than 5"), 
                   paste(x, "is not greater than 5"))
  return(result)  # Return results 
}

## read data into R

tun22 <- read_csv("https://raw.githubusercontent.com/KevinKoehlerSSSA/Intro-to-R/refs/heads/main/tunisia_survey.csv")

## Typical age of all respondents

# using the mean() function
mean(tun22$age, na.rm=T) 

# the same thing manually
sum(tun22$age,na.rm=T)/sum(!is.na(tun22$age))

## typical age of Saied voters
mean(tun22$age[tun22$pres2019_1==1], na.rm = T)

# or, manually:
sum(tun22$age[tun22$pres2019_1==1], na.rm = T)/sum(!is.na(tun22$age[tun22$pres2019_1 == 1]))

## Variation in education levels
sd(tun22$edu[tun22$pres2019_1==1], na.rm=T)

## Write a function which calculates all of these measures at the same time.

summary_function <- function(x) {
  x_mean <- mean(x, na.rm=T)
  x_median <- median(x, na.rm=T)
  x_range <- max(x, na.rm=T)-min(x, na.rm=T)
  x_sd <- sd(x, na.rm=T)
  cat("Mean: ", x_mean, "\n",
      "Median: ", x_median, "\n",
      "Range: ", x_range, "\n",
      "SD: ", x_sd, "\n")
}

summary_function(tun22$edu)
