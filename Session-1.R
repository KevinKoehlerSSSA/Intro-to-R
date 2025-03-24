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


## Show all data sets included in R
data()

## write the arrests data set into the environment, calling it data
data <- arrests

## The typical value of murders per 100,000 inhabitants across all states is the mean
mean(data$Murder) # note that capitalization matters, data$murder would not work
# equivalently
sum(data$Murder)/length(data$Murder)

## The value such that 50% of states have lower murder rates is the median
median(data$Murder)
# equivalently (but, admittedly, more complicated)
data <- data %>%
  arrange(Murder) # arrange the data according to the murder rate
# take the average of the 25th and the 26th value
(data$Murder[length(data$Murder)/2]+data$Murder[(length(data$Murder)/2)+1])/2

## The most frequent murder rate is the mode
as.numeric(names(table(data$Murder)[table(data$Murder)==max(table(data$Murder))]))

## Describe how much states differ from each other in terms ofmurder rates. Which measures could you use? Why?
## I would use the range and the standard deviation

# Range:
max(data$Murder)-min(data$Murder)
# Standard deviation
sd(data$Murder)
# or, equivalently
sqrt(var(data$Murder))

# you can also calulate the variance
var(data$Murder)

# and the interquartile range
IQR(data$Murder)

## Write a function which calculates all of these measures at the same time.

summary_function <- function(x) {
  x_mean <- mean(x)
  x_median <- median(x)
  x_range <- max(x)-min(x)
  x_sd <- sd(x)
  cat("Mean: ", x_mean, "\n",
      "Median: ", x_median, "\n",
      "Range: ", x_range, "\n",
      "SD: ", x_sd, "\n")
}
