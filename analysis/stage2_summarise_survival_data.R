# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: summarise /explore  survival data
# Output:  summary of survival data

library(prodlim);library(readr); library(dplyr); library(survival)

fs::dir_create(here::here("output", "review", "descriptives"))

data <- read_rds("output/input_stage1_all.rds")
data <- data %>% dplyr::select(lcovid_surv, lcovid_cens)
attach(data)

# Summarise follow-up, reverse the event indicator to summarise follow-up
fup <- quantile(prodlim(Hist(lcovid_surv,lcovid_cens)~1,reverse=TRUE))
a <- fup[1]$quantiles.survival[3,] # median follow-up time

# Median time to long covid diagnosis
surv_time <- quantile(prodlim(Hist(lcovid_surv,lcovid_cens)~1,reverse=FALSE))
b <- surv_time[1]$quantiles.survival[3,] # median survival time

results <- rbind(a,b)
# return NA if not observing 50% people experiencing the event of interest by the end of the follow-up time

rownames(results) <- c("median follow-up", "median survival time")
#colnames(results) <- c("quantile")
write.csv(results, file="output/review/descriptives/summarise_survival_data.csv")
