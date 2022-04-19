# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: summarise /explore  survival data
# Output:  summary of survival data

library(prodlim);library(readr); library(dplyr); library(survival)

data <- read_rds("output/input_stage1.rds")

attach(data)
#select <- data.frame(lcovid_surv_vax_c, lcovid_i_vax_c)
#View(select)

# Summarise follow-up, scaled to years, reverse the event indicator to summarise follow-up

fup <- quantile(prodlim(Hist(lcovid_surv_vax_c,lcovid_i_vax_c)~1,reverse=TRUE))
a <- fup[1]$quantiles.survival[3,] # median follow-up time

# Median time to long covid diagnosis
surv_time <- quantile(prodlim(Hist(lcovid_surv_vax_c,lcovid_i_vax_c)~1,reverse=FALSE))
b <- surv_time[1]$quantiles.survival[3,] # median survival time

results <- rbind(a,b)

rownames(results) <- c("median follow-up", "median survival time")

saveRDS(results, file="output/survival_data.rds")