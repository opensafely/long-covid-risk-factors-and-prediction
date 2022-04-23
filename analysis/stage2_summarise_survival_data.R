# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: summarise /explore  survival data
# Output:  summary of survival data

library(prodlim);library(readr); library(dplyr); library(survival)

data <- read_rds("output/input_stage1_all.rds")

attach(data)
#select <- data.frame(lcovid_surv, lcovid_cens)
#View(select)

# Summarise follow-up, scaled to years, reverse the event indicator to summarise follow-up

fup <- quantile(prodlim(Hist(lcovid_surv,lcovid_cens)~1,reverse=TRUE))
a <- fup[1]$quantiles.survival[3,] # median follow-up time

# Median time to long covid diagnosis
surv_time <- quantile(prodlim(Hist(lcovid_surv,lcovid_cens)~1,reverse=FALSE))
b <- surv_time[1]$quantiles.survival[3,] # median survival time

results <- rbind(a,b)

rownames(results) <- c("median follow-up", "median survival time")
#colnames(results) <- c("quartile")
write.csv(results, file="output/summarise_survival_data.csv")

rmarkdown::render("analysis/compiled_summarsied_survival_data.Rmd",
                  output_file="summarise_survival_data",output_dir="output")
