# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Development models
# Output:  hazard ratios and 95% CI

library(readr); library(dplyr); library(rms)

data <- read_rds("output/survival_data.rds")

#fit_cox_model <-rms::cph(formula= ,data=, weight=,surv = TRUE,x=TRUE,y=TRUE)