install.packages("finalfit")
library(finalfit)
library(dplyr)
library(ggplot2)

data(colon_s)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
  hr_plot(dependent, explanatory, dependent_label = "Survival")
