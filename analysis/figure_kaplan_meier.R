# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: cumulative probability of long COVID by age and  sex
# Output:  Two KM plots by age and sex
library(readr); library(dplyr);library(survival);library(survminer);library(tidyverse)
source("analysis/functions/function_round_km.R")
fs::dir_create(here::here("output", "review", "descriptives"))
cohort = "all"
input <- read_rds(paste0("output/input_stage1_", cohort,".rds"))
input <- input %>% select(lcovid_surv, lcovid_cens, contains("sex"), contains("cov_cat_age"))

levels(input$cov_cat_age_group) <- c("18-39", "40-59", "60-79", "80+")
table(input$cov_cat_age_group)
table(input$cov_cat_sex)
levels(input$cov_cat_sex) <- c("Female", "Male")

dat_sex <- round_km(input, "lcovid_surv", "lcovid_cens", "cov_cat_sex", threshold=6)

for(i in c("Female", "Male")){
  dat_sex$x[which(dat_sex$cov_cat_sex ==i)] <- seq(length=length(which(dat_sex$cov_cat_sex ==i))) #Add case numbers (in order, since sorted)
  dat_sex$y[which(dat_sex$cov_cat_sex ==i)] <- cumsum(replace_na(dat_sex$n.event[which(dat_sex$cov_cat_sex ==i)], 0))
}
library(ggplot2)
  ggplot(dat_sex, aes(x,y, group = cov_cat_sex)) + geom_path() + #Ploting
  # scale_y_continuous(name= "Days since follow-up start date") +
  # scale_x_continuous(name= "Cumulative incidence of long COVID")+
  labs(title="",x="\nDays since 1 December 2020", y = "\nCumulative incidence of long COVID\n")
