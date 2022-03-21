# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Prepare variables
# Output:  input.rds
library(readr); library(dplyr); library("arrow"); library("data.table")

input <- read_feather("output/input.feather")
View(input)
names(input)
which(grepl("bmi", names(input))==TRUE)
names(input)[which(grepl("cov_num", names(input))==TRUE)]
snomed_vars <- names(input)[which(grepl("snomed", names(input))==TRUE)]

# snomed_266226000: post viral debility
# snomed_272038003: Complaining of postviral syndrome 
# snomed_51771007 : Postviral fatigue syndrome (disorder)
input = input[,!(names(input) %in% snomed_vars)]

# create an indicator variable for covid infection
input$out_covid <- ifelse(is.na(input$out_covid_date), FALSE, TRUE)

# create a categorical variable to indicate covid phenotype: no infection, non-hospitalised covid and hospitalised covid
input$cov_cat_covid_phenotype <- ifelse(is.na(input$out_covid_date), "no infection", "non-hospitalised covid")
index = which(!is.na(input$hospital_covid))  # index for hospitalise covid
input$cov_cat_covid_phenotype[index] <- "hospitalised covid"
table(input$cov_cat_covid_phenotype)

table(input$first_long_covid_code)

vars_to_drop <- c("sgss_positive", "sgss_positive", "primary_care_covid", "hospital_covid",
                  "primary_care_death_date",  "ons_died_from_any_cause_date", "first_post_viral_fatigue_date")
input = input[,!(names(input) %in% vars_to_drop)]

# partial sorting by variable names in the data frame, keep patient_id and practice_id at the front
input <- input %>% select(patient_id, practice_id, sort(tidyselect::peek_vars()))
