# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Prepare variables
# Output:  input.rds
library(readr); library(dplyr); library("arrow"); library("data.table"); library(base)

input <- read_feather("output/input.feather")
View(input)
names(input)

# Step 1. Define variables: COVID infection, long COVID
# create an indicator variable for covid infection
input$out_covid <- ifelse(is.na(input$out_covid_date), FALSE, TRUE)
# create a categorical variable to indicate covid phenotype: 
# no infection, non-hospitalised covid and hospitalised covid
input$cov_cat_covid_phenotype <- ifelse(is.na(input$out_covid_date), "no_infection", "non_hospitalised")
index = which(!is.na(input$hospital_covid))  # index for hospitalise covid
input$cov_cat_covid_phenotype[index] <- "hospitalised"

# table(input$cov_cat_covid_phenotype)
# table(input$first_long_covid_code)

# Step 2. Remove irrelevant variables which are not included in the analysis
# remove variables start with snomed code: this return the count for the diagnosis for the particular disease
# snomed_266226000: post viral debility
# snomed_272038003: Complaining of postviral syndrome 
# snomed_51771007 : Postviral fatigue syndrome (disorder)
snomed_vars <- names(input)[which(grepl("snomed", names(input))==TRUE)]
input = input[,!(names(input) %in% snomed_vars)]

vars_to_drop <- c("sgss_positive", "sgss_positive", "primary_care_covid", "hospital_covid",
                  "primary_care_death_date",  "ons_died_from_any_cause_date", "first_post_viral_fatigue_date")
input = input[,!(names(input) %in% vars_to_drop)]

# partial sorting by variable names in the data frame, keep patient_id and practice_id at the front
input <- input %>% select(patient_id, practice_id, sort(tidyselect::peek_vars()))

# Step 3. define variable types: factor or numerical or date

# For categorical factors, specify references
cat_factors <- colnames(input)[grepl("_cat_",colnames(input))]
input[,cat_factors] <- lapply(input[,cat_factors], function(x) factor(x, ordered = FALSE))

vars_dates <- grep("date", names(input))
vars_dates <- names(input)[vars_dates]

is.date <- function(x) inherits(x, 'Date')
# for(i in vars_dates){
#   print(i)
#   temp = input[,i]
#   temp = as.Date(as.vector(temp))
# }
# is.date(input$death_date)
# Step 4. Define eligible population









