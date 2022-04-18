# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Prepare variables
# Output:  input_stage0.rds

library(readr); library(dplyr); library("arrow"); library(lubridate)

input <- read_feather("output/input.feather")
#View(input)
#names(input)

# define cohort start date:
index_date=as.Date("2020-12-01")
input$index_date = as.Date(index_date)

# Step 1. Define variables: COVID infection, long COVID-------------------------
# create an indicator variable for covid infection
input$out_covid <- ifelse(is.na(input$out_covid_date), FALSE, TRUE)


# create a categorical variable to indicate covid phenotype: -------------------
# no infection, non-hospitalised covid and hospitalised covid
input$cov_cat_covid_phenotype <- ifelse(is.na(input$out_covid_date), "no_infection", "non_hospitalised")
index = which(!is.na(input$hospital_covid))  # index for hospitalise covid
input$cov_cat_covid_phenotype[index] <- "hospitalised"

# Step 2. Remove variables which are not included in the analysis---------------
# remove variables start with snomed
snomed_vars <- names(input)[which(grepl("snomed", names(input))==TRUE)]
input = input[,!(names(input) %in% snomed_vars)]

tmp_vars <- names(input)[which(grepl("tmp", names(input))==TRUE)]
input = input[,!(names(input) %in% tmp_vars)]
vars_to_drop <- c("sgss_positive", "sgss_positive", "primary_care_covid", "hospital_covid",
                  "primary_care_death_date",  "ons_died_from_any_cause_date", "first_post_viral_fatigue_date")
input = input[,!(names(input) %in% vars_to_drop)]

# partial sorting by variable names in the data frame, keep patient_id and practice_id at the front
input <- input %>% select(patient_id, practice_id, index_date, death_date,
                          colnames(input)[grepl("out_",colnames(input))],
                          colnames(input)[grepl("vax_",colnames(input))],
                          sort(tidyselect::peek_vars()))

# Step 3. define variable types: factor or numerical or date--------------------

# For categorical factors, specify references-----------------------------------
cat_factors <- colnames(input)[grepl("_cat_",colnames(input))]
input[,cat_factors] <- lapply(input[,cat_factors], function(x) factor(x, ordered = FALSE))

# cov_cat_imd by quintile-------------------------------------------------------
table(input$cov_cat_imd)
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==0] <-"0 (missing)"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==1] <-"1 (most deprived)"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==2] <-"2"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==3] <-"3"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==4] <-"4"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==5] <-"5 (least deprived)"

# ordered categorical factor, the first level is the reference
input$cov_cat_imd <- ordered(input$cov_cat_imd, 
                             levels = c("1 (most deprived)","2","3","4","5 (least deprived)", "0 (missing)"))

# for ordered factor, the first level is taken as reference level

#table(input$cov_cat_imd)


# input <- input %>% filter(cov_cat_imd != "0 (missing)")
# 
# input$cov_cat_imd <- ordered(input$cov_cat_imd, levels = c("1 (most deprived)","2","3","4","5 (least deprived)"))

# cov_cat_asthma ---------------------------------------------------------------
levels(input$cov_cat_asthma)[levels(input$cov_cat_asthma)==0] <-"No asthma"
levels(input$cov_cat_asthma)[levels(input$cov_cat_asthma) == 1 | levels(input$cov_cat_asthma) == 2 ] <-"Asthma"

## cov_cat_smoking_status-------------------------------------------------------
table(input$cov_cat_smoking_status)
levels(input$cov_cat_smoking_status) <- list("Ever smoker" = "E", "Missing" = "M", "Never smoker" = "N", "Current smoker" = "S")
input$cov_cat_smoking_status <- ordered(input$cov_cat_smoking_status, levels = c("Never smoker","Ever smoker","Current smoker","Missing"))
table(input$cov_cat_smoking_status)

# cov_cat_age_group-------------------------------------------------------------
# Define age groups
input$cov_cat_age_group <- ""
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_110", input$cov_cat_age_group)

# specify date variables in the format of "%Y-%m-%d"----------------------------
vars_dates <- grep("date", names(input))
vars_dates <- names(input)[vars_dates]

table(input$cov_cat_stroke_or_dementia)

convert_to_date <- function(x){
  as.Date(x,format = "%Y-%m-%d")
}
input[vars_dates] = lapply(input[vars_dates], convert_to_date)
lapply(input[vars_dates], is.Date)

# define a variable covid_history to indicate if individuals have covid infection before the start of the cohort
input$sub_cat_covid_history <-ifelse(input$out_covid_date < input$index_date, TRUE, FALSE)

select_variables <- input %>% select(c(sub_cat_covid_history, out_covid_date, index_date))


## cov_cat_region if replace with missing causes problem, temporarily comment out to run on real data
# # For categorical variables, replace "na" with "Missing" as a category
# cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
# input_factor_vars <- input[,cov_factor_names]

for(i in 1:length(cov_factor_names)){
  print(table(input_factor_vars[,i]))
}

for(i in 1:length(cov_factor_names)){
  index = which(is.na(input_factor_vars[,i]))
  if(length(index)>0){
    input_factor_vars[index,i]="Missing"
  }
  print(table(input_factor_vars[,i]))
}

input[,cov_factor_names] <- input_factor_vars

for(i in cov_factor_names){
  print(table(input[,i]))
}

saveRDS(input, file = "output/input_stage0.rds")

