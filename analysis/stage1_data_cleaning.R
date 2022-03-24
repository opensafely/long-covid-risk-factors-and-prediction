# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Prepare variables
# Output:  input.rds

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate)

input <- read_feather("output/input.feather")
#View(input)
#names(input)

# define cohort start date:
index_date="2020-11-01"
input$index_date = index_date

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
input <- input %>% select(patient_id, practice_id, index_date, death_date,  colnames(input)[grepl("out_",colnames(input))], colnames(input)[grepl("vax_",colnames(input))], sort(tidyselect::peek_vars()))

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

# cov_cat_asthma ---------------------------------------------------------------
levels(input$cov_cat_asthma)[levels(input$cov_cat_asthma)==0] <-"No asthma"
levels(input$cov_cat_asthma)[levels(input$cov_cat_asthma) == 1 | levels(input$cov_cat_asthma) == 2 ] <-"Asthma"


input$cov_cat_imd <- ordered(input$cov_cat_imd, levels = c("0 (missing)","1 (most deprived)","2","3","4","5 (least deprived)"))
table(input$cov_cat_imd)

## cov_cat_smoking_status-------------------------------------------------------
table(input$cov_cat_smoking_status)
levels(input$cov_cat_smoking_status) <- list("Ever smoker" = "E", "Missing" = "M", "Never smoker" = "N", "Current smoker" = "S")
input$cov_cat_smoking_status <- ordered(input$cov_cat_smoking_status, levels = c("Never smoker","Ever smoker","Current smoker","Missing"))
table(input$cov_cat_smoking_status)

# specify date variables in the format of "%Y-%m-%d"----------------------------
vars_dates <- grep("date", names(input))
vars_dates <- names(input)[vars_dates]

table(input$cov_cat_stroke_or_dementia)

convert_to_date <- function(x){
  as.Date(x,format = "%Y-%m-%d")
}
input[vars_dates] = lapply(input[vars_dates], convert_to_date)
lapply(input[vars_dates], is.Date)

#View(input[,vars_dates])

# Step 4. Define eligible population--------------------------------------------
steps <- c("starting point","dead before index date", "missing sex", "missing age", "age <18y", "age>105y", "ethnicity")
# starting point
flow_chart_n <- nrow(input)

# Dead: removed if dead before index date
input <- input%>%filter(death_date > index_date | is.na(death_date))
flow_chart_n <- c(flow_chart_n, nrow(input))

# Sex: remove if missing
input <- input%>%filter(!is.na(cov_cat_sex))
table(input$cov_cat_sex)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Age: remove if missing
input <- input%>%filter(!is.na(cov_num_age))
#table(input$cov_cat_age)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Adult: remove if age < 18
input <- input%>%filter(cov_num_age>=18)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Adult: remove if age > 105 years
input <- input%>%filter(cov_num_age <=105)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Ethnicity: remove if missing
input <- input%>%filter(!is.na(cov_cat_ethnicity))
flow_chart_n <- c(flow_chart_n, nrow(input))

# previous covid
#table(input$cov_cat_previous_covid)
#input <- input%>%filter(cov_cat_previous_covid == " No COVID code")
#flow_chart_n <- c(flow_chart_n, nrow(input))

flow_chart<-cbind(steps, flow_chart_n)

write.csv(flow_chart, file="output/flow_chart.csv")

# For categorical variables, replace "na" with "Missing" as a category
cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
input_factor_vars <- input[,cov_factor_names]

for(i in 1:length(cov_factor_names)){
  index = which(is.na(input_factor_vars[,i]))
  if(length(index)>0){
    input_factor_vars[index,i]="Missing"
  }
}

# # vaccination status is a dynamic variable
# input[,cov_factor_names] <- input_factor_vars 
# input$vax_doses <- NULL
# index = which(!is.na(input$vax_covid_date1)& input$vax_covid_date1  < input$index_date)
# input$vax_does[index] = 1

saveRDS(input, file = "output/input_stage1.rds")

