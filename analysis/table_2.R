# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 2: Event count, person years and incidence rate
# Output:  table_2.csv, table_2.html

library(readr); library(dplyr);library(lubridate)

## function for small number suppression
source("analysis/functions/redactor2.R")

cohort_start = as.Date("2020-01-29", format="%Y-%m-%d") # this is the same as the index date column - do you need both?
cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
cohort_days = cohort_end - cohort_start + 1
fs::dir_create(here::here("output", "review", "descriptives"))

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort <- "all"          # all eligible population
  #cohort <- "vaccinated"    # vaccinated population
  #cohort <- "infected"      # infected population
  #cohort <- "all_vax_c"     # all eligible population but follow-up censored by 1st vaccination
}else{
  cohort <- args[[1]]
}

table2_creation <- function(cohort){
  vax_c = FALSE
  if(cohort == "all_vax_c"){
    cohort = "all"
    vax_c = TRUE
  }
  ## Read in data and identify factor variables and numerical variables------------
  input <- read_rds(paste0("output/input_stage1_", cohort,".rds"))
  if(vax_c == TRUE){
    input$fup_end_date = input$fup_end_date_vax_c
    input$lcovid_surv = input$lcovid_surv_vax_c
  }
  ## define variables to keep
  vax <- names(input)[grepl("vax_covid_", names(input))]
  keep <- names(input)[!names(input)%in%(vax)]
  data <- input[,keep]
  
  ## calculate follow-up days
  data <- data %>% rename(person_days = lcovid_surv) # days from time origin to follow-up end date, previously calculated
  hist(data$person_days)


  
  ## function to calculate incidence rate for covid infection, do not calculate if event count <= 5
  compute_incidence_rate <- function(event_count, person_days_total){
      person_years_total = round(person_days_total / 365.2,4)
      # ir = incidence rate
      ir = round(event_count/person_years_total,4)
      ir_lower = round(ir - 1.96 * sqrt(event_count/person_years_total^2),4)
      ir_upper = round(ir + 1.96 * sqrt(event_count/person_years_total^2),4)
    return(c(event_count,person_years_total, ir, ir_lower, ir_upper))
  }
  
  ## create an empty data frame
  table_2 <- data.frame(outcome = character(),
                        subgrp = character(),
                        subgrp_level = character(),
                        event_count = numeric(),
                        person_years = numeric(),
                        ir = numeric(),
                        ir_lower = numeric(),
                        ir_upper = numeric()
                        )
  
  outcome <- c("covid", "long covid")
  subgrp <- subgrp_level <- c("main","main")
  # #hist(data$person_days)
  # YW: 21/09/2022, ideally the following filter should be removed or placed in stage_1_eligibility
  print("before applying filter [0, cohort_days]!")
  print(nrow(data))
  data <- data %>% filter(person_days >= 0 & person_days <= cohort_days)
  print("after applying filter [0, cohort_days]!")
  print(nrow(data))
  ## long covid count
  long_covid_count <- length(which(data$out_first_long_covid_date >= data$index_date &
                                     data$out_first_long_covid_date <= data$fup_end_date))
  
  ## covid count
  covid_count <- length(which(data$out_covid_date >= data$index_date &
                                data$out_covid_date <= data$fup_end_date))
  person_days_total = round(sum(data$person_days, na.rm=TRUE),1)
  
  table_2[1,4:8] <- compute_incidence_rate(covid_count, person_days_total)
  table_2[2,4:8] <- compute_incidence_rate(long_covid_count, person_days_total)
  table_2$outcome <- outcome
  table_2$subgrp <- table_2$subgrp_level <- subgrp <- subgrp_level
  
  ## extend to subgroups by demographics + post viral fatigue
  demographics <- c("cov_cat_sex", "cov_cat_age_group", "cov_cat_region", 
                    "cov_cat_ethnicity", "cov_cat_imd", "cov_cat_healthcare_worker",
                    "cov_cat_post_viral_fatigue_pre_pandemic", "cov_cat_bmi",
                    "cov_cat_smoking_status", "cov_cat_gp_patient_interaction")

  
  ##RK - not sure if this is just a dummy data check but if you think it could be in
  ##the real data, should this be moved to before calculating the main IR on line 77/78?
  
  ##YW - it could be move to before line 77/78 to save some computing time
  ##although it would not make a difference in results it is before or after
  
  
  ##start.time = Sys.time()
 
  for(i in demographics){
    for(outcome in c("covid", "long covid")){
            print(i)
            level <- names(table(data[,i]))
            print(level)
            start = nrow(table_2)+1
            end = nrow(table_2)+length(level)
            table_2[start:end,1] = rep(outcome, length(level))
            table_2[start:end,2] = rep(i, length(level))
            table_2$subgrp_level[start:end] = level
            index = start
            for(j in level){
             # index = which(data[,i]==j)
              print(j)
              sub_data <- data[which(data[,i]==j),]
              if(outcome == "covid"){
                count <- length(which(sub_data$out_covid_date >= sub_data$index_date &
                                            sub_data$out_covid_date <= sub_data$fup_end_date))
              }
              if(outcome=="long covid"){
                count <- length(which(sub_data$out_first_long_covid_date >= sub_data$index_date &
                                      sub_data$out_first_long_covid_date <= sub_data$fup_end_date))
  
              }
              # calculate total follow-up days
              person_days_total = round(sum(sub_data$person_days, na.rm=TRUE),1)
              table_2[index,4:8] <- compute_incidence_rate(count, person_days_total)
              index = index+1
            }
      }
  }
  table_2 <- table_2 %>%filter(outcome == "long covid")
  table(table_2$subgrp)

  ## redaction by subgroup
  ##RK - the redactor function looks like it's doing what it needs to do.
  ##Currently you put both the long covid and covid counts in as one but should these be redacted separately
  ##as these are separate outcomes? Otherwise you might redact one level in long covid then when it tries to 
  ##redact the next smallest it might do it in the covid outcome which won't remove the disclosure.
  
  ## YW: You are right! This is now amended by using redactor for covid and long covid separately
  
  
  for(i in demographics){
    for(j in c("covid", "long covid")){
      print(i)
      index = which(table_2$subgrp == i & table_2$outcome == j)
      table_2$event_count[index] <- redactor2(table_2$event_count[index])
    }
  }
  col_names <- c("event_count","person_years","ir", "ir_lower", "ir_upper")
  table_2$subgrp <- gsub("cov_cat_", "", table_2$subgrp)
  
  if(vax_c == TRUE){
    cohort = "all_vax_c"
  }
  write.csv(table_2, file=paste0("output/review/descriptives/table_2_", cohort,".csv"),row.names=F)
}
if(cohort == "all_cohorts") {
  table2_creation("all")
  table2_creation("vaccinated")
  table2_creation("infected")
  table2_creation("all_vax_c")
} else{
  table2_creation(cohort)
}
