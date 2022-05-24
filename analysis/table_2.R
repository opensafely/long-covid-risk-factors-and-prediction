# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 2: Event count, person years and incidence rate
# Output:  table_2.csv, table_2.html

library(readr); library(dplyr);library(lubridate)

# function for small number suppression
source("analysis/functions/redactor2.R")

fs::dir_create(here::here("output", "review", "descriptives"))

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  #cohort <- "all"          # all eligible population
  cohort <- "vaccinated"    # vaccinated population
}else{
  cohort <- args[[1]]
}

table2_creation <- function(cohort){
  ## Read in data and identify factor variables and numerical variables------------
  input <- read_rds(paste0("output/input_stage1_", cohort,".rds"))
  
  ## define variables to keep
  vax <- names(input)[grepl("vax_covid_", names(input))]
  keep <- names(input)[!names(input)%in%(vax)]
  data <- input[,keep]
  
  ## calculate follow-up days
  data <- data %>% rename(person_days = lcovid_surv) # days from time origin to follow-up end date, previously calculated
  hist(data$person_days)
  person_days_total = round(sum(data$person_days, na.rm=TRUE),1)
  
  ## long covid count
  long_covid_count <- length(which(data$out_first_long_covid_date >= data$index_date &
                                   data$out_first_long_covid_date <= data$fup_end_date))
  
  ## covid count
  covid_count <- length(which(data$out_covid_date >= data$index_date &
                                     data$out_covid_date <= data$fup_end_date))
  
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
  table_2[1,4:8] <- compute_incidence_rate(covid_count, person_days_total)
  table_2[2,4:8] <- compute_incidence_rate(long_covid_count, person_days_total)
  table_2$outcome <- outcome
  table_2$subgrp <- table_2$subgrp_level <- subgrp <- subgrp_level
  
  ## extend to subgroups by demographics
  demographics <- c("cov_cat_sex", "cov_cat_age_group", "cov_cat_region", 
                    "cov_cat_ethnicity", "cov_cat_imd", "cov_cat_healthcare_worker")
  
  #nrow(table_2)
  
  #RK - what are the following two lines for?
  #YW - these two lines are now deleted
  #outcome = "covid"
  #outcome = "long covid"
  
  # data <- data %>% rowwise() %>% mutate(fup_end_date=min(out_first_long_covid_date, death_date, cohort_end_date,na.rm = TRUE))
  # data <- data %>% filter(fup_end_date >= index_date & fup_end_date != Inf)
  ## calculate follow-up days
  
  #RK - why have you re-calculated  person days here? It looks like it's the same?
  #YW - I now delete this repeated calculation
  
  # data <- data %>% mutate(person_days = as.numeric(as.Date(fup_end_date) - as.Date(index_date))+1)
  # #hist(data$person_days)
  data <- data %>% filter(person_days >= 1 & person_days <= 486)
  
  
  #Rk - makes no difference to the script but if switch the two 'for' statements
  #around so that you do demographics first them outcome it means that you wont be 
  #subsetting the data twice when you don't need to i.e will subset then run for both 
  #outcomes rather than subset for one outcome then subset for the other outcome
  
  #YW - thanks for this suggestion, I now swap the two 'for' statements around
  
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
  
  table_2$event_count <- redactor2(table_2$event_count)
  
  # impose an NA for testing
  #table_2$event_count[1:2] = NA
  
  #RK - I don't think you need to redact the person years as that is a whole
  #population statistic so can keep if you want it
  
  # YW - I now keep person years
  
  col_names <- c("event_count", "ir", "ir_lower", "ir_upper")
  table_2[is.na(table_2$event_count),col_names] =rep("[redacted]",length(col_names))
  
  table_2$subgrp <- gsub("cov_cat_", "", table_2$subgrp)
  
  #RK - Kurt added these to the vacc/unvacc repo which mean that outcomes
  #can be saved in subdirectories within the outputs folder which has been useful
  #in case you wanted to include in this repo
  #fs::dir_create(here::here("output", "review", "descriptives"))
  write.csv(table_2, file=paste0("output/review/descriptives/table_2_", cohort,".csv"),row.names=F)
  
  rmarkdown::render("analysis/compilation/compiled_table2_results.Rmd", output_file=paste0("table_2_", cohort),output_dir="output")
}

if(cohort == "both") {
  table2_creation("all")
  table2_creation("vaccinated")
} else{
  table2_creation(cohort)
}