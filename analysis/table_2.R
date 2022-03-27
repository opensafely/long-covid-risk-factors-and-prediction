# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 2: Event count, person years and incidence rate
# Output:  table_2.csv, table_2.html

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate); library(htmlTable)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

drop <-  names(input)[grepl("cov_", names(input))]
keep <- names(input)[!names(input)%in%(drop)]
drop <- names(input)[grepl("vax_", names(input))]
keep <- names(input)[!names(input)%in%(drop)]
data <- input[,keep]

# to increase efficiency
rm(input)

# study period: index date = "2020-12-01", end date = "2022-03-31"
cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
data$cohort_end_date = cohort_end

# specify follow-up end date

data <- data %>% rowwise() %>% mutate(follow_up_end_date=min(out_first_long_covid_date, death_date, cohort_end_date,na.rm = TRUE))

data <- data %>% filter(follow_up_end_date >= index_date & follow_up_end_date != Inf)

# calculate follow-up days
data <- data %>% mutate(person_days = as.numeric(as.Date(follow_up_end_date) - as.Date(index_date))+1)
hist(data$person_days)
data <- data %>% filter(person_days >= 1 & person_days <= 486)
person_days_total = round(sum(data$person_days, na.rm=TRUE),1)

# long covid count
long_covid_count <- length(which(data$out_first_long_covid_date >= data$index_date &
                                 data$out_first_long_covid_date <= data$follow_up_end_date))

# covid count
covid_count <- length(which(data$out_covid_date >= data$index_date &
                                   data$out_covid_date <= data$follow_up_end_date))

# write a function to calculate incidence rate
# incidence rate for covid infection, do not calculate if event count <= 5
compute_incidence_rate <- function(event_count, person_days_total){
  if(event_count > 5){
    person_years_total = round(person_days_total / 365.2,4)
    # ir = incidence rate
    ir = round(event_count/person_years_total,4)
    ir_lower = round(ir - 1.96 * sqrt(event_count/person_years_total^2),4)
    ir_upper = round(ir + 1.96 * sqrt(event_count/person_years_total^2),4)
  }else{
    person_years_total = event_count = ir = ir_lower = ir_upper = "redacted"
  }
  return(c(event_count,person_years_total, ir, ir_lower, ir_upper))
}

# create an empty data frame

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


# next step: extend to subgroups by demographics
input <- read_rds("output/input_stage1.rds")
# Define age groups
input$cov_cat_age_group <- ""
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_110", input$cov_cat_age_group)
demographics <- c("cov_cat_sex", "cov_cat_age_group" , "cov_cat_region", 
                  "cov_cat_ethnicity", "cov_cat_imd", "cov_cat_healthcare_worker")

keep <-  names(input)[grepl("date", names(input))]
data <- input[,c(keep, demographics)]
drop <- names(data)[grepl("vax_", names(data))]
keep <- names(data)[!names(data)%in%(drop)]
data <- input[,keep]
cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
data$cohort_end_date = cohort_end

nrow(table_2)

#to improve efficiency
rm(input)

# outcome = "covid"
# outcome = "long covid"
# for(outcome in c("covid", "long covid")){
#     for(i in demographics){
#           print(i)
#           level <- names(table(data[,i]))
#           print(level)
#           start = nrow(table_2)+1
#           end = nrow(table_2)+length(level)
#           table_2[start:end,1] = rep(outcome, length(level))
#           table_2[start:end,2] = rep(i, length(level))
#           table_2$subgrp_level[start:end] = level
#           index = start
#           for(j in level){
#            # index = which(data[,i]==j)
#             print(j)
#             sub_data <- data[which(data[,i]==j),]
#             sub_data <- sub_data %>% rowwise() %>% mutate(follow_up_end_date=min(out_first_long_covid_date, death_date, cohort_end_date,na.rm = TRUE))
#             sub_data <- sub_data %>% filter(follow_up_end_date >= index_date & follow_up_end_date != Inf)
#             if(outcome == "covid"){
#               count <- length(which(sub_data$out_covid_date >= sub_data$index_date &
#                                           sub_data$out_covid_date <= sub_data$follow_up_end_date))
#             }
#             if(outcome=="long covid"){
#               count <- length(which(sub_data$out_first_long_covid_date >= sub_data$index_date &
#                                     sub_data$out_first_long_covid_date <= sub_data$follow_up_end_date))
#               
#             }
#             # calculate follow-up days
#             sub_data <- sub_data %>% mutate(person_days = as.numeric(as.Date(follow_up_end_date) - as.Date(index_date))+1)
#             #hist(data$person_days)
#             sub_data <- sub_data %>% filter(person_days >= 1 & person_days <= 486)
#             person_days_total = round(sum(sub_data$person_days, na.rm=TRUE),1)
#             table_2[index,4:8] <- compute_incidence_rate(count, person_days_total)
#             index = index+1
#           }
#     }
# 
# }
table_2$subgrp <- gsub("cov_cat_", "", table_2$subgrp)

write.csv(table_2, file="output/table_2.csv")

htmlTable(table_2, file="output/table_2.html")

