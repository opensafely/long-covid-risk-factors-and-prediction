# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Prepare data for survival analysis
# Output:  survival_data.rds

library(readr); library(dplyr)

## Read data
input <- read_rds("output/input_stage1.rds")

## define variables to keep
drop <- names(input)[grepl("cov_", names(input))]
keep <- names(input)[!names(input)%in%(drop)]
drop <- names(input)[grepl("vax_", names(input))]
keep <- names(input)[!names(input)%in%(drop)]
data <- input[,keep]

## to increase efficiency
rm(input)

## study period: index date = "2020-12-01", end date = "2022-03-31"
cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
data$cohort_end_date = cohort_end

#-------------------------------------------------------------------------------
## population = "unvaccinated"
## for unvaccianted population, the follow-up start date is the index date

## specify follow-up end date
data <- data %>% rowwise() %>% mutate(follow_up_end_date=min(out_first_long_covid_date, 
                                                             death_date, 
                                                             cohort_end_date,
                                                             na.rm = TRUE))

data <- data %>% filter(follow_up_end_date >= index_date & follow_up_end_date != Inf)

## define days since follow-up to long COVID diagnosis, vaccination censored long covid diagnosis
data$lcovid_surv_vax_c <- as.numeric(data$follow_up_end_date - data$index_date)

## define event indicator, vaccination censored long covid diagnosis
data <- data %>% mutate(lcovid_i_vax_c = ifelse((out_first_long_covid_date <= follow_up_end_date & 
                                           out_first_long_covid_date >= index_date &
                                           !is.na(out_first_long_covid_date)), 1, 0))
#-------------------------------------------------------------------------------
## Save data

saveRDS(data, file = "output/survival_data.rds")
