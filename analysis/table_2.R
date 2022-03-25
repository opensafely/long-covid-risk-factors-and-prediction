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
    person_years_total = person_days_total / 365.2
    # ir = incidence rate
    ir = round(event_count/person_years_total,4)
    ir_lower = round(ir - 1.96 * sqrt(event_count/person_years_total^2),4)
    ir_upper = round(ir + 1.96 * sqrt(event_count/person_years_total^2),4)
  }else{
    event_count = event_ir = event_ir_lower = event_ir_upper = "redacted"
  }
  return(c(event_count,person_years_total, ir, ir_lower, ir_upper))
}

# create an empty data frame

table_2 <- data.frame(outcome = character(),
                      event_count = numeric(),
                      person_years = numeric(),
                      ir = numeric(),
                      ir_lower = numeric(),
                      ir_upper = numeric()
                      )

outcome <- c("covid", "long covid")
table_2[1,2:6] <- compute_incidence_rate(covid_count, person_days_total)
table_2[2,2:6] <- compute_incidence_rate(long_covid_count, person_days_total)
table_2$outcome <- outcome

# next step: extend to subgroups by demographics
