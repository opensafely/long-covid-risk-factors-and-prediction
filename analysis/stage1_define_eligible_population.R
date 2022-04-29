# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: define eligible population
# Output:  input_stage1_all.rds; input_stage1_vaccinated.rds

library(readr); library(dplyr); library(htmlTable)
input <- read_rds("output/input_stage0.rds")

################################################################################
## Part 1. Define eligible population                                          #
################################################################################

steps <- c("starting point","dead before index date", "missing sex", 
           "missing age", "age <18y", "age>105y", "ethnicity", "long covid before cohort start")
## starting point
flow_chart_n <- nrow(input)

## Dead: removed if dead before index date
input <- input%>%filter(death_date > index_date | is.na(death_date))
flow_chart_n <- c(flow_chart_n, nrow(input))

## Sex: remove if missing
input <- input%>%filter(!is.na(cov_cat_sex))
table(input$cov_cat_sex)
flow_chart_n <- c(flow_chart_n, nrow(input))

## Age: remove if missing
input <- input%>%filter(!is.na(cov_num_age))
#table(input$cov_cat_age)
flow_chart_n <- c(flow_chart_n, nrow(input))

## Adult: remove if age < 18
input <- input%>%filter(cov_num_age>=18)
flow_chart_n <- c(flow_chart_n, nrow(input))

## Adult: remove if age > 105 years
input <- input%>%filter(cov_num_age <=105)
flow_chart_n <- c(flow_chart_n, nrow(input))

## Ethnicity: remove if missing
input <- input%>%filter(!is.na(cov_cat_ethnicity))
flow_chart_n <- c(flow_chart_n, nrow(input))

# ##table(input$cov_cat_previous_covid)
# input <- input%>%filter(sub_cat_previous_covid == "No COVID code")
# flow_chart_n <- c(flow_chart_n, nrow(input))

## for individuals whose first long covid date is after follow-up end, set first long covid date as na
index <- which(input$out_first_long_covid_date > input$fup_end_date)
input$out_first_long_covid_date[index] <- NA

sum(!is.na(input$out_first_long_covid_date))

## previous long covid
cohort_start = as.Date("2020-12-01", format="%Y-%m-%d") # this is the same as the index date column - do you need both?
input <- input%>%filter(out_first_long_covid_date >= cohort_start |
                          is.na(out_first_long_covid_date))
flow_chart_n <- c(flow_chart_n, nrow(input))

## redefine age group
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_105", input$cov_cat_age_group)
input$cov_cat_age_group <- factor(input$cov_cat_age_group, ordered = TRUE)

################################################################################
#Part 2. Define follow-up end date and construct survival data for long covid  #
################################################################################

## study period: index date = "2020-12-01", end date = "2022-03-31"
cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
input$cohort_end_date = cohort_end

## To improve efficiency: keep only necessary variables
variables_to_keep <-c("patient_id", "out_first_long_covid_date", "vax_covid_date1",
                      "death_date", "cohort_end_date", "index_date")

input_select <- input%>% dplyr::select(all_of(variables_to_keep))

## Construct time to long COVID for analysis 1, analysis 2 and analysis 3-----------------

## Define survival data for analysis 1----------------------------------------------------
## lcovid_surv: days from index date to long COVID, without censoring by vaccination 
## lcovid_cens: indicator for long covid          

input_select <- input_select%>% rowwise() %>% mutate(fup_end_date=min(out_first_long_covid_date, 
                                                                       death_date, 
                                                                       cohort_end_date,
                                                                       na.rm = TRUE))
input_select <- input_select %>% filter(fup_end_date >= index_date & fup_end_date != Inf)
input_select$lcovid_surv<- as.numeric(input_select$fup_end_date - input_select$index_date)+1
# max_fup <- as.numeric(cohort_end - input_select$index_date[1])+1 
# input_select <- input_select %>% filter(lcovid_surv >= 0 & lcovid_surv<= max_fup) 
## define event indicator, without censoring for vaccination
input_select <- input_select %>% mutate(lcovid_cens = ifelse((out_first_long_covid_date <= fup_end_date & 
                                                     out_first_long_covid_date >= index_date &
                                                     !is.na(out_first_long_covid_date)), 1, 0))

## Define survival data for analysis 2-----------------------------------------------------
## lcovid_surv_vax_c: days from index date to long COVID, censored by vaccination 
## lcovid_cens_vax_c: indicator for long covid   
input_select <- input_select%>% rowwise() %>% mutate(fup_end_date_vax_c=min(out_first_long_covid_date, 
                                                                      vax_covid_date1,
                                                                      death_date, 
                                                                      cohort_end_date,
                                                                      na.rm = TRUE))
input_select$lcovid_surv_vax_c<- as.numeric(input_select$fup_end_date_vax_c - input_select$index_date)+1
input_select <- input_select %>% mutate(lcovid_cens_vax_c = ifelse((out_first_long_covid_date <= fup_end_date_vax_c & 
                                                             out_first_long_covid_date >= index_date &
                                                             !is.na(out_first_long_covid_date)), 1, 0))

## Define survival data for analysis 3-----------------------------------------------------
## input_vaccinated: data for population with at least 1st vaccination
## lcovid_surv: days from 1st vaccination to long COVID 
## lcovid_cens   : indicator for long covid 
input_vaccinated <- input_select%>% dplyr::select(all_of(variables_to_keep))
input_vaccinated <- input_vaccinated%>%filter(!is.na(vax_covid_date1) & 
                                     vax_covid_date1 >= index_date &
                                     vax_covid_date1 <= cohort_end_date &
                                     (death_date >= vax_covid_date1 |
                                          is.na(death_date)) &
                                     (out_first_long_covid_date >= vax_covid_date1 |
                                      is.na(out_first_long_covid_date)))

# input_vaccinated <- input_vaccinated%>%filter(!is.na(vax_covid_date1) & 
#                                                 vax_covid_date1 >= index_date &
#                                                 vax_covid_date1 <= cohort_end_date)

input_vaccinated <- input_vaccinated%>% rowwise() %>% mutate(fup_start_date = vax_covid_date1)
input_vaccinated <- input_vaccinated%>% rowwise() %>% mutate(fup_end_date = min(out_first_long_covid_date, 
                                                                            death_date, 
                                                                            cohort_end_date,
                                                                            na.rm = TRUE))
# lcovid_surv may be negative here - maybe you want to add some conditions to avoid this?
input_vaccinated <- input_vaccinated %>% mutate(lcovid_surv = as.numeric(fup_end_date - fup_start_date)+1,
                                                lcovid_cens = ifelse((out_first_long_covid_date <= fup_end_date & 
                                                                     out_first_long_covid_date >= fup_start_date &
                                                                     !is.na(out_first_long_covid_date)), 1, 0))

################################################################################
# Part 3. Create and output datasets                                           #
################################################################################
## Create data set for analysis 1 and analysis 2

variables_to_keep <-c("patient_id", "fup_end_date", "cohort_end_date",
                      "lcovid_surv", "lcovid_cens","lcovid_surv_vax_c", "lcovid_cens_vax_c",
                      "fup_end_date_vax_c")

# warnings if non-neg follow-up time
if (!all(input_select$lcovid_surv>=0)) warning("lcovid_surv should be  >= 0 in input_select")
if (!all(input_select$lcovid_cens>=0)) warning("lcovid_surv should be  >= 0 in input_select")

input_select <- input_select[,variables_to_keep]

## left join: keep all observations in input_select
input <- merge(x = input_select, y = input, by = "patient_id", all.x = TRUE)

rm(input_select)

## Data set for analysis 1 and analysis 2
## Time origin: index date; 
## fup end: long covid or death or end of cohort, with / without censoring by 1st vax
saveRDS(input, file = "output/input_stage1_all.rds")

print("Date set for analyses 1 and 2 created successfully!")

## create data set for analysis 3
variables_to_keep <-c("patient_id", "fup_end_date", "cohort_end_date", "fup_start_date",
                      "lcovid_surv", "lcovid_cens")

input_vaccinated <- input_vaccinated[,variables_to_keep]

# warnings if non-neg follow-up time
if (!all(input_vaccinated$lcovid_surv>=0)) warning("lcovid_surv should be  >= 0 in input_vaccinated")

## left join: keep all observations in input_select
input <- merge(x = input_vaccinated, y = input, by = "patient_id", all.x = TRUE)

rm(input_vaccinated)

## Data set for analysis 3: time origin: 1st vaccination, end fup: long covid or death or end of cohort
saveRDS(input, file = "output/input_stage1_vaccinated.rds")

print("Date set for analysis 3 created successfully!")

################################################################################
# Part 4. Flowchart output                                                     #
################################################################################

flow_chart<-data.frame(steps, flow_chart_n)
flow_chart$drop <- rep(0, nrow(flow_chart))
for(i in 2:nrow(flow_chart)){
  flow_chart$drop[i] = flow_chart$flow_chart_n[i-1] - flow_chart$flow_chart_n[i]
  if(flow_chart$drop[i]<=5 & flow_chart$drop[i]!=0){
    flow_chart$drop[i] = "redacted"
    flow_chart$flow_chart_n[i] = flow_chart$flow_chart_n[i-1]
  }
}

# function for small number suppression
# source("analysis/functions/redactor2.R")
# flow_chart$drop <- redactor2(flow_chart$drop)
# flow_chart[is.na(flow_chart$drop),2:3] = "[redacted]"

write.csv(flow_chart, file="output/flow_chart.csv", row.names = F)

rmarkdown::render("analysis/compiled_flow_chart_results.Rmd",output_file ="flow_chart", 
                  output_dir = "output")

print("Flowchart table is saved successfully!")