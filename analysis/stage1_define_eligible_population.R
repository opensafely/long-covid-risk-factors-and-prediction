# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: define eligible population
# Output:  input_stage1.rds

library(readr); library(dplyr); library(htmlTable)
input <- read_rds("output/input_stage0.rds")

population <- "unvaccianted"
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

# previous long covid
#cohort_start = as.Date("2020-12-01", format="%Y-%m-%d")
#index <- which(input$out_first_long_covid_date < cohort_start)

#table(input$cov_cat_previous_covid)
#input <- input%>%filter(cov_cat_previous_covid == " No COVID code")
#flow_chart_n <- c(flow_chart_n, nrow(input))

flow_chart<-data.frame(steps, flow_chart_n)

flow_chart$drop <- rep(0, nrow(flow_chart))
for(i in 2:nrow(flow_chart)){
  flow_chart$drop[i] = flow_chart$flow_chart_n[i-1] - flow_chart$flow_chart_n[i]
  if(flow_chart$drop[i]<=5 & flow_chart$drop[i]!=0){
    flow_chart$drop[i] = "redacted"
    flow_chart$flow_chart_n[i] = flow_chart$flow_chart_n[i-1]
  }
}

write.csv(flow_chart, file="output/flow_chart.csv", row.names = F)

rmarkdown::render("analysis/compiled_flow_chart_results.Rmd",output_file ="flow_chart", output_dir = "output")
                  #output_file=paste0("flow_chart_", population),output_dir="output")

#---------define follow-up end date---

## study period: index date = "2020-12-01", end date = "2022-03-31"
cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
input$cohort_end_date = cohort_end

#-------------------------------------------------------------------------------
## population = "unvaccinated"
## for unvaccianted population, the follow-up start date is the index date


## To improve efficiency: keep only necessary variables

variables_to_keep <-c("patient_id", "out_first_long_covid_date",
                      "death_date", "cohort_end_date", "index_date")

input_select <- input[,variables_to_keep]

## to increase efficiency
rm(input)

## specify follow-up end date ----------------------------------------------------------
input_select <- input_select%>% rowwise() %>% mutate(follow_up_end_date=min(out_first_long_covid_date, 
                                                               death_date, 
                                                               cohort_end_date,
                                                               na.rm = TRUE))

input_select <- input_select %>% filter(follow_up_end_date >= index_date & follow_up_end_date != Inf)

## define days since follow-up to long COVID diagnosis, vaccination censored long covid diagnosis
input_select$lcovid_surv_vax_c <- as.numeric(input_select$follow_up_end_date - input_select$index_date)

## define event indicator, vaccination censored long covid diagnosis
input_select <- input_select %>% mutate(lcovid_i_vax_c = ifelse((out_first_long_covid_date <= follow_up_end_date & 
                                                     out_first_long_covid_date >= index_date &
                                                     !is.na(out_first_long_covid_date)), 1, 0))

variables_to_keep <-c("patient_id", "follow_up_end_date", "cohort_end_date",
                      "lcovid_surv_vax_c", "lcovid_i_vax_c")

input_select <- input_select[,variables_to_keep]

input <- read_rds("output/input_stage0.rds")

# left join: keep all observations in input_select
input <- merge(x = input_select, y = input, by = "patient_id", all.x = TRUE)

rm(input_select)

# ## specify follow-up end date ----------------------------------------------------------
# input <- input %>% rowwise() %>% mutate(follow_up_end_date=min(out_first_long_covid_date, 
#                                                              death_date, 
#                                                              cohort_end_date,
#                                                              na.rm = TRUE))
# 
# input <- input %>% filter(follow_up_end_date >= index_date & follow_up_end_date != Inf)
# 
# ## define days since follow-up to long COVID diagnosis, vaccination censored long covid diagnosis
# input$lcovid_surv_vax_c <- as.numeric(input$follow_up_end_date - input$index_date)
# 
# ## define event indicator, vaccination censored long covid diagnosis
# input <- input %>% mutate(lcovid_i_vax_c = ifelse((out_first_long_covid_date <= follow_up_end_date & 
#                                                    out_first_long_covid_date >= index_date &
#                                                    !is.na(out_first_long_covid_date)), 1, 0))

saveRDS(input, file = "output/input_stage1.rds")
