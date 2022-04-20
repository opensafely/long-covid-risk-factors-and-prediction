# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: define eligible population
# Output:  input_stage1.rds

library(readr); library(dplyr); library(htmlTable)
input <- read_rds("output/input_stage0.rds")

population <- "unvaccianted"

# Define eligible population--------------------------------------------
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
#input <- input%>%filter(cov_cat_previous_covid == "No COVID code")
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

rmarkdown::render("analysis/compiled_flow_chart_results.Rmd",output_file ="flow_chart", 
                  output_dir = "output")

#---------define follow-up end date---------------------------------------------

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
input_select$lcovid_surv_vax_c <- as.numeric(input_select$follow_up_end_date - input_select$index_date)+1

input_select <- input_select %>% filter(lcovid_surv_vax_c >= 1 & lcovid_surv_vax_c<= 486)

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

## for individuals whose first long covid date is after follow-up end, set first long covid date as na
index <- which(input$out_first_long_covid_date > input$follow_up_end_date)
input$out_first_long_covid_date[index] <- NA

sum(!is.na(input$out_first_long_covid_date))


#----------define multimorbidity: need to be carefully checked -------------------

condition_names <- names(input)[grepl("cov_cat", names(input))]
not_a_condition <- c("cov_cat_age_group", "cov_cat_sex","cov_cat_healthcare_worker",
                     "cov_cat_imd","cov_cat_region","cov_cat_smoking_status",
                     "cov_cat_covid_phenotype", "cov_cat_previous_covid",
                     "cov_cat_ethnicity")

condition_names <- condition_names[!condition_names%in%not_a_condition]

input_select <- input %>% select(patient_id, condition_names)
input_select<- as_tibble(
  data.matrix(input_select)
        )

# asthma is defined in a different way
condition_names <- condition_names[-grep("cov_cat_asthma", condition_names)]

for(i in condition_names){
  input_select[, i] =ifelse(input_select[,i]==1, 0, 1)  # greater than 
}

input_select$cov_cat_asthma <- ifelse(input_select$cov_cat_asthma == 1, 1,0)

input_select <- input_select %>% mutate(cov_num_multimorbidity = rowSums(.[ , condition_names]))

input_select <- input_select %>% mutate(cov_cat_multimorbidity =ifelse(cov_num_multimorbidity>1, 1,0))

input_select <- input_select %>% select(c(patient_id, cov_cat_multimorbidity))

#-----------------end of definition for multimorbidity -------------------------

# left join: keep all observations in input_select
input <- merge(x = input_select, y = input, by = "patient_id", all.x = TRUE)

rm(input_select)

#View(input_select)

# redefine age group
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_105", input$cov_cat_age_group)
input$cov_cat_age_group <- factor(input$cov_cat_age_group, ordered = TRUE)


saveRDS(input, file = "output/input_stage1.rds")
