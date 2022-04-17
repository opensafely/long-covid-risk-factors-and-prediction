# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: model development and internal evaluation
# Output:  hazard ratios and 95% CI
#          two CSV files, Two HTML files, and TWO SVG files

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available

####################################################################################################
# Part 1: load data, define inverse probability weighting, fit cox model and assess PH assumption  #
#         variable selection using backward elimination                                            #
####################################################################################################

input <- read_rds("output/input_stage1.rds")

cases <- input %>% filter(!is.na(out_first_long_covid_date) & 
                                  (out_first_long_covid_date == follow_up_end_date))

non_cases <- input %>% filter(!patient_id %in% cases$patient_id)

## sample non_cases, size = 5*nrow(cases) if 5*nrow(cases) < nrow(cases)
if(nrow(cases)*5 < nrow(non_cases)){
  non_cases <- non_cases[sample(1:nrow(non_cases), nrow(cases)*5,replace=FALSE), ]
}else if (nrow(cases)*5 >= nrow(non_cases)){
  non_cases=non_cases
}

print(paste0("Number of cases: ", nrow(cases)))
print(paste0("Number of controls: ", nrow(non_cases)))

non_case_inverse_weight=(nrow(input)-nrow(cases))/nrow(non_cases)

## recreate input after sampling
input <- bind_rows(cases,non_cases)

## extract candidate predictors
covariate_names <- names(input)[grep("cov_", names(input))]

## remove categorical and continuous age
covariate_names <- covariate_names[-grep("age", covariate_names)]

## remove previous covid history as a covariate
covariate_names <- covariate_names[-grep("cov_cat_previous_covid", covariate_names)]

## remove cov_cat_healthcare_worker
covariate_names <- covariate_names[-grep("cov_cat_healthcare_worker", covariate_names)]


print("candidate predictors")
covariate_names

## Add inverse probability weights for non-cases
noncase_ids <- unique(non_cases$patient_id)
input$weight <-1
input$weight <- ifelse(input$patient_id %in% noncase_ids,
                                    non_case_inverse_weight, 1)
## for computational efficiency, only keep the variables needed in fitting the model
variables_to_keep <- c("patient_id", "practice_id",
                       "lcovid_surv_vax_c", "lcovid_i_vax_c", covariate_names,
                       "cov_num_age", "weight")

input <- input %>% dplyr::select(all_of(variables_to_keep))

knot_placement=as.numeric(quantile(input$cov_num_age, probs=c(0.1,0.5,0.9)))
surv_formula <- paste0(
  "Surv(lcovid_surv_vax_c, lcovid_i_vax_c) ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ cluster(practice_id)"
)

surv_formula_predictors <- paste0(
  " ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ cluster(practice_id)"
)
print(paste0("survival formula: ", surv_formula))

dd <<- datadist(input)
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment"))


