# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: input set up, define survival formula through variables selection and AIC
# Output:  survival formula for selected model

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available
fs::dir_create(here::here("output", "not_for_review", "model"))
fs::dir_create(here::here("output", "review", "model"))
source("analysis/functions/function_cox_output.R")
source("analysis/functions/function_df_summary.R")
analysis = "fatigue_all"
ratio_non_cases_to_cases = 20 # this is used in sampling non-cases to increase efficiency without loss of information
set.seed(123456) # to ensure reproducibility in the sampling

################################################################################
# Part 1: load data, define inverse probability weighting                      #
################################################################################
input <- read_rds("output/input_stage1_all.rds")

##--specify inverse probability weighting
cases <- input %>% filter(fatigue_cens==1)

non_cases <- input %>% filter(!patient_id %in% cases$patient_id)

if(nrow(cases)*ratio_non_cases_to_cases < nrow(non_cases)){
  non_cases <- non_cases[sample(1:nrow(non_cases), nrow(cases)*ratio_non_cases_to_cases,replace=FALSE), ]
}else if (nrow(cases)*ratio_non_cases_to_cases >= nrow(non_cases)){
  non_cases=non_cases
}
print(paste0("Number of cases: ", nrow(cases)))
print(paste0("Number of controls: ", nrow(non_cases)))
non_case_inverse_weight=(nrow(input)-nrow(cases))/nrow(non_cases)
## recreate input after sampling
input <- bind_rows(cases,non_cases)
## Add inverse probability weights for non-cases
noncase_ids <- unique(non_cases$patient_id)
input$weight <-1
input$weight <- ifelse(input$patient_id %in% noncase_ids,
                       non_case_inverse_weight, 1)

## remove region as a covariate
input <- rename(input, sub_cat_region = "cov_cat_region")

## extract candidate predictors
covariate_names <- names(input)[grep("cov_", names(input))]

## remove categorical and continuous age, as continuous age will be added
## later as with a spline function
covariate_names <- covariate_names[-grep("age", covariate_names)]

print("candidate predictors")
print(covariate_names)
## for computational efficiency, only keep the variables needed in fitting the model
variables_to_keep <- c("patient_id", "practice_id",
                       "fatigue_surv", "fatigue_cens", covariate_names,
                       "cov_num_age", "weight", "sub_cat_region")


input <- input %>% dplyr::select(all_of(variables_to_keep))
print("Part 1: load data, define inverse probability weighting is completed!")

################################################################################
# Part 2: number of people in each covariate level                             #
################################################################################
function_df_summary(input, analysis)
## set up before using rms::cph
dd <<- datadist(input) #
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment")) #

################################################################################
# Part 3: define survival analysis formula                                     #
################################################################################
## linear predictors + a restricted cubic spline for age + 
##  a restricted cubic spline for gp consultation rate + clustering effect for practice
knot_placement=as.numeric(quantile(input$cov_num_age, probs=c(0.1,0.5,0.9)))

## Age sex model
surv_formula_age_spl_sex <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  "cov_cat_sex",
  "+rms::rcs(cov_num_age,parms=knot_placement)"
  # "+ strat(sub_cat_region)"
)

## Age sex model
surv_formula_age_linear_sex <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  "cov_cat_sex", "+ cov_num_age"
)

## Full model
surv_formula <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)"
)

## full model: age is added as a linear predictor
surv_formula_lp <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+ cov_num_age"
)

print("Part 3: define survival analysis formula is completed!")

print("End of stage3_model_input_setup.R")

################################################################################
# Part 4: Fit Cox model with post-viral fatigue as an outcome                  #
################################################################################
# age sex model

fit_age_sex_cox_model_splines <-rms::cph(formula= as.formula(surv_formula_age_spl_sex),
                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
which_model = "fatigue_age_sex_splines"
output_file = paste0("output/review/model/HR_", which_model, "_", analysis)
cox_output2(fit_age_sex_cox_model_splines, which_model, output_file, save_output = TRUE)

fit_age_sex_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_age_linear_sex),
                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
which_model = "fatigue_age_sex_linear"
output_file = paste0("output/review/model/HR_", which_model, "_", analysis)
cox_output2(fit_age_sex_cox_model_linear, which_model, output_file, save_output = TRUE)

# full model
fit_full_cox_model_splines <-rms::cph(formula= as.formula(surv_formula),
                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

which_model = "fatigue_full_splines"
output_file = paste0("output/review/model/HR_", which_model, "_", analysis)
cox_output2(fit_full_cox_model_splines, which_model, output_file, save_output = TRUE)

fit_full_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_lp),
                                data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
which_model = "fatigue_full_linear"
output_file = paste0("output/review/model/HR_", which_model, "_", analysis)
cox_output2(fit_full_cox_model_linear, which_model, output_file, save_output = TRUE)

# model selection

fit_cox_model_selected <- fastbw(fit_full_cox_model_splines, sls=0.20)

which_model = "fatigue_selected_splines"
output_file = paste0("output/review/model/HR_", which_model, "_", analysis)
cox_output2(fit_full_cox_model_linear, which_model, output_file, save_output = TRUE)