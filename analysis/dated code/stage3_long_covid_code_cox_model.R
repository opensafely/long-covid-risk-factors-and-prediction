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

outcome = "long_covid"
################################################################################
#  Specify analysis cohort                                                     #
################################################################################
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  #analysis <- "all"          # all eligible population
  analysis <- "all_vax_c"        # all eligible population but censored them by the 1st vaccination
  #analysis <- "vaccinated"   # vaccinated population
  #analysis <- "all_vax_td"    # vaccination status is included as a time-dependent covariate
  #analysis <- "infected"
}else{
  analysis <- args[[1]]
}

################################################################################
# Part 1: load data, define inverse probability weighting                      #
################################################################################
input <- read_rds(paste0("output/input_stage2_", analysis, ".rds"))
covariate_names <- names(input)[grep("cov_", names(input))]

covariate_names <- covariate_names[-grep("age", covariate_names)]

print("candidate predictors")
print(covariate_names)

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

## Age sex model, age as sline
surv_formula_age_spl_sex <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  "cov_cat_sex",
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ strat(sub_cat_region)"
)

## Age sex model, age linear
surv_formula_age_linear_sex <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  "cov_cat_sex", "+ cov_num_age", "+ strat(sub_cat_region)"
)

## Full model, age spline
surv_formula <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ strat(sub_cat_region)"
)

## full model: age linear
surv_formula_lp <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+ cov_num_age", 
  "+ strat(sub_cat_region)"
)

print("Part 3: define survival analysis formula is completed!")

################################################################################
# Part 4: Fit Cox model with long covid code as an outcome                  #
################################################################################
# age sex model, age splines

fit_age_sex_cox_model_splines <-rms::cph(formula= as.formula(surv_formula_age_spl_sex),
                                         data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
which_model = paste0(outcome, "_age_sex_splines")
output_file = paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis)
cox_output2(fit_age_sex_cox_model_splines, which_model, output_file, save_output = TRUE)

print("Part 4: age sex Cox model - age splines, completed successfully!")

# age sex cox model, age linear
fit_age_sex_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_age_linear_sex),
                                        data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
which_model = paste0(outcome, "_age_sex_linear")
output_file = paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis)
cox_output2(fit_age_sex_cox_model_linear, which_model, output_file, save_output = TRUE)

print("Part 4: age sex Cox model - age linear, completed successfully!")

# full model, age splines
fit_full_cox_model_splines <-rms::cph(formula= as.formula(surv_formula),
                                      data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

which_model = paste0(outcome, "_full_splines")
output_file = paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis)
cox_output2(fit_full_cox_model_splines, which_model, output_file, save_output = TRUE)

print("Part 4: full Cox model - age splines, completed successfully!")

# full model, age linear
fit_full_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_lp),
                                     data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
which_model = paste0(outcome, "_full_linear")
output_file = paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis)
cox_output2(fit_full_cox_model_linear, which_model, output_file, save_output = TRUE)

print("Part 4: full Cox model - age linear, completed successfully!")
# model selection

fit_cox_model_selected <- fastbw(fit_full_cox_model_splines, sls=0.20)

which_model = paste0(outcome, "_selected_splines")
output_file = paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis)
cox_output2(fit_full_cox_model_linear, which_model, output_file, save_output = TRUE)

print("selected model:")
fit_cox_model_selected$names.kept

selected_covariate_names <- fit_cox_model_selected$names.kept

print("Part 4: model selection, completed successfully!")

print("Part 4: All Cox models completed successfully!")

################################################################################
# Part 5: Output apparent model evaluation - C and calibration slope           #
################################################################################
# source file for model evaluation
source("analysis/functions/function_model_evaluation.R")
subset_vars =""
#which_model = "full" # reset the model to full, evaluate full model

function_model_evaluation(input,fit_age_sex_cox_model_splines, "age_sex_splines", 
                          analysis, subset_vars, graphics_output = TRUE, save_output = TRUE)

function_model_evaluation(input,fit_age_sex_cox_model_linear, "age_sex_linear", 
                          analysis, subset_vars, graphics_output = TRUE, save_output = TRUE)

function_model_evaluation(input,fit_full_cox_model_splines, "full_model_splines", 
                          analysis, subset_vars, graphics_output = TRUE, save_output = TRUE)

function_model_evaluation(input,fit_full_cox_model_linear, "full_model_linear", 
                          analysis, subset_vars, graphics_output = TRUE, save_output = TRUE)

if(length(selected_covariate_names)>0){
  if("cov_num_age" %in% selected_covariate_names & grepl("rms::rcs", surv_formula) == TRUE){
    selected_covariate_names <- selected_covariate_names[-grep("age", selected_covariate_names)]
    surv_formula_selected <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+rms::rcs(cov_num_age,parms=knot_placement)", 
      "+ strat(sub_cat_region)"
    )
  }
  if("cov_num_age" %in% selected_covariate_names & grepl("rms::rcs", surv_formula) == FALSE){
    selected_covariate_names <- selected_covariate_names[-grep("age", selected_covariate_names)]
    surv_formula_selected <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+ cov_num_age", 
      "+ strat(sub_cat_region)"
    )
  }
  if(!("cov_num_age" %in% selected_covariate_names)){
    surv_formula_selected <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+ strat(sub_cat_region)"
    )
  }
  print("Selected models: survival formula is")
  print(surv_formula_selected)
}

#function_model_evaluation(input,fit_cox_model_selected, "selected_model", 
#                          analysis, subset_vars, graphics_output = TRUE, save_output = TRUE)

print("Finished model valuation")

print("Part 5: output model evaluation completed successfully!")