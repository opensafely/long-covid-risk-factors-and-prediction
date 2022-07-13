# Purpose: Long COVID risk factors and prediction models
# Content: Cox model: model evaluation
# Output:  One CSV file, One HTML file, for performance measures

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available in opensafely

################################################################################
# Part 1: load fitted model                                                    #
################################################################################
# load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_selection.R")

print("Starting stage_4_model_evaluation.R")

fs::dir_create(here::here("output", "review", "model"))

if(which_model == "selected"){
  # loading the selected model as fit_cox_model_vs from backward elimination is not a standard Cox model object
  fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                           data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
}
# the full model is already loaded in stage3_model_input_set_up, so no need to refit

print("Part 1. Finished loading fitted cox model!")

# source file for model evaluation
source("analysis/functions/function_model_evaluation.R")
subset_vars =""
function_model_evaluation(input,fit_cox_model, which_model, analysis, subset_vars, graphics_output = TRUE, save_output = TRUE)
print("Finished model valuation")