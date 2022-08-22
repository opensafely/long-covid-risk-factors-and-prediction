# Purpose: Long COVID risk factors and prediction models
# Content: Cox model: model evaluation
# Output:  Performance measures

library(readr); library(dplyr); library(rms); library(MASS)
library(rms); library(fastDummies); library(pseudo)
# library(survcomp) ## not yet available in opensafely

################################################################################
# Part 1: load fitted model                                                    #
################################################################################
# load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_selection.R")

print("Starting stage_4_model_evaluation.R")

fs::dir_create(here::here("output", "review", "descriptives"))
fs::dir_create(here::here("output", "review", "model"))

#the full model is already loaded in stage3_model_input_set_up, so no need to refit

print("Part 1. Finished loading fitted cox model!")

################################################################################
# Part 2: Output apparent model evaluation - C and calibration slope           #
################################################################################
# Evaluation conducted for both the full model and (if any) the selected model
# source file for model evaluation
source("analysis/functions/function_model_evaluation.R")
subset_vars =""
if(which_model == "selected"){
  print(fit_cox_model_selected)
  fit_cox_model_selected_refit <-rms::cph(formula= as.formula(surv_formula_selected),
                             data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  function_model_evaluation(input,fit_cox_model_selected_refit, which_model, analysis, 
                            subset_vars, graphics_output = TRUE, save_output = TRUE)
  print("Fished model evaluation for the selected model!")
}

print(fit_cox_model_selected_refit)
print(fit_cox_model_selected)

## fit_cox_model is the full model
which_model = "full" # reset the model to full, evaluate full model
print(fit_cox_model)

function_model_evaluation(input,fit_cox_model, which_model, analysis, subset_vars, 
                          graphics_output = TRUE, save_output = TRUE)
print("Finished model valuation for the full model!")
