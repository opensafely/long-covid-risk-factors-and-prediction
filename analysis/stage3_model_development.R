# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: model development and some evaluation
# Output:  hazard ratios and 95% CI
#          two CSV files, Two HTML files, and TWO SVG files
# to do 1. output standard errors of c statistics
#       2. output calibration slope, se and 95% CI

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available

####################################################################################################
# Part 1: load data, define inverse probability weighting, fit cox model and assess PH assumption  #
#         variable selection using backward elimination                                            #
####################################################################################################

# # load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_selection.R")
source("analysis/functions/function_cox_output.R")

print("Starting stage3_model_development.R")

if(which_model == "selected"){
  fit_cox_model_selected <-rms::cph(formula= as.formula(surv_formula),
                                     data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  print("The selected model is")
  print(fit_cox_model_selected)
}

#names(fit_cox_model)
print("The full model is")
print(fit_cox_model)
output_file = paste0("output/review/model/hazard_ratio_estimates_", "full", "_", analysis)
cox_output2(fit_cox_model, "full",output_file, save_output=TRUE)
if(which_model == "selected"){
  output_file = paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis)
  cox_output2(fit_cox_model_selected, "selected", output_file, save_output = TRUE)
}
print("Finished stage3_model_development.R")

