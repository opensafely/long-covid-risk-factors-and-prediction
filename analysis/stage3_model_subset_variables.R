# Purpose: Run Cox model on the subset variables
# Programmed by Yinghui Wei
# Date: 2022-08-13
# YW comment: ? Perhaps best to include an argument in model input set up to separate original development
#          and the model with only the subset variables

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)

fs::dir_create(here::here("output", "not_for_review", "model"))
fs::dir_create(here::here("output", "review", "model"))

source("analysis/stage2_model_input_set_up.R") # calling this script to determine the analysis and to read in input data
source("analysis/functions/function_cox_output.R")
source("analysis/functions/function_model_evaluation.R")
print("source files successfully!")


source("analysis/functions/function_df_summary.R")
args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  #analysis <- "all"          # all eligible population
  #analysis <- "all_vax_c"        # all eligible population but censored them by the 1st vaccination
  #analysis <- "vaccinated"   # vaccinated population
  #analysis <- "all_vax_td"    # vaccination status is included as a time-dependent covariate
  analysis <- "infected"
}else{
  analysis <- args[[1]]
}

ratio_non_cases_to_cases = 20 # this is used in sampling non-cases to increase efficiency without loss of information
set.seed(123456) # to ensure reproducibility in the sampling

################################################################################
# Part 1: Read in the subset variables                                         #
################################################################################

selected_vars <- read.csv("output/review/model/selected_vars.csv", header=T)
selected_vars <- as.vector(selected_vars$vars_names)

## identify is age is selected
index <- which(selected_vars == "cov_num_age")
selected_vars <- selected_vars[! selected_vars %in% "cov_num_age"] # remove continuous age and it will be included as splines later
selected_vars
################################################################################
# Part 2: Read in input data, define samples and weighting                     #
################################################################################
#source("analysis/stage3_read_and_define_input_data.R")

################################################################################
# Part 2: Specify survival formula, and fit Cox models                         #
################################################################################
len = length(selected_vars)
if(analysis == "infected" | analysis == "all_vax_td"){
  len = len + 1
}
if(len!=0){
  surv_formula <- paste0(
    "Surv(lcovid_surv, lcovid_cens) ~ ",
    paste(selected_vars, collapse = "+")
  )
  # include age into the model is age is a selected variable
  if(length(index)!=0){
    surv_formula <- paste0(surv_formula, "+ rms::rcs(cov_num_age,parms=knot_placement)") 
    #surv_formula_linear <- paste0(surv_formula, "+ cov_num_age") 
  }
  if(analysis == "infected"){
    surv_formula <- paste0(surv_formula, " + cov_cat_covid_phenotype")
  }
  if(analysis == "all_vax_td"){
    surv_formula <- paste0(surv_formula, " + cov_cat_ie.status")
  }
  print(surv_formula)
  #RK - where index3 is defined has been commented out? Do you still need this?
  # YW - I have now uncomment index3 and related code. This script currently can only run locally 
  # and some how it doesn't work on yaml， error message:
  # In addition: Warning message:
  #In file(file, "rt") :
  #  cannot open file 'output/not_for_review/model/selected_variables_all.csv': No such file or directory
  # YW 2020/08/15 - this error message is now resolved by calling stage_1_ action as a need in the yaml
  
  fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                           data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  
  print("The fitted model is")
  print(fit_cox_model)
  
  ################################################################################
  # Part 3: Output results from the Cox Model                                    #
  ################################################################################
  which_model = "subset_vars"
  output_file = paste0("output/review/model/HR_",which_model,"_",  analysis)
  results <- cox_output2(fit_cox_model, which_model, output_file, save_output=TRUE)
  
  ################################################################################
  # Part 4: Model evaluation                                                     #
  ################################################################################
  subset_vars = "subset_vars"
  which_model = ""
  results_pm <- function_model_evaluation(input,fit_cox_model, which_model, analysis, subset_vars, graphics_output=FALSE, save_output=TRUE)
  print("Finished model valuation for subset variables model!")
}else{
  # Output file otherwise return error in opensafely run
  results <- "None"
  which_model = "subset_vars"
  output_file = paste0("output/review/model/HR_",which_model,"_",  analysis)
  write.csv(results, file = output_file)
  #Output file otherwise return error in opensafely run
  results.pm <- "None"
  subset_vars = "subset_vars"
  which_model = ""
  output_file=paste0("output/review/model/PM_", subset_vars,which_model, "_", analysis, ".csv")
  write.csv(results.pm, file = output_file)
  print("No variable is selected")
}
