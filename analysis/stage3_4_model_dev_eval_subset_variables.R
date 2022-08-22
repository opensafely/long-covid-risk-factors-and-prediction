# Purpose: to identify a subset of variables to be included in the model development
#          to run Cox model for model development
# Programmed by Yinghui Wei
# Date: 2022-07-26

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)

fs::dir_create(here::here("output", "not_for_review", "model"))
fs::dir_create(here::here("output", "review", "model"))

source("analysis/stage2_model_input_set_up.R")
source("analysis/functions/function_cox_output.R")
source("analysis/functions/function_model_evaluation.R")
print("source files successfully!")
################################################################################
# Part 1: load data, define inverse probability weighting                      #
################################################################################
results_dir <- "output/not_for_review/model/"
output_dir <- "output/not_for_review/model/"
 file_list=list.files(path = results_dir, pattern = "selected_variables_*")
 file_list = c("selected_variables_all.csv","selected_variables_all_vax_c.csv",
               "selected_variables_all_vax_td.csv", "selected_variables_infected.csv",
               "selected_variables_vaccinated.csv")
 print("setting path successfully!")
 for (i in 1:length(file_list)){
   assign(file_list[i],
          read.csv(paste(results_dir, file_list[i], sep=''))
   )
 }

 selected_variables_all.csv <- read.csv(paste(results_dir, "selected_variables_all.csv", sep=''))
 selected_variables_all_vax_c.csv <- read.csv(paste(results_dir, "selected_variables_all_vax_c.csv", sep=''))
 selected_variables_all_vax_td.csv <- read.csv(paste(results_dir, "selected_variables_all_vax_td.csv", sep=''))
 selected_variables_infected.csv <- read.csv(paste(results_dir, "selected_variables_infected.csv", sep=''))
 selected_variables_vaccinated.csv <- read.csv(paste(results_dir, "selected_variables_vaccinated.csv", sep=''))
 print("read in data successfully!")
 df_list <- list(selected_variables_all.csv,
                 selected_variables_all_vax_c.csv,
                 selected_variables_all_vax_td.csv,
                 selected_variables_infected.csv,
                 selected_variables_vaccinated.csv)

 print("Load data successfully!")

selected_vars <- as.vector(unique(rbind(df_list[[1]], df_list[[2]], df_list[[3]], df_list[[4]], df_list[[5]])))

subset_vars <- subset_vars[!subset_vars %in% c("cov_cat_ie.status", "cov_cat_covid_phenotype")]
index1<-which(selected_vars == "cov_cat_ie.status")
index2<-which(selected_vars == "cov_cat_covid_phenotype")
index3 <- which(selected_vars == "cov_num_age")
selected_vars <- selected_vars[-c(index1,index2, index3),] # remove continuous age and it will be included as splines later

selected_vars <- c(cov_factor_names, cov_num_names)

print("Define selected variables successfully!")

################################################################################
# Part 2: Specify survival formula, and fit Cox models                         #
################################################################################
surv_formula <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  paste(selected_vars, collapse = "+")
)

if(length(index3)!=0){
  surv_formula <- paste0(surv_formula, "+ rms::rcs(cov_num_age,parms=knot_placement)") 
  #surv_formula_linear <- paste0(surv_formula, "+ cov_num_age") 
}

fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

print("The fitted model is")
print(fit_cox_model)

################################################################################
# Part 3: Output results from the Cox Model                                    #
################################################################################
which_model = "selected_vars"
output_file = paste0("output/review/model/HR_",which_model,"_",  analysis)
results <- cox_output2(fit_cox_model, which_model, output_file, save_output=TRUE)

################################################################################
# Part 4: Model evaluation                                                     #
################################################################################
subset_vars = "selected_vars"
which_model = ""
results_pm <- function_model_evaluation(input,fit_cox_model, which_model, analysis, subset_vars, graphics_output=FALSE, save_output=TRUE)
print("Finished model valuation for subset variables model!")
