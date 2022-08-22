## Purpose: to identify a subset of variables to be included in the model development
##          variables which were never selected by any analyses were left out
## Programmed by Yinghui Wei
## Date: 2022-07-26

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)

fs::dir_create(here::here("output", "not_for_review", "model"))
fs::dir_create(here::here("output", "review", "model"))

################################################################################
## Part 1: load data, define inverse probability weighting                    ##
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
## - Start: add for testing purpose - remove if run on real data
## selected_vars <-rbind(selected_vars,"cov_cat_sex")
## selected_vars <- rbind(selected_vars,"cov_num_age")
## selected_vars <- rbind(selected_vars, "cov_cat_bmi")
## - End: add for testing purpose
names(selected_vars) <-"vars_names"
selected_vars <- selected_vars %>%filter(!vars_names %in% c("None","cov_cat_ie.status", "cov_cat_covid_phenotype"))
selected_vars

print("Define selected variables successfully!")

write.csv(selected_vars, "output/review/model/selected_vars.csv", 
          row.names = F)

