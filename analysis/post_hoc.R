# Purpose: to produce a contingency table to explore the relationship between gp_patient_interaction and smoking status
# Programme by Yinghui Wei
library(readr); library(dplyr); library(rms); library(MASS)
fs::dir_create(here::here("output", "review", "descriptives"))
fs::dir_create(here::here("output", "not_for_review", "descriptives"))
source("analysis/functions/function_df_summary.R")
args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  analysis <- "all"          # all eligible population
  #analysis <- "all_vax_c"        # all eligible population but censored them by the 1st vaccination
  #analysis <- "vaccinated"   # vaccinated population
  #analysis <- "all_vax_td"    # vaccination status is included as a time-dependent covariate
  #analysis <- "infected"
}else{
  analysis <- args[[1]]
}

## Analyses 1 and 4: all eligible patients, without censoring individuals by vaccination
if(analysis == "all"){
  input <- read_rds("output/input_stage1_all.rds")
}

if(analysis == "all_vax_td"){
  input <- read_rds("output/input_stage1_all.rds")
}

## Analysis 2: all eligible patients, but censoring individuals by vaccination
if(analysis == "all_vax_c"){
  input <- read_rds("output/input_stage1_all.rds")
  input <- input %>% dplyr::select(-lcovid_surv, -lcovid_cens) %>%
    dplyr::rename(lcovid_surv = lcovid_surv_vax_c, lcovid_cens = lcovid_cens_vax_c)
}
## Analysis 3: time origin is the first vaccination 
if(analysis == "vaccinated"){
  input <- read_rds("output/input_stage1_vaccinated.rds")
}

## Analysis 6: time origin is the first covid infection
if(analysis == "infected"){
  input <- read_rds("output/input_stage1_infected.rds")
}

tbl_gp_interaction_smoking <- table(input$cov_cat_gp_patient_interaction, input$cov_cat_smoking_status)

results <- chisq.test(tbl_gp_interaction_smoking)

tbl_results <- data.frame(round(results$statistic,2), results$parameter,
                     round(results$p.value,2), results$method)

colnames(tbl_results) <- c("test_statistic", "df", "P-value", "method")

write.csv(tbl_results, file=paste0("output/review/descriptives/table_gp_smoking_", 
                                 analysis, "_chi_square_test.csv"), row.names = F )

write.csv(tbl_gp_interaction_smoking, file=paste0("output/not_for_review/descriptives/table_gp_smoking_", 
                                                   analysis, ".csv"), row.names = F)
