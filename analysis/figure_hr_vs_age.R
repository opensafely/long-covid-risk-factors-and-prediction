## Programmed by Yinghui Wei
## Content: plot of long hazard ratio against continuous age

library(readr); library(dplyr); library(rms); library(MASS)
fs::dir_create(here::here("output", "review", "model"))

source("analysis/stage2_model_input_set_up.R")
source("analysis/functions/function_HR_vs_age.R")
splines_model <- read_rds(paste0("output/not_for_review/model/fit_cox_model_splines_", analysis, ".rds"))

write.csv(table(input$cov_num_age),file=paste0("output/review/model/support_hr_vs_age_table_age_", analysis, ".csv"))

function_figure_hr_vs_age(input, splines_model, analysis)
