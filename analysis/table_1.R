# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 1. Baseline characteristics of patients. 
#          Categorical variables: number and percentage
#          Continuous variables:  number and percentage of observations, 
#                                 mean and standard deviation
# Output:  table1.csv, table1.html

library(readr); library(dplyr); library(lubridate)
# library(scales): not available in opensafely yet
fs::dir_create(here::here("output", "review", "descriptives"))

# function for small number suppression
source("analysis/functions/redactor2.R")

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  #cohort <- "all"          # all eligible population
  #cohort <- "vaccinated"   # vaccinated population
  cohort <- "infected"      # infected population
}else{
  cohort <- args[[1]]
}

table1_creation <- function(cohort){
  # Read in data and identify factor variables and numerical variables------------
  input <- read_rds(paste0("output/input_stage1_", cohort,".rds"))
  cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
  sub_factor_names <- names(input)[grepl("sub_cat", names(input))]
  cov_factor_names <- c(cov_factor_names, sub_factor_names)   # to include sub_cat_covid_phenotype
  cov_num_names <- names(input)[grepl("cov_num", names(input))]
  sub_num_names <- names(input)[grepl("sub_num", names(input))]
  # to include sub_num_gp_consultation which stores the original values before truncation
  cov_num_names <- c(cov_num_names, sub_num_names) 
  
  # summary table for practice id - for exploration only
  print("summary statistics for practice id")
  dtable <- table(input$practice_id)
  print(table(input$practice_id))
  print("number of practice id")
  print(nrow(dtable))
  print("number of missing practice id")
  print(length(which(is.na(input$practice_id))))
  
  # Create an empty data frame ---------------------------------------------------
  table_1 <- data.frame(variable = character(),
                       number  = numeric(),
                       percent = numeric(),
                       mean    = numeric(),
                       sd      = numeric(), 
                       inter_quartile_range     = numeric(),
                       stringsAsFactors = FALSE)
  
  # factor variables: number and percentage---------------------------------------
  input_factor_vars <- input[, cov_factor_names]
  for(i in 1:length(cov_factor_names)){
    levels = names(table(input_factor_vars[,i]))
    start = nrow(table_1)+1
    table_1[start,1] = cov_factor_names[i]
    start = nrow(table_1)+1
    end = nrow(table_1)+length(levels)
    table_1[start:end,1] <- c(levels)            # variable name
    table_1[start:end,2] <- c(table(input_factor_vars[,i]))  # number
    table_1[start:end,3] <- 100*round(c(table(input_factor_vars[,i]))/nrow(input_factor_vars),4)  # percentage
    print(levels)
  }
  
  # numerical variables: number and percentage of observations, mean and standard deviations
  input_num_vars <- input[,cov_num_names]
  if(length(cov_num_names) == 1){
    index = nrow(table_1)+1
    table_1[index,1] <- cov_num_names
    table_1[index,2] <- length(which(!is.na(unlist(input_num_vars)))) # number of observations
    table_1[index,4] <- round(mean(unlist(input_num_vars)),2) # mean
    table_1[index,5] <- round(sd(unlist(input_num_vars)),2) # sd
    table_1[index,6] <- round(IQR(unlist(input_num_vars)),2)  # IQR
  }
  if(length(cov_num_names)>1){
    for(i in 1:length(cov_num_names)){
      index = nrow(table_1)+1
      table_1[index,1] <- cov_num_names[i]
      table_1[index,2] <- length(which(!is.na(unlist(input_num_vars[,i])))) # number of observations
      table_1[index,4] <- round(mean(unlist(input_num_vars[,i])),2) # mean
      table_1[index,5] <- round(sd(unlist(input_num_vars[,i])),2) # sd
      table_1[index,6] <- round(IQR(unlist(input_num_vars[,i])),2)  # IQR
    }
  }
  # small number suppression if number <=5
  table_1$number[index] = redactor2(table_1$number[index])
  
  # index <- which(table_1$number<=5)
  # table_1[index,2:ncol(table_1)] = "redacted"
  
  write.csv(table_1, file=paste0("output/review/descriptives/table_1_",cohort, ".csv"), row.names = F)
  
  rmarkdown::render("analysis/compilation/compiled_table1_results.Rmd",
                    output_file=paste0("table_1_", cohort),output_dir="output/review/descriptives")
}

if (cohort == "all_cohorts") {
  table1_creation("all")
  table1_creation("vaccinated")
  table1_creation("infected")
} else{
  table1_creation(cohort)
}
