# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 1. Baseline characteristics of patients. 
#          Data are numbers and percentages for categorical variables;
#          Data are mean and standard deviation for continuous variables
# Output:  table1.csv, table1.html

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate)

input <- read_rds("output/input_stage1.rds")
cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
cov_num_names <- names(input)[grepl("cov_num", names(input))]


#lapply(input[,cov_factor_names], function(x) table(x))
