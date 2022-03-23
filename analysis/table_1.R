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
table1 <- data.frame(characteristic = character(),
                     number  = numeric(),
                     percent = numeric(),
                     mean    = numeric(),
                     sd      = numric())

levels(input$cov_cat_age)

paste0(cov_factor_names[1], "_", levels(input$cov_cat_age))

input <- input[, cov_factor_names]
names(table(input[,1]))

for(i in 1:2){
  #temp = input[,cov_factor_names[i]]
  levels = paste0(cov_factor_names[i], "_",  names(table(input[,i])))
  print(levels)
}
