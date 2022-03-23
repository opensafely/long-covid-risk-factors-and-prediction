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
table_1 <- data.frame(variable = character(),
                     number  = numeric(),
                     percent = numeric(),
                     mean    = numeric(),
                     sd      = numeric(), 
                     stringsAsFactors = FALSE)

levels(input$cov_cat_age)

paste0(cov_factor_names[1], "_", levels(input$cov_cat_age))

input_factor_vars <- input[, cov_factor_names]
names(table(input_factor_vars[,1]))

#table_1[1,1]="test"
for(i in 1:length(cov_factor_names)){
  levels = paste0(cov_factor_names[i], "_",  names(table(input_factor_vars[,i])))
  start = nrow(table_1)+1
  end = nrow(table_1)+length(levels)
  table_1[start:end,1] <- c(levels)            # variable name
  table_1[start:end,2] <- c(table(input_factor_vars[,i]))  # number
  table_1[start:end,3] <- 100*round(c(table(input_factor_vars[,i]))/nrow(input_factor_vars),4)  # percentage
  print(levels)
}

#position <- which(grepl("cov_num_",names(input)) == TRUE)
input_num_vars <- input[,cov_num_names]
for(i in 1:length(cov_num_names)){
  index = nrow(table_1)+1
  table_1[index,1] <- cov_num_names[i]
  table_1[index,4] <- round(mean(unlist(input_num_vars[,i])),2) # mean
  table_1[index,5] <- round(sd(unlist(input_num_vars[,i])),2) # sd
}

