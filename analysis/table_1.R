# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 1. Baseline characteristics of patients. 
#          Categorical variables: number and percentage
#          Continuous variables:  number and percentage of observations, 
#                                 mean and standard deviation
# Output:  table1.csv, table1.html

library(readr); library(dplyr); library(lubridate)

#population ="unvaccinated"

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")
cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
cov_factor_names <- c(cov_factor_names, "sub_cat_covid_history")
cov_num_names <- names(input)[grepl("cov_num", names(input))]

# Create an empty data frame ---------------------------------------------------
table_1 <- data.frame(variable = character(),
                     number  = numeric(),
                     percent = numeric(),
                     mean    = numeric(),
                     sd      = numeric(), 
                     iqr     = numeric(),
                     min     = numeric(),
                     max     = numeric(),
                     stringsAsFactors = FALSE)

# factor variables: number and percentage---------------------------------------
input_factor_vars <- input[, cov_factor_names]
for(i in 1:length(cov_factor_names)){
 # levels = paste0(cov_factor_names[i], "_",  names(table(input_factor_vars[,i])))
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
for(i in 1:length(cov_num_names)){
  index = nrow(table_1)+1
  table_1[index,1] <- cov_num_names[i]
  table_1[index,2] <- length(which(!is.na(unlist(input_num_vars[,2])))) # number of observations
  table_1[index,3] = table_1[index,2]/nrow(input_num_vars)  # percentage of not missing
  table_1[index,4] <- round(mean(unlist(input_num_vars[,i])),2) # mean
  table_1[index,5] <- round(sd(unlist(input_num_vars[,i])),2) # sd
  table_1[index,6] <- round(IQR(unlist(input_num_vars[,i])),2)  # IQR
  table_1[index,7] <- round(min(unlist(input_num_vars[,i])),2)  # min
  table_1[index,8] <- round(max(unlist(input_num_vars[,i])),2)  # max  
}

# small number suppression if number <=5
index <- which(table_1$number<=5)
table_1[index,2:ncol(table_1)] = "redacted"

write.csv(table_1, file="output/table_1.csv", row.names = F)

rmarkdown::render("analysis/compiled_table1_results.Rmd",
                  output_file="table_1",output_dir="output")


