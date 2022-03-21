# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Prepare variables
# Output:  input.rds
library(readr); library(dplyr); library("arrow"); library("data.table")

input <- read_feather("output/input.feather")
View(input)
names(input)
which(grepl("bmi", names(input))==TRUE)
names(input)[which(grepl("cov_num", names(input))==TRUE)]

