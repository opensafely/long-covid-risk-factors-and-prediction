# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 3: Sequence count for 1.COVID-VAX-Long COVID; 
#                                      2.COVID-Long COVID -VAX; 
#                                      3.VAX-COVID-Long COVID; 
#                                      4.COVID-VAX;
#                                      5.VAX-COVID
#                                      6.VAX-No COVID
# Output:  table_3.csv, table_3.html

library(readr); library(dplyr); 

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")
