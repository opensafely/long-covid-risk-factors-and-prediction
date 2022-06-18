# purpose: to format table 1 externally
# programmed by Yinghui Wei
# date: 2022-06-18

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
results_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/"
output_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/tables/"
file_list=list.files(path = results_dir, pattern = "table_1_*")

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  #temp = gsub(".csv", "", file_list[i])
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )
}

df_list <- list(table_1_all.csv, 
                table_1_infected.csv, 
                table_1_vaccinated.csv) 

csv_index = 1
table1 = df_list[[csv_index]]

# df <- table1 %>%select(variable)
# write.csv(df, file = paste0(output_dir, "table_1_template.csv"), row.names=F)

# df <- read.csv(file=paste0(output_dir, "table_1_var_order_fixed.csv"))
# 
# a <- merge(df,table1,all=T)
# a <- a[order(a$row.num),]
# #View(a)
# 
# hr <- a

table1 <- table1%>%
  #filter(!grepl("sub_cat_covid_phenotype", variable)) %>%
  #filter(!grepl("sub_cat_covid_history", variable)) %>%
  mutate(variable = sub("cov_cat_", "", variable)) %>%
  mutate(variable = sub("cov_num_", "", variable)) %>%
  mutate(variable = sub("sub_num_", "", variable)) %>%
  mutate(variable = gsub("_", " ", variable))%>%
  mutate(variable = str_to_sentence(variable)) %>%
  mutate(variable = sub("True", "TRUE", variable)) %>%
  mutate(variable = sub("False", "FALSE", variable)) %>%
  mutate(variable = sub("Bmi", "BMI", variable)) %>%
  mutate(variable = sub("Imd", "IMD", variable))

table1[is.na(table1)] <- " "
a <- sub(".*_1_","",file_list[csv_index])
b <- sub(".csv","", a)
write.csv(table1, file = paste0(output_dir, "table_1_", b, ".csv"), row.names=F)
