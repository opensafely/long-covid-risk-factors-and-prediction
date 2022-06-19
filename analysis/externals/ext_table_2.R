# purpose: to format table 2 externally
# programmed by Yinghui Wei
# date: 2022-06-19

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
results_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/"
output_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/tables/"
file_list=list.files(path = results_dir, pattern = "table_2_*")


# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  #temp = gsub(".csv", "", file_list[i])
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )
}

df_list <- list(table_2_all.csv, 
                table_2_infected.csv, 
                table_2_vaccinated.csv) 

csv_index = 1
for(csv_index in c(1,2,3)){
  table2 = df_list[[csv_index]]
  a <- sub(".*_2_","",file_list[csv_index])
  b <- sub(".csv","", a)
  table2 <- table2 %>% filter(outcome == "long covid") %>%
    mutate(ir = ir*1000) %>%
    mutate(ir_lower =ir_lower*1000) %>%
    mutate(ir_upper = ir_upper*1000) %>%
    mutate(ir_ci = paste0(ir, " (", ir_lower, ", ", ir_upper, ")" )) %>%
    select(subgrp, subgrp_level, event_count, ir_ci) %>%
    mutate(cohort = b)
  write.csv(table2, file = paste0(output_dir, "table_2_", b, ".csv"), row.names=F)
}



