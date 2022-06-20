# Purpose: to create forest plots for performance measures externally
# Programmed by Yinghui Wei
# Date: 2022-06-19

#https://nightingalehealth.github.io/ggforestplot/articles/ggforestplot.html
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
library(ggforestplot)
results_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/"
output_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/figures/"

file_list=list.files(path = results_dir, pattern = "val_*")
file_list <- file_list[-6]
for (i in 1:length(file_list)){
  #temp = gsub(".csv", "", file_list[i])
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )
}

df_list <- list(val_performance_measures_all.csv, 
                val_performance_measures_all_vax_c.csv, 
                val_performance_measures_all_vax_td.csv,
                val_performance_measures_infected.csv,
                val_performance_measures_vaccinated.csv) 
csv_index = 1
a <- sub(".*measures_","",file_list)
analysis <- sub(".csv","", a)

for(csv_index in 1:length(df_list)){
  pm <- df_list[[csv_index]]
  pm <- pm %>% select("analysis", "c_stat","c_stat_var" ,"c_stat_lower", "c_stat_upper")
  names(pm)[grep("analysis",names(pm))] <- "region"
  pm$analysis = analysis[csv_index]
  pm$c_stat_se = sqrt(pm$c_stat_var)
  df_list[[csv_index]] <- pm
}

data <- df_list[[1]]

for(csv_index in 2:length(df_list)){
  data <- rbind(data, df_list[[csv_index]])
}


View(data)

data <-rename(data, name = region)
data <-rename(data, se = c_stat_se)
#data <- rename(data, beta = c_stat)
# sort data by region
data2 <-data %>%dplyr::arrange(name) 

# Forest plot
pm_plot <- ggforestplot::forestplot(
  df = data2,
  estimate = c_stat,
  #logodds = FALSE,
  colour = analysis,
  xlab = "Concordance statistic"
)
pm_plot
ggsave(file=paste0("plot_pm",".svg"), path = output_dir,
       plot=pm_plot, width=10, height=12)
