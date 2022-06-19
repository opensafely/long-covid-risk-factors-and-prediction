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


#write.csv(data, file = paste0(output_dir, "pm.csv"), row.names=F)


# x_min = min(data$c_stat_lower)
# x_max = max(data$c_stat_upper)
# pm_plot <- ggplot(data, aes(x = c_stat, y = reorder(region, desc(region)))) +    # ggplot2 plot with confidence intervals
#   geom_point(shape=15, size = 3, color = "darkblue") +
#   geom_errorbar(aes(xmin = c_stat_lower, xmax = c_stat_upper), size=0.5) +
#   xlab(label='\nC Statistic')+
#   #
#   ylab(label='Region\n') +
#   xlim(x_min, x_max) +
#   facet_grid(rows = vars(analysis)) +
#   #geom_vline(xintercept = 1, linetype = 2, color = "#b16100", size=1) +
#   theme(axis.text.y = element_text(hjust = 0), axis.text = element_text(size = 12),
#         axis.title=element_text(size=14,face="bold"), strip.text.y = element_text(angle = 0))
# 
# pm_plot
# 
# ggsave(file=paste0("plot_pm",".svg"), path = output_dir,
#        plot=pm_plot, width=10, height=12)
# 
# # for(csv_index in 1:length(df_list)){
# #   pm <- df_list[[csv_index]]
# #   pm <- pm %>% select("analysis", "c_stat", "c_stat_lower", "c_stat_upper")
# #   names(pm)[grep("c_stat",names(pm))] <- paste0("c_stat_", analysis[csv_index])
# #   names(pm)[grep("c_stat_lower",names(pm))] <- paste0("c_stat_lower_", analysis[csv_index]) 
# #   names(pm)[grep("c_stat_upper",names(pm))] <- paste0("c_stat_upper_", analysis[csv_index]) 
# #   df_list[[csv_index]] <- pm
# # }
# # 
# # # analysis = "all"
# # region <- data.frame(df_list[1])$analysis
# # c_all <- data.frame(df_list[1])$c_stat
# # c_lower_all <- data.frame(df_list[1])$c_stat_lower
# # c_upper_all <- data.frame(df_list[1])$c_stat_upper
# # 
# # # analysis = "all_vax_c"
# # c_vax_c <- data.frame(df_list[2])$c_stat
# # c_lower_vax_c <- data.frame(df_list[2])$c_stat_lower
# # c_upper_vax_c <- data.frame(df_list[2])$c_stat_upper
# # 
# # # analysis = "all_vax_td"
# # c_td <- data.frame(df_list[2])$c_stat
# # c_lower_td <- data.frame(df_list[2])$c_stat_lower
# # c_upper_td <- data.frame(df_list[2])$c_stat_upper
# # 
# # # analysis = "infected"
# # c_infected <- data.frame(df_list[2])$c_stat
# # c_lower_infected <- data.frame(df_list[2])$c_stat_lower
# # c_upper_infected <- data.frame(df_list[2])$c_stat_upper
# # 
# # # analysis = "vaccinated"
# # c_vaccinated <- data.frame(df_list[2])$c_stat
# # c_lower_vaccinated <- data.frame(df_list[2])$c_stat_lower
# # c_upper_vaccinated <- data.frame(df_list[2])$c_stat_upper
# # 
# # data <- data.frame(region, c_all, c_lower_all, c_upper_all, 
# #                         c_vax_c, c_lower_vax_c, c_upper_vax_c, 
# #                         c_td, c_lower_td, c_upper_td,
# #                         c_infected, c_lower_infected, c_upper_infected,
# #                         c_vaccinated, c_lower_vaccinated, c_upper_vaccinated)
# 
# 
# 
