# Purpose: to create hazard ratio plot externally

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
results_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/"
output_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/figures/"
file_list=list.files(path = results_dir, pattern = "hazard_ratio_estimates_*")

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  #temp = gsub(".csv", "", file_list[i])
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )
  }

df_list <- list(hazard_ratio_estimates_full_all.csv, 
                hazard_ratio_estimates_full_all_vax_c.csv, 
                hazard_ratio_estimates_full_all_vax_td.csv,
                hazard_ratio_estimates_full_infected.csv,
                hazard_ratio_estimates_full_vaccinated.csv,
                hazard_ratio_estimates_selected_all.csv,
                hazard_ratio_estimates_selected_all_vax_c.csv,
                hazard_ratio_estimates_selected_all_vax_td.csv,
                hazard_ratio_estimates_selected_infected.csv,
                hazard_ratio_estimates_selected_vaccinated.csv) 

hr <- data.frame(df_list[1])
row.ref = which(hr$term == "cov_cat_post_viral_fatigue=TRUE")
post_viral_fatigue_hr <- hr[row.ref, ]
hr[row.ref,2:4] = NA
hr <- hr %>% 
  dplyr::select(c("term","hazard_ratio", "conf.low", "conf.high")) %>%
  # keep all characters before =
  mutate(pred_name = sub("=.*", "", term))  %>%
  mutate(pred_name = sub("cov_cat_", "", pred_name)) %>%
  mutate(pred_name = sub("cov_num_", "", pred_name)) %>%
  # keep all characters after =
  mutate(pred_level = sub(".*=", "", term)) %>%
  mutate(term = sub("cov_cat_", "", term)) %>%
  mutate(term = sub("cov_num_", "", term)) %>%
  mutate(term = gsub("_", " ", term)) %>%
  mutate(term = gsub("=TRUE", "", term))

hr <- hr[order(hr$term),]

row.ref = which(hr$term == "post viral fatigue")

hr_plot <- ggplot(hr, aes(x = hazard_ratio, y = term)) +    # ggplot2 plot with confidence intervals
  geom_point(shape=15, size = 3, color = "darkblue") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), size=0.5) +
  xlab(label='\nHazard Ratio and 95% Confidence Interval')+
  #
  ylab(label='Predictorss\n') +
  xlim(0, 4) +
  geom_vline(xintercept = 1, linetype = 2, color = "#b16100", size=1)
hr_plot 
text = paste0("HR for post viral fagitue: ", post_viral_fatigue_hr$hazard_ratio, " (95% CI:", 
              post_viral_fatigue_hr$conf.low, ",", post_viral_fatigue_hr$conf.high, ")")
hr_plot + annotate("text", x=3, y=row.ref, label = text)

ggsave(file=paste0(file_list[1], ".svg"), path = output_dir,
       plot=hr_plot, width=12, height=10)

