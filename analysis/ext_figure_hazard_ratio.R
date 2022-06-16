# Purpose: to create hazard ratio plot externally

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
results_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/"

file_list=list.files(path = results_dir, pattern = "hazard_ratio_estimates_*")

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )}


hr <- hazard_ratio_estimates_full_all.csv %>% dplyr::select(c("term","hazard_ratio", "conf.low", "conf.high")) %>%
  # keep all characters before =
  mutate(pred_name = sub("=.*", "", term))  %>%
  mutate(pred_name = sub("cov_cat_", "", pred_name)) %>%
  mutate(pred_name = sub("cov_num_", "", pred_name)) %>%
  # keep all characters after =
  mutate(pred_level = sub(".*=", "", term)) %>%
  mutate(term = sub("cov_cat_", "", term)) %>%
  mutate(term = sub("cov_num_", "", term)) %>%
  mutate(term = sub("_", " ", term)) 

hr_plot <- ggplot(hr, aes(x = hazard_ratio, y = term)) +        # ggplot2 plot with confidence intervals
  geom_point(shape=15, size = 3, color = "darkblue") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), size=0.5) +
  xlab(label='\nHazard Ratio and 95% Confidence Interval')+
  #
  ylab(label='Predictorss\n') +
  geom_vline(xintercept = 1, linetype = 2, color = "#b16100", size=1)
hr_plot
ggsave(file=paste0("output/review/model/figure_hr_",which_model, "_", analysis, ".svg"), 
       plot=hr_plot, width=12, height=10)

# 
# hr_files=hr_files[endsWith(hr_files,".csv")]
# hr_files=paste0(results_dir,"/",hr_files)
# 
# hr_file_paths <- pmap(list(hr_files), 
#                       function(fpath){ 
#                         df <- fread(fpath) 
#                         return(df)
#                       })
# combined_hr <- rbindlist(hr_file_paths, fill=TRUE)
# 
# combined_hr <- combined_hr %>% mutate(across(c("hazard_ratio","conf.low","conf.high"), as.numeric))
