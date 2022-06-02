# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Produce hazard ratio plot for predictors
# Output:  

library(readr); library(dplyr)

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  analysis <- "all"          # all eligible population
  #analysis <- "all_vax_c"        # all eligible population but censored them by the 1st vaccination
  #analysis <- "vaccinated"   # vaccinated population
  #analysis <- "all_vax_td"    # vaccination status is included as a time-dependent covariate
  #analysis <- "infected"
}else{
  analysis <- args[[1]]
}

which_model = "full"

################################################################################
# Part 1. Create a list of candidate predictors                                #
################################################################################
# input <- read_rds("output/input_stage1_all.rds")
# pred_name <- colnames(input)[grepl("cov_",colnames(input))]
# 
# hr_table <- data.frame(pred_name = pred_name, n_levels = NA)
# for(i in hr_table$pred_name){
#   print(table(input[,i]))
#   if(grepl("_cat_",i)){
#   hr_table$n_levels[which(hr_table$pred_name==i)] = length(names(table(input[,i])))
#   }
# }
# 
# n_rows = sum(hr_table$n_levels, na.rm = TRUE) + length(which(is.na(hr_table$n_levels) ==TRUE))
# hr_table2 <- data.frame(pred_name = rep(NA, n_rows), pred_level = rep(NA, n_rows))
# j=1
# for(i in 1:nrow(hr_table)){
#   print(hr_table[i,1])
#   len = hr_table$n_levels[i]
#   if(is.na(len)){
#     len = 1
#   }
#   end = j+len-1
#   hr_table2$pred_name[j:end] <- rep(hr_table$pred_name[i], len)
#   if(len > 1){
#   hr_table2$pred_level[j:end] = names(table(input[,hr_table$pred_name[i]]))
#   }
#   j = j+len
# }
# 
# hr_table2$term <- paste0(hr_table2$pred_name, "=", hr_table2$pred_level)
# 
hr <- read_csv(file=paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis, ".csv"))
# 
# hr_combine <- full_join(hr_table2, hr, by = "term")
# 
# hr <- hr_combine

hr <- hr %>% dplyr::select(c("term","hazard_ratio", "conf.low", "conf.high")) %>%
  # keep all characters before =
  mutate(pred_name = sub("=.*", "", term))  %>%
  mutate(pred_name = sub("cov_cat_", "", pred_name)) %>%
  mutate(pred_name = sub("cov_num_", "", pred_name)) %>%
  # keep all characters after =
  mutate(pred_level = sub(".*=", "", hr$term)) %>%
  mutate(term = sub("cov_cat_", "", term)) %>%
  mutate(term = sub("cov_num_", "", term)) %>%
  mutate(term = sub("_", " ", term)) 

ggplot(hr, aes(x = hazard_ratio, y = term)) +        # ggplot2 plot with confidence intervals
  geom_point(shape=15, size = 3, color = "darkblue") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), size=0.5) +
  xlab(label='\nHazard Ratio and 95% Confidence Interval')+
  #
  ylab(label='Predictorss\n') +
  geom_vline(xintercept = 1, linetype = 2, color = "#E69F00", size=1)
