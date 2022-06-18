# Purpose: to create hazard ratio plot externally

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
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
  dplyr::select(c("term","hazard_ratio", "conf.low", "conf.high", "std.error")) %>%
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

pred_name <- unique(hr$pred_name)

pred_count <- rep(NA, length(pred_name))
index = 1
for(i in pred_name){
  pred_count[index] <- length(which(hr$pred_name == i))
  index = index + 1
}

df <- data.frame(pred_name, pred_count)
df <- df %>% filter(pred_count>1) %>%select(pred_name)
a <- data.frame(matrix(nrow = nrow(hr)+nrow(df), ncol=ncol(hr)))
a[1:nrow(hr),] <- hr
names(a) <- names(hr)
for(i in 1:nrow(df)){
  a$term[nrow(hr)+i] <- a$pred_name[nrow(hr)+i] <- df$pred_name[i]
}

hr <- a
hr <- hr %>% mutate(term = gsub("_", " ", term))

hr$term <- str_to_sentence(hr$term)
hr$term <-sub("Bmi", "BMI", hr$term)
hr$term <-sub("Imd", "IMD", hr$term)
hr$term <-sub("Sle", "SLE", hr$term)

# hr$term <- sub("gp consultation=Greater or equal to 12", "Frequent GP consultation", hr$term)
# hr$term <- sub("sex=M", "Male", hr$term)
# hr$term <-gsub(".*=","       ",hr$term)
# #hr$term <-gsub("="," ",hr$term)
# hr$term <- str_to_sentence(hr$term)
# hr$term <-sub("Bmi", "BMI", hr$term)
# hr$term <-sub("Imd", "IMD", hr$term)
# hr$term <-sub("Sle", "SLE", hr$term)

temp <- hr$term[order(hr$term, decreasing = T)]
row.ref = which(temp == "Post viral fatigue")

hr_plot <- ggplot(hr, aes(x = hazard_ratio, y = reorder(term, desc(term)))) +    # ggplot2 plot with confidence intervals
  geom_point(shape=15, size = 3, color = "darkblue") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), size=0.5) +
  xlab(label='\nHazard Ratio and 95% Confidence Interval')+
  #
  ylab(label='Predictorss\n') +
  xlim(0.25, 2.5) +
  geom_vline(xintercept = 1, linetype = 2, color = "#b16100", size=1) +
  theme(axis.text.y = element_text(hjust = 0), axis.text = element_text(size = 12),
        axis.title=element_text(size=14,face="bold"))


hr_plot 
text = paste0("HR = ", post_viral_fatigue_hr$hazard_ratio, " (", 
              post_viral_fatigue_hr$conf.low, " to ", post_viral_fatigue_hr$conf.high, ")")
hr_plot <- hr_plot + annotate("text", x=2, y=row.ref, label = text)
hr_plot
ggsave(file=paste0(file_list[1], ".svg"), path = output_dir,
       plot=hr_plot, width=10, height=12)