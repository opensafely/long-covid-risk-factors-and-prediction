# Purpose: to create forest plots for apparent performance measures externally
# Programmed by Yinghui Wei
# Date: 2022-06-20

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)

results_dir <- "C:/Users/ywei3/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/"
output_dir <- "C:/Users/ywei3/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/figures/"
file_list=list.files(path = results_dir, pattern = "hazard_ratio_estimates_*")
for (i in 1:length(file_list)){

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

pm_df <- data.frame(analysis = character(),
                     concordance  = numeric(),
                     concordance_lower = numeric(),
                     concordance_upper = numeric(),
                     stringsAsFactors = FALSE)

csv_index = 1
a <- sub(".*estimates_","",file_list)
analysis <- sub(".csv","", a)

for(csv_index in 1:length(df_list)){
  index = nrow(pm_df)+1
  print(csv_index)
  pm_df[index,1] = analysis[csv_index]
  df <- df_list[[csv_index]]
  pm_df[index,2] = df$concordance[1]
  pm_df[index,3] = df$concordance.lower[1]
  pm_df[index,4] = df$concordance.upper[1]
}

pm_df$cohort <- pm_df$analysis
b <- sub("..*", "", a)

pm_df$cohort <- gsub("full_*", "",pm_df$cohort)
pm_df$cohort <- gsub("selected_*", "",pm_df$cohort)
pm_df$cohort <- gsub("_", " ",pm_df$cohort)
pm_df$cohort <- str_to_sentence(pm_df$cohort)
pm_df$cohort <- gsub("All vax c", "Unvaccinated", pm_df$cohort)
pm_df$cohort <- gsub("All vax td", "All vax td", pm_df$cohort)

pm_df$model<- pm_df$analysis
# keep all characters before the first _ 
pm_df$model <- sub("_.*", "", pm_df$model)
pm_df$model <- str_to_sentence(pm_df$model)

pm_df1 <- pm_df %>% filter(model == "Full")
pm_df2 <- pm_df %>%filter(model == "Selected")

names(pm_df)[grep("model",names(pm_df))] <- "Model"

cohort_order <- c("Infected", "All vax td","Vaccinated", "Unvaccinated", "All")

pm_df$cohort = factor(pm_df$cohort, level = cohort_order)

#define colours for dots and bars
dotCOLS = c("#a6d8f0","#f9b282")
barCOLS = c("#008fd5","#de6b35")


p <- ggplot(pm_df, aes(x=cohort, y=concordance, ymin=concordance_lower, ymax=concordance_upper,col=Model,fill=Model)) + 
  #specify position here
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
 # geom_hline(yintercept=1, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="Cohort\n") +
  scale_y_continuous(name="\nConcordance Statistic", limits = c(0.5, 0.9)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0), axis.text = element_text(size = 16),
        axis.title=element_text(size=14,face="bold"), 
        legend.title=element_text(size=16),
        legend.text=element_text(size=16))
p

ggsave(file=paste0("apparent_c_statistic",".svg"), path = output_dir,
       plot=p, width=12, height=6)
