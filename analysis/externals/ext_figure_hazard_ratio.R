# Purpose: to create hazard ratio plot externally
# Programmed by Yinghui Wei
# Date: 2022-06-17

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(stringr)
results_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/"
output_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/figures/"
file_list=list.files(path = results_dir, pattern = "hazard_ratio_estimates_*")
#file_to_keep <- grepl("infected",file_list)
#file_list <- file_list[file_to_keep]
# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  #temp = gsub(".csv", "", file_list[i])
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )
}

# df_list <- list(hazard_ratio_estimates_full_infected.csv,
#                 hazard_ratio_estimates_selected_infected.csv) 
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
csv_index = 1
infected_cohort <- grep("infected",file_list)
td_analysis <- grep("td", file_list)
for(csv_index in 1:length(df_list)){
  hr <- data.frame(df_list[csv_index])
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
  
  index = which(hr$term == "post viral fatigue")
  #x_min = max(0,min(hr[-index,]$conf.low,na.rm=T)-0.1)
  #x_max = max(hr[-index,]$conf.high,na.rm=T)+0.1
  x_min = max(0,min(hr[-index,]$conf.low)-0.1)
  x_max = max(hr[-index,]$conf.high)+0.1
  
  if(csv_index %in% c(1,3)){
    x_annot = x_max - 0.5
  }
  if(csv_index %in% c(2,5)){
    x_annot = x_max - 1
  }
  if(csv_index %in% c(4,9)){
    x_annot = x_max - 0.6
  }
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
  
  
  temp <- hr$term[order(hr$term, decreasing = T)]
  hr <- hr[order(hr$term),]
  
  # df <- hr %>% select(term, pred_name, pred_level)
  # 
  # write.csv(df, file = paste0(output_dir, "hr_var_order.csv"), row.names=F)
  
  df <- read.csv(file=paste0(output_dir, "hr_var_order_fixed.csv"))
  
  # remove Ie.status if the HR are not from Cox model with vaccination status as a time-dependent variable
  if(!csv_index%in%td_analysis){
    df <- df%>%filter(!grepl("Ie", df$term, fixed = TRUE))
  }
  if(!csv_index%in%infected_cohort){
    df <- df%>%filter(!grepl("Covid", term, fixed = TRUE)) %>%
      filter(!grepl("COVID", term, fixed = TRUE))
  }
  
  a <- merge(df,hr,by="term", all=T)
  
  a <- a[order(a$row.num),]
  #View(a)
  
  hr <- a
  
  temp <- hr$term[reorder(hr$term, desc(hr$row.num))]
  row.ref = which(temp == "Post viral fatigue")
  
  
  #hr[row.ref,2:4] = NA
  #b <- reorder(hr$term, desc(hr$row.num))
  #row.ref = hr$row.num[which(hr$term == "Post viral fatigue")]
  #row.ref = which(temp == "Post viral fatigue")
  
  #hr$row.num = seq(1:nrow(hr))
  hr$term <- sub("Ie.status", "Vaccination status", hr$term)
  hr$term <- sub("Vaccination status=1", "       Vaccinated", hr$term)
  hr$term <- sub("Smoking status=missing", "       Missing (smoking status)", hr$term)
  hr$term <- sub("Age'", "Age spline: the 2nd basis function", hr$term)
  hr$term <- sub("Gp consultation=greater or equal to 12", "      12 or more", hr$term)
  hr$term <- sub("Gp consultation=less than 12", "       <12", hr$term)
  hr$term <- sub("Gp consultation truncated", "GP consultation (0-11)", hr$term)
  hr$term <- sub("Sex=m", "       Male", hr$term)
  hr$term <-gsub(".*=","       ",hr$term)
  #hr$term <-gsub("="," ",hr$term)
  hr$term <- str_to_sentence(hr$term)
  hr$term <-sub("Bmi", "BMI", hr$term)
  hr$term <-sub("Imd", "IMD", hr$term)
  hr$term <-sub("Sle", "SLE", hr$term)
  hr$term <-sub("Ra sle psoriasis", "Rheumatoid arthritis, SLE or Psoriasis", hr$term)
  hr$term <- sub("gp", "GP", hr$term)
  hr$term <- sub("Gp", "GP", hr$term)
  hr$term <- sub("Haem cancer", "Haematological cancer", hr$term)
  hr <- hr %>% filter(!is.na(term))
  temp2 <- which(hr$term == "Post viral fatigue")
  post_viral_fatigue_hr <- hr[temp2, ]
  
  hr_plot <- ggplot(hr, aes(x = hazard_ratio, y = reorder(term, desc(row.num)))) +    # ggplot2 plot with confidence intervals
    geom_point(shape=15, size = 3, color = "darkblue") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), size=0.5) +
    xlab(label='\nHazard Ratio')+
    #
    ylab(label='Predictors\n') +
    xlim(x_min, x_max) +
    geom_vline(xintercept = 1, linetype = 2, color = "#b16100", size=1) +
    theme(axis.text.y = element_text(hjust = 0), axis.text = element_text(size = 12),
          axis.title=element_text(size=14,face="bold"))
  
  hr_plot 
  text = paste0(">>>> HR = ", post_viral_fatigue_hr$hazard_ratio, " (", 
                post_viral_fatigue_hr$conf.low, " to ", post_viral_fatigue_hr$conf.high, ")")
  hr_plot <- hr_plot + annotate("text", x=x_annot, y=row.ref, label = text)
  hr_plot
  
  a <- sub(".*estimates","",file_list[csv_index])
  b <- sub(".csv","", a)
  ggsave(file=paste0("plot_hr",b, ".svg"), path = output_dir,
         plot=hr_plot, width=10, height=12)
}

