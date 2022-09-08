# Purpose: to format table 1 for the manuscript
# Programmed by Yinghui Wei
# 8 September 2022

library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)
common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"
output_dir <- paste0(common_dir, "2022-09-01/")

df <- read.csv(file=paste0(output_dir,"table_1_combined.csv"), header=T)

df <- df %>% filter(subgroup_level != FALSE)

df <- df %>% rename(term = variable) %>%
  mutate(variable = ifelse(grepl("age",term), "Demographics",
                                     ifelse(grepl("sex",term), "Demographics",
                                            ifelse(grepl("bmi", term), "Demographics",
                                                   ifelse(grepl("ethnicity", term), "Demographics",
                                                          ifelse(grepl("region", term), "Demographics", 
                                                                 ifelse(grepl("imd", term), "Demographics",
                                                                        ifelse(grepl("worker", term), "Demographics",
                                                                        ifelse(grepl("gp", term), "GP-Patient Interaction", "Disease History")))))))))
# order by variable name and term name

df <- df[order(df$variable, df$term),]

df_cols <- df %>% select(c("term", "subgroup_level"))

#write.csv(df_cols, file=paste0(output_dir,"table_1_fixed.csv"), row.names=F)

df_order <- read.csv(paste0(common_dir,"table_1_fixed.csv"))

temp <- merge(df, df_order, all=T)

temp <- temp[order(temp$row.num),]

df <- temp

df <- df %>% mutate(number.all = format(number.all, big.mark=",", scientific=FALSE)) %>%
  mutate(number.vaccinated = format(number.vaccinated, big.mark=",", scientific=FALSE)) %>%
  mutate(number.infected = format(number.infected, big.mark=",", scientific=FALSE)) 

df <- df %>% mutate(primary = paste0(number.all, " (", percent.all, ")" )) %>%
  mutate(vax = paste0(number.vaccinated, " (", percent.vaccinated, ")" )) %>%
  mutate(infected = paste0(number.infected, " (", percent.infected, ")" ))
df$primary[1] = df$number.all[2]
df$vax[1] = df$number.vaccinated[2]
df$infected[1] = df$number.infected[2]

index = which(df$term == "cov_num_age")
df$primary[index] = paste0(df$mean.all[index], " (", df$sd.all[index], ")")
df$vax[index] = paste0(df$mean.vaccinated[index], " (", df$sd.vaccinated[index], ")")
df$infected[index] = paste0(df$mean.infected[index], " (", df$sd.infected[index], ")")

df <- df %>% filter(term != "sub_num_gp_patient_interaction") %>%
  filter(term != "sub_cat_covid_phenotype") %>%
  filter(term != "sub_cat_multimorbidity")

df <- df %>%
  select("term", "subgroup_level", "primary", "vax", "infected", everything())


write.csv(df, file=paste0(output_dir,"paper_table_1.csv"), row.names=F)
