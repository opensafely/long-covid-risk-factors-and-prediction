# Purpose: to format table 1 for the manuscript
# Programmed by Yinghui Wei
# 8 September 2022

library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)
common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"
output_dir <- paste0(common_dir, "2022-09-01/")

df <- read.csv(file=paste0(output_dir,"table_2_combined.csv"), header=T)


df <- df %>% mutate(ir.all = format(round(1000* ir.all, 1), nsmall=1)) %>%
  mutate(ir_lower.all = format(round(1000*ir_lower.all, 1), nsmall=1)) %>%
  mutate(ir_upper.all = format(round(1000*ir_upper.all, 1), nsmall=1)) %>%
  mutate(ir.all_vax_c = format(round(1000*ir.all_vax_c, 1), nsmall=1)) %>%
  mutate(ir_lower.all_vax_c = format(round(1000*ir_lower.all_vax_c, 1), nsmall=1)) %>%
  mutate(ir_upper.all_vax_c = format(round(1000*ir_upper.all_vax_c, 21), nsmall=1)) %>%
  mutate(ir.vaccinated = format(round(1000*ir.vaccinated, 1), nsmall=1)) %>%
  mutate(ir_lower.vaccinated = format(round(1000*ir_lower.vaccinated, 1), nsmall=1)) %>%
  mutate(ir_upper.vaccinated = format(round(1000*ir_upper.vaccinated, 1), nsmall=1)) %>%
  mutate(ir.infected = format(round(1000*ir.infected, 1), nsmall=1)) %>%
  mutate(ir_lower.infected = format(round(1000*ir_lower.infected, 1), nsmall=1)) %>%
  mutate(ir_upper.infected = format(round(1000*ir_upper.infected, 21), nsmall=1))

df <- df %>% mutate(primary = paste0(ir.all, " (", ir_lower.all, ", ", ir_upper.all,  ")" )) %>%
  mutate(prevax = paste0(ir.all_vax_c, " (", ir_lower.all_vax_c, ", ", ir_upper.all_vax_c,  ")" )) %>%
  mutate(vax = paste0(ir.vaccinated, " (", ir_lower.vaccinated, ", ", ir_upper.vaccinated,  ")" )) %>%
  mutate(infected = paste0(ir.infected, " (", ir_lower.infected, ", ", ir_upper.infected,  ")"))

df <- df %>% mutate(event_count.all = format(event_count.all, big.mark=",", scientific=FALSE)) %>%
  mutate(event_count.all_vax_c = format(event_count.all_vax_c, big.mark=",", scientific=FALSE)) %>%
  mutate(event_count.vaccinated = format(event_count.vaccinated, big.mark=",", scientific=FALSE)) %>%
  mutate(event_count.infected = format(event_count.infected, big.mark=",", scientific=FALSE)) 

df <- df %>% 
  select("subgrp", "subgrp_level", 
         "event_count.all", "primary",
         "event_count.all_vax_c", "prevax",
         "event_count.vaccinated", "vax",
         "event_count.infected", "infected", everything())


df_cols <- df %>% select(c("subgrp", "subgrp_level"))

#write.csv(df_cols, file=paste0(output_dir,"table_2_fixed.csv"), row.names=F)

df_order <- read.csv(paste0(common_dir,"table_2_fixed.csv"))

temp <- merge(df, df_order, all=T)

temp <- temp[order(temp$row.num),]

df <- temp

# YW comment: have missed BMI, is this needed?

df <- df %>% filter(subgrp != "	healthcare_worker")  %>%
  filter(subgrp != "post_viral_fatigue_pre_pandemic")

write.csv(df, file=paste0(output_dir,"paper_table_2.csv"), row.names=F)
