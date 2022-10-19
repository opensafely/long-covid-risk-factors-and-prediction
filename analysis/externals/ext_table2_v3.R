# Purpose: to format table 1 for the manuscript
# Programmed by Yinghui Wei
# 29 September 2022

library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)
common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"
output_dir <- paste0(common_dir, "2022-10-10/")

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


df <- df %>% mutate(count_pr.all = paste0(event_count.all, "/", format(round(person_years.all/1000,1), big.mark=",", scientific=FALSE))) %>%
  mutate(count_pr.prevax = paste0(event_count.all_vax_c, "/", format(round(person_years.all_vax_c/1000,1), big.mark=",", scientific=FALSE))) %>%
  mutate(count_pr.vax = paste0(event_count.vaccinated, "/", format(round(person_years.vaccinated/1000,1), big.mark=",", scientific=FALSE))) %>%
  mutate(count_pr.infected = paste0(event_count.infected, "/", format(round(person_years.infected/1000,1), big.mark=",", scientific=FALSE)))


# original table 2: include event count and incidence rate
# df <- df %>% 
#   select("subgrp", "subgrp_level", 
#          "event_count.all", "primary",
#          "event_count.all_vax_c", "prevax",
#          "event_count.vaccinated", "vax",
#          "event_count.infected", "infected", everything())

df <- df %>% 
  select("subgrp", "subgrp_level", 
         "count_pr.all", "ir.all",
         "count_pr.prevax", "ir.all_vax_c",
         "count_pr.vax", "ir.vaccinated",
         "count_pr.infected","ir.infected", everything())



df_cols <- df %>% select(c("subgrp", "subgrp_level"))

#write.csv(df_cols, file=paste0(output_dir,"table_2_fixed.csv"), row.names=F)

df_order <- read.csv(paste0(common_dir,"table_2_fixed.csv"))

temp <- merge(df, df_order, all=T)

temp <- temp[order(temp$row.num),]

df <- temp

df <- df %>% filter(subgrp != "	healthcare_worker")  %>%
  filter(subgrp != "post_viral_fatigue_pre_pandemic")

write.csv(df, file=paste0(output_dir,"paper_table_2_v2.csv"), row.names=F)
