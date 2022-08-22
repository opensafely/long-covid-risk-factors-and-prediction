# Purpose: to create hazard ratio plot externally
# Programmed by Yinghui Wei
# Date: 2022-06-17
# https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)
source("analysis/externals/ext_function_HR_df.R")
common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"
results_dir = paste0(common_dir, "2022-08-22/")
output_dir <- paste0(common_dir, "2022-08-22/")
file_list=list.files(path = results_dir, pattern = "HR_*")
for (i in 1:length(file_list)){
  #temp = gsub(".csv", "", file_list[i])
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )
}

df_list <- list(HR_full_model_age_spline_all_vax_c.csv, 
                HR_full_model_age_spline_vaccinated.csv) 
csv_index = 1

infected_cohort <- grep("infected",file_list)
td_analysis <- grep("td", file_list)

hr_vax_c <- function_HR_df(df_list=df_list, csv_index=1)
hr_vax_c$cohort = "Pre-vaccination"
hr_vaccinated <- function_HR_df(df_list=df_list, csv_index=2)
hr_vaccinated$cohort = "Post-vaccination"
hr_two_cohorts <- rbind(hr_vax_c, hr_vaccinated)
hr_two_cohorts$cohort <- factor(hr_two_cohorts$cohort, levels=c("Pre-vaccination", "Post-vaccination"))

## Two cohorts together

fp_two <- ggplot(data=hr_two_cohorts, aes(y=hazard_ratio, x = reorder(term, desc(row.num)), 
                          ymin=conf.low, ymax=conf.high, colour=subgroup)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Predictors\n") + ylab(label = "\nHazard Ratio") +
  theme_bw()  # use a white background
fp_two <- fp_two + 
  scale_y_log10(breaks=c(0,0.25, 0.5,1, seq(2,ceiling(max(hr_two_cohorts$conf.high,na.rm =T)),by=2))) +
  facet_wrap(~cohort,strip.position="top",ncol=2,scales = "free_x") +
  theme(legend.position='none',axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        # facet title size
        strip.text.x = element_text(size = 18))

cohort = "pre_vax_post_vax"
ggsave(file=paste0("plot_HR_",cohort, ".svg"), path = paste0(output_dir, "figures"),
       plot=fp_two, width=25, height=15)

## One cohort
fp <- ggplot(data=hr_vax_c, aes(y=hazard_ratio, x = reorder(term, desc(row.num)), 
                          ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Predictors\n") + ylab(label = "\nHazard Ratio") +
  theme_bw()  # use a white background
fp + scale_y_log10(breaks=c(0,0.25, 0.5,1,seq(2:ceiling(max(hr_vax_c$conf.high,na.rm =T),by=2))))

fp_one <- ggplot(data=hr_vax_c, aes(y=hazard_ratio, x = reorder(term, desc(row.num)), 
                                          ymin=conf.low, ymax=conf.high, colour=subgroup)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_log10(breaks=c(0,0.25, 0.5,1, seq(2,ceiling(max(hr_vax_c$conf.high,na.rm =T)),by=2))) +
  xlab("Predictors\n") + ylab(label = "\nHazard Ratio") +
  theme_bw()  # use a white background
fp_one <- fp_one + 
  theme(legend.position='none',axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.margin = margin(t = 20,  # Top margin
                             r = 20,  # Right margin
                             b = 20,  # Bottom margin
                             l = 20))

cohort = "pre_vax"
ggsave(file=paste0("plot_HR_",cohort, ".svg"), path = paste0(output_dir, "figures"),
       plot=fp_one, width=12, height=15)
