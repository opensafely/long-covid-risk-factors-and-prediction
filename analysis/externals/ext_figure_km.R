# Purpose: to extract hazard ratios: 1) fully adjusted; 2) age-sex adjusted
# Programmed by Yinghui Wei
# Date: 2022-11-07

library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)

common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"

results_dir = paste0(common_dir, "2022-11-02/")
output_dir <- paste0(common_dir, "2022-11-02/")

dat_age <- read.csv(paste0(results_dir, "tbl_km_dat_age.csv"))
dat_age <- dat_age %>% mutate(x_date = as.Date(x_date))
# Create cumulative incidence plot -----------------------------------------------------------
svglite::svglite("output/review/descriptives/figure_kaplan_meier_age_sex_cum_incidence.svg", width = 9, height = 5,)

ggplot(dat_age, 
       aes(x_date,y, group = cov_cat_age_group, color=cov_cat_age_group, 
           linetype = cov_cat_age_group)) +
  geom_path() + #Plotting
  geom_line(aes(linetype=cov_cat_age_group),size=1)+
  scale_linetype_manual(values=c("18-39" = "longdash",
                                 "40-59" = "dotted",
                                 "60-79" = "dotdash",
                                 "80+"   = "solid"))+
  scale_color_manual(values = c("18-39" = "#808080",
                                "40-59"="#FF8000",
                                "60-79"= "#B266FF",
                                "80+" = "red"))+
  labs(title="",x="\nDate", y = "\nCumulative incidence of long COVID code\n") +
  labs(linetype='Age group', colour="Age group") +
  facet_grid(cols = vars(sex)) +
  scale_x_date(
    #limits = c(index_date, end_date), 
    date_breaks = "6 months", 
    date_labels =  "%b %Y") +
  theme(legend.position="bottom", legend.title=element_text(size=13), 
        legend.text=element_text(size=13),
        axis.text = element_text(size = 13),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 13))

dev.off()

# Create cumulative probability plot -----------------------------------------------------------
svglite::svglite(paste0(results_dir,"figures/km_plot.svg"), width = 3, height = 5)

p2 <- ggplot(dat_age, 
       aes(x_date,y_prob, group = cov_cat_age_group, color=cov_cat_age_group, 
           linetype = cov_cat_age_group)) +
  geom_path() + #Ploting
  geom_line(aes(linetype=cov_cat_age_group),size=1)+
  scale_linetype_manual(values=c("18-39" = "longdash",
                                 "40-59" = "dotted",
                                 "60-79" = "dotdash",
                                 "80+"   = "solid"))+
  scale_color_manual(values = c("18-39" = "#808080",
                                "40-59"="#FF8000",
                                "60-79"= "#B266FF",
                                "80+" = "red"))+
  labs(title="",x="\nDate", y = "\nCumulative probability of long COVID code\n") +
  labs(linetype='Age group', colour="Age group") +
  facet_grid(cols = vars(sex)) +
  scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y") +
  theme(legend.position="bottom", legend.title=element_text(size=13), 
        legend.text=element_text(size=13),
        axis.text = element_text(size = 13),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 13))

# ggsave(file= "km_plot.png",
#        path = paste0(output_dir, "figures"),
#        plot=p2, width=10, height=8)
dev.off()
