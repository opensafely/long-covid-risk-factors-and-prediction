# Purpose: to extract hazard ratios: 1) fully adjusted; 2) age-sex adjusted
# Programmed by Yinghui Wei
# Date: 2022-11-07

library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)

common_dir = "C:/Users/ywei3/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"

results_dir = paste0(common_dir, "2023-04-15/")
output_dir <- paste0(common_dir, "2023-04-15/")

cohort = "all"
#cohort = "infected"

# km_plot <- function(results_dir, output_dir, cohort){
  print(cohort)
  dat_age <- read.csv(paste0(results_dir, file=paste0("tbl_km_dat_age_",cohort,".csv")))
  dat_age <- dat_age %>% mutate(x_date = as.Date(x_date))
  # Create cumulative incidence plot -----------------------------------------------------------
  svglite::svglite(paste0(results_dir,"figures/km_incipence_plot_", cohort,".svg"), width = 10, height = 8)
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
  svglite::svglite(paste0(results_dir, "figures/km_plot_",cohort,".svg"), width = 10, height = 8)
  
  ggplot(dat_age, 
         aes(x_date,y_prob, group = cov_cat_age_group, color=cov_cat_age_group, 
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
# }
  
  sub_dat <- dat_age %>% filter(cov_cat_age_group == "40-59")
  index_m <- which(sub_dat$sex == "Male")
  index_f <- which(sub_dat$sex == "Female")
  
  summary(sub_dat$y_prob[index_m])
  summary(sub_dat$y_prob[index_f])
  
# cohort = "all"
# km_plot(results_dir, output_dir, cohort)