# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: cumulative probability of long COVID by age and  sex
# Output:  Two KM plots by age and sex
library(readr); library(dplyr);library(survival);library(survminer);library(tidyverse); library(ggplot2)
source("analysis/functions/function_round_km.R")
fs::dir_create(here::here("output", "review", "descriptives"))
cohort = "all"
input <- read_rds(paste0("output/input_stage1_", cohort,".rds"))
input <- input %>% select(lcovid_surv, lcovid_cens, contains("sex"), contains("cov_cat_age"))

levels(input$cov_cat_age_group) <- c("18-39", "40-59", "60-79", "80+")
table(input$cov_cat_age_group)
table(input$cov_cat_sex)

# Male: Create KM data -----------------------------------------------------------------
input_sex_m <- input %>% filter(cov_cat_sex == "M")
dat_age <- round_km(input_sex_m, "lcovid_surv", "lcovid_cens", "cov_cat_age_group", threshold=6)
dat_age$x <- dat_age$y <- NA
for(i in c("18-39", "40-59","60-79", "80+")){
  dat_age$x[which(dat_age$cov_cat_age_group ==i)] <- seq(length=length(which(dat_age$cov_cat_age_group ==i))) #Add case numbers (in order, since sorted)
  dat_age$y[which(dat_age$cov_cat_age_group ==i)] <- cumsum(replace_na(dat_age$n.event[which(dat_age$cov_cat_age_group ==i)], 0))
}
dat_age_m <- dat_age
dat_age_m$sex <- "Male"

# Female: Create KM data ----------------------------------------------------------------
input_sex_f <- input %>% filter(cov_cat_sex == "F")
dat_age <- round_km(input_sex_f, "lcovid_surv", "lcovid_cens", "cov_cat_age_group", threshold=6)
dat_age$x <- dat_age$y <- NA
for(i in c("18-39", "40-59","60-79", "80+")){
  dat_age$x[which(dat_age$cov_cat_age_group ==i)] <- seq(length=length(which(dat_age$cov_cat_age_group ==i))) #Add case numbers (in order, since sorted)
  dat_age$y[which(dat_age$cov_cat_age_group ==i)] <- cumsum(replace_na(dat_age$n.event[which(dat_age$cov_cat_age_group ==i)], 0))
}
dat_age_f <- dat_age
dat_age_f$sex <- "Female"
dat_age <- rbind(dat_age_m, dat_age_f)
  
# Create the cumulative incidence plot -----------------------------------------------------------
svglite::svglite("output/review/descriptives/figure_cum_incidence_age_sex.svg", width = 9, height = 5,)

ggplot(dat_age, 
       aes(x,y, group = cov_cat_age_group, color=cov_cat_age_group, linetype = cov_cat_age_group)) +
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
  labs(title="",x="\nDays since 1 December 2020", y = "\nCumulative incidence of long COVID\n") +
  labs(linetype='Age group', colour="Age group") +
  facet_grid(cols = vars(sex)) +
  theme(legend.position="bottom", legend.title=element_text(size=13), 
        legend.text=element_text(size=13),
        axis.text = element_text(size = 13),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()
  