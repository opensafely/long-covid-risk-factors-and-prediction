# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: cumulative probability of long COVID by age and  sex
# Output:  Two KM plots by age and sex
library(readr); library(dplyr);library(survival);library(survminer);library(tidyverse); library(ggplot2)
source("analysis/functions/function_round_km.R")
fs::dir_create(here::here("output", "review", "descriptives"))
fs::dir_create(here::here("output", "not_for_review", "descriptives"))
cohort = "all"
input <- read_rds(paste0("output/input_stage1_", cohort,".rds"))
input <- input %>% dplyr::select(lcovid_surv, lcovid_cens, contains("sex"), contains("cov_cat_age"))

levels(input$cov_cat_age_group) <- c("18-39", "40-59", "60-79", "80+")
table(input$cov_cat_age_group)
table(input$cov_cat_sex)

index_date=as.Date("2020-01-29")
end_date = as.Date("2022-03-31")
# Define a function to extract KM data ---------------------------------------------------------
function_km_data <-function(input_sex, index_date){
  dat_age <- round_km2(input_sex, "lcovid_surv", "lcovid_cens", "cov_cat_age_group", threshold=6)
  dat_age$x <- dat_age$y <- dat_age$y_prob <-NA
  for(i in c("18-39", "40-59","60-79", "80+")){
    dat_age$x[which(dat_age$cov_cat_age_group ==i)] <- seq(length=length(which(dat_age$cov_cat_age_group ==i))) #Add case numbers (in order, since sorted)
    dat_age$y[which(dat_age$cov_cat_age_group ==i)] <- cumsum(replace_na(dat_age$n.event[which(dat_age$cov_cat_age_group ==i)], 0))
  }
  for(i in c("18-39", "40-59","60-79", "80+")){
    index = which(dat_age$cov_cat_age_group==i)
    dat_age_select <- dat_age %>% filter(cov_cat_age_group == i)
    dat_age_select$y_prob <- NA
    total = dat_age_select$n.risk[1]
    dat_age_select$y_prob = dat_age_select$y/total
    dat_age$y_prob[index] <- dat_age_select$y_prob
  }
  dat_age$x_date = index_date + dat_age$time
 # dat_age$x_date = index_date + dat_age$x
 dat_age <- dat_age %>% filter(n.risk >=0)
  return(dat_age)
}
# Male: Create KM data -----------------------------------------------------------------
input_sex_m <- input %>% filter(cov_cat_sex == "M")
dat_age_m <- function_km_data(input_sex_m, index_date)
dat_age_m$sex <- "Male"

# Female: Create KM data ----------------------------------------------------------------
input_sex_f <- input %>% filter(cov_cat_sex == "F")
dat_age_f <- function_km_data(input_sex_f, index_date) 
dat_age_f$sex <- "Female"
dat_age <- rbind(dat_age_m, dat_age_f)
# Create a supporting document to show evidence of rounding for small number suppression control
write.csv(dat_age,"output/review/descriptives/tbl_km_dat_age.csv", row.names=F)
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
svglite::svglite("output/review/descriptives/figure_kaplan_meier_age_sex_cum_porobability.svg", width = 9, height = 5)

ggplot(dat_age, 
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

dev.off()