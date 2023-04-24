# Purpose: Forest plot for hazard ratios with numerical numbers
# Programmed by Yinghui Wei
# 2022-10-31
# Use Alimu Dyimu's package: 
# https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
# https://github.com/adayim/forestploter/blob/dev/tests/testthat/test-forest.R

#install.packages("devtools")
#library(devtools)
#devtools::install_github("adayim/forestploter@dev")
library(forestploter)
library(grid)
library(gridExtra)
library(dplyr)
source("analysis/externals/ext_figure_hazard_ratio_read_data_for_v14_plot.R")

#################################################################################
## Part 1. Version 14                                                          ##
#################################################################################
# Version 14: Demographics one graph; non-demographics another graph
#             fully adjusted HR on the plot; 
#             and age-sex-adjusted HR on a separate plot

# Set-up theme
tm2 <- forest_theme(base_size = 10,
                    refline_lty = "solid",
                    ci_pch = c(5,15),
                    ci_col = c("#FF8000", "#377eb8"),
                    footnote_col = "blue",
                    legend_name = "Cox Model", legend_position = "bottom",
                    legend_value = c(" Age-and-sex adjusted   ", " Fully adjusted"),
                    # vertline_lty = c("dashed", "dotted"),
                    # vertline_col = c("#d6604d", "#bababa")
                    vertline_lty = rep("dashed", 4),
                    vertline_col = rep("#d6604d", 4)
)


v14_plot <- function(df,var_grp, cohort){
  # show in order: 1 Characteristic, 
  # 21 name of cohort A and space for CI,  # 19 - age-sex adjusted HR; 8 - fully adjusted HR;
  # 23 - small space; 
  # 22 - name of cohort  B and space for CI; # 20 age-sex adjusted HR; 12 fully adjusted HR;  
  
  df <- df %>% filter(variable==var_grp)

  col_to_display = c(1, 21, 19, 8, 23,  22,  20, 12) # column in data frame

  hr = list(df$as_hr.x,         # age-and-sex adjusted HR, cohort A
            df$as_hr.y,         # age-and-sex adjusted HR, cohort B
            df$hazard_ratio.x,  # fully adjusted HR, cohort A
            df$hazard_ratio.y   # fully adjusted HR, cohort B
            )
  hr_low = list(df$as_hr_low.x,  # lower bound of 95%CI
                df$as_hr_low.y,
                df$conf.low.x, 
                df$conf.low.y
                ) 
  hr_high = list(df$as_hr_high.x,  # upper bound of 95%CI
                df$as_hr_high.y,
                df$conf.high.x, 
                df$conf.high.y
                )
  p <- forest(
    #df[,c(1, 13, 8, 16, 15,  14,  12, 17)],  
    # 1-characteristic; 15-space for pre-vaccination CI; 8-HR.x; 13: age-sex adjusted HR pre-vax
    # 17: empty column space; 16: space for vax CI; 12: HR.y; 14: age-sex adjusted HR vax
    df[,col_to_display],  
    est = hr,
    lower = hr_low, 
    upper = hr_high,
    sizes = 0.45,
    ci_column = c(2, 6),
    ref_line = 1,
    x_trans = c("log10", "log10"),
    vert_line = c(0.25, 0.5, 2, 4),
    xlim = c(0, 10),
    ticks_digits = 2L,
    ticks_at = c(0.1, 0.25, 0.5, 1, 2, 4, 10),
    nudge_y = 0.2,
    # xlab = c(paste0("\n", fig_lab, " hazard ratio"), 
    #          paste0("\n", fig_lab, " hazard ratio")),
    xlab = c("\nHazard ratio", "\nHazard ratio"),
    theme = tm2)
  # ggsave(file=paste0("v14_plot_HR_",cohort, "_", var_grp,"_", model, ".png"), path = paste0(output_dir, "figures"),
  #        plot=p, width=25, height=15)
  p
}

#################################################################################
## Part 2. Pre-vax and vax                                                     ##
#################################################################################
df <- merge(tbl_hr_prevax_combined, tbl_hr_vax_combined, by=c("term","row.num"), all=T)

cohort = "pre_vax_and_vax"

df <- arrange(df,row.num)

# replace na with empty space
df[is.na(df$age_sex_aHR.x),"age_sex_aHR.x"] <- ""
df[is.na(df$age_sex_aHR.y),"age_sex_aHR.y"] <- ""
df[is.na(df$`HR (95% CI).x`),"HR (95% CI).x"] <- ""
df[is.na(df$`HR (95% CI).y`),"HR (95% CI).y"] <- ""

df <- df %>% select(c(term,row.num,variable.x, subgroup.x, hazard_ratio.x, conf.low.x, conf.high.x,
                      "HR (95% CI).x", hazard_ratio.y, conf.low.y, conf.high.y, "HR (95% CI).y",
                      as_hr.x, as_hr_low.x, as_hr_high.x,
                      as_hr.y, as_hr_low.y, as_hr_high.y,
                      age_sex_aHR.x, age_sex_aHR.y)) %>%
  rename(variable = variable.x) %>% rename(subgroup = subgroup.x) %>%
  rename("HR (95% CI)" = "HR (95% CI).x") %>%
  rename("HR (95% CI) " = "HR (95% CI).y")%>%
  rename(Characteristic = term)

# Add two blank column for CI
df$`      Pre-vaccination` <- paste(rep(" ", 25), collapse = " ")
df$`      Post-vaccination` <- paste(rep(" ", 25), collapse = " ")
df$` ` <- paste(rep(" ", 0.01), collapse = " ")

df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

df <- df %>% rename("Fully aHR" = "HR (95% CI)") %>%
  rename("Fully aHR " = "HR (95% CI) ") %>%
  rename("Age-sex aHR" = age_sex_aHR.x) %>%
  rename("Age-sex aHR " = age_sex_aHR.y) 


#--Capitalise Region

vars_names_formatting <- function(df){
  # Region name
  df$Characteristic[which(df$Characteristic== "       East midlands")] = "       East Midlands"                                
  df$Characteristic[which(df$Characteristic== "       North east")] = "       North East"                                
  df$Characteristic[which(df$Characteristic== "       North west")] = "       North West"
  df$Characteristic[which(df$Characteristic== "       South east")] = "       South East"
  df$Characteristic[which(df$Characteristic== "       South west")] = "       South West"  
  df$Characteristic[which(df$Characteristic== "       West midlands")] = "       West Midlands" 
  df$Characteristic[which(df$Characteristic== "       Yorkshire and the humber")] = "       Yorkshire and the Humber" 
  
  # ethnic group name
  df$Characteristic[which(df$Characteristic== "       Asian or asian british")] = "       Asian or Asian British"                              
  df$Characteristic[which(df$Characteristic== "       Black or black british")] = "       Black or Black British"                                
  
  # GP-patient interaction
  df$Characteristic[which(df$Characteristic== "GP patient interaction")] = "GP-Patient interaction"                              
  
  # History of disease
  df$Characteristic[which(df$Characteristic== "History of diseases")] = "History of disease"                             
  
  # # BMI to obesity
  # df$Characteristic[which(df$Characteristic== "BMI")] = "Obesity" 
  # df$Characteristic[which(df$Characteristic== "       Obese i (30-34.9)")] = "       Obese class I" 
  # df$Characteristic[which(df$Characteristic== "       Obese ii (35-39.9)")] = "       Obese class II"
  # df$Characteristic[which(df$Characteristic== "       Obese iii (40+)")] = "       Obese class III"
  # 
  # Pre-pandemic post-viral fatigue
  df$Characteristic[which(df$Characteristic== "       Post viral fatigue pre pandemic" )] = "       Pre-pandemic post-viral fatigue" 
  
  # IMD
  df$Characteristic[which(df$Characteristic== "IMD" )] = "Deprivation (IMD) quintile" 
  
  # Non-hospitalised
  df$Characteristic[which(df$Characteristic== "       Non hospitalised (reference)")] = "       Non-hospitalised (reference)"
  df$Characteristic[which(df$Characteristic== "Covid phenotype")] = "COVID phenotype"
  
  df
}

df <- vars_names_formatting(df)
var_grp="demographics"
p1 <- v14_plot(df,var_grp="demographics", cohort)
ggsave(file=paste0("v14_plot_HR_",cohort,"_" , var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p1, width=11.5, height=13)

var_grp="non_demographics"
p2 <- v14_plot(df,var_grp="non_demographics", cohort)
ggsave(file=paste0("v14_plot_HR_",cohort,"_" , var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p2, width=12, height=12)

#################################################################################
## Part 3. Primary and infected                                                ##
#################################################################################
cohort = "primary_infected"

df <- merge(tbl_hr_primary_combined, tbl_hr_infected_combined, by=c("term","row.num"), all=T)

df <- arrange(df,row.num)

# replace na with empty space
df[is.na(df$age_sex_aHR.x),"age_sex_aHR.x"] <- ""
df[is.na(df$age_sex_aHR.y),"age_sex_aHR.y"] <- ""
df[is.na(df$`HR (95% CI).x`),"HR (95% CI).x"] <- ""
df[is.na(df$`HR (95% CI).y`),"HR (95% CI).y"] <- ""

df <- df %>% select(c(term,row.num,variable.y, subgroup.y, hazard_ratio.x, conf.low.x, conf.high.x,
                      "HR (95% CI).x", hazard_ratio.y, conf.low.y, conf.high.y, "HR (95% CI).y",
                      as_hr.x, as_hr_low.x, as_hr_high.x,
                      as_hr.y, as_hr_low.y, as_hr_high.y,
                      age_sex_aHR.x, age_sex_aHR.y)) %>%
  rename(variable = variable.y) %>% rename(subgroup = subgroup.y) %>%
  rename("HR (95% CI)" = "HR (95% CI).x") %>%
  rename("HR (95% CI) " = "HR (95% CI).y")%>%
  rename(Characteristic = term) 

# Add two blank column for CI
df$`              Primary` <- paste(rep(" ", 25), collapse = " ")
df$`      Post-COVID diagnosis` <- paste(rep(" ", 25), collapse = " ")
#df$` ` <- paste(rep(" ", nrow(df)), collapse = " ") # add empty space
df$` ` <- paste(rep(" ", 0.01), collapse = " ")

# df <- df %>% filter(variable=="Demographics")
df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

## created for the purpose of plot exercise - to be replaced by the real estimates
# df$age_sex_aHR <- df$`HR (95% CI)`
# df$age_sex_aHR2 <- df$`HR (95% CI) `

df <- df %>% rename("Fully aHR" = "HR (95% CI)") %>%
  rename("Fully aHR " = "HR (95% CI) ") %>%
  rename("Age-sex aHR" = age_sex_aHR.x) %>%
  rename("Age-sex aHR " = age_sex_aHR.y) 

df <- vars_names_formatting(df)
var_grp="demographics"
p3 <- v14_plot(df,var_grp="demographics", cohort)
ggsave(file=paste0("v14_plot_HR_",cohort,"_" , var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p3, width=11.5, height=13)

var_grp="non_demographics"
p4 <- v14_plot(df,var_grp="non_demographics", cohort)
ggsave(file=paste0("v14_plot_HR_",cohort,"_" , var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p4, width=12, height=13)
