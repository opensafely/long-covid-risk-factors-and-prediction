# Purpose: Forest plot for hazard ratios with numerical numbers
# Programmed by Yinghui Wei
# 2022-09-06
# Use Alimu Dyimu's package: 
# https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
# https://github.com/adayim/forestploter/blob/dev/tests/testthat/test-forest.R

#install.packages("devtools")
library(devtools)
#devtools::install_github("adayim/forestploter@dev")
library(forestploter)

library(grid)
library(dplyr)
source("analysis/externals/ext_figure_hazard_ratio_read_data.R")

#################################################################################
## Part 1. Version 10                                                          ##
#################################################################################
# Version 10: Demographics one graph; non-demographics another graph
#            with HR and 95%CI for age-sex adjsuted HR and fully adjusted HR on the plot

# Set-up theme
tm2 <- forest_theme(base_size = 10,
                    refline_lty = "solid",
                    ci_pch = c(15,18),
                    ci_col = c("#377eb8", "#4daf4a"),
                    footnote_col = "blue",
                    legend_name = "Cox Model", legend_position = "bottom",
                    legend_value = c("Fully adjusted   ", "Age and sex adjusted"),
                    vertline_lty = c("dashed", "dotted"),
                    vertline_col = c("#d6604d", "#bababa"))


v10_plot <- function(df,var_grp, cohort){
  # show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x, and 14 Space for CI, 12 = HR.y
  # 16 age-sex adjusted HR; 17 age-sex adjusted HR
  
  df <- df %>% filter(variable==var_grp)
  p <- forest(
             #df[,c(1, 13, 8, 16, 15,  14,  12, 17)],  
    # 1-characteristic; 15-space for pre-vaccination CI; 8-HR.x; 13: age-sex adjusted HR pre-vax
    # 17: empty column space; 16: space for vax CI; 12: HR.y; 14: age-sex adjsuted HR vax
             df[,c(1, 15, 8, 13, 17,  16,  12, 14)],  
              est = list(df$hazard_ratio.x,
                         df$hazard_ratio.y),
              lower = list(df$conf.low.x,
                           df$conf.low.y), 
              upper = list(df$conf.high.x,
                           df$conf.high.y),
              ci_column = c(2, 6),
              ref_line = 1,
              x_trans = c("log10", "log10"),
              vert_line = c(0.5, 2),
              xlim = c(0, 10),
              ticks_at = c(0.1, 0.5, 1, 2, 6, 10),
              nudge_y = 0.2,
              xlab = c("\nFully adjusted hazard ratio", "\nFully adjusted hazard ratio"),
              theme = tm2)
  ggsave(file=paste0("v10_plot_HR_",cohort, "_", var_grp,".svg"), path = paste0(output_dir, "figures"),
         plot=p, width=30, height=20)
}

# Set-up theme for one cohort
tm1 <- forest_theme(base_size = 10,
                    refline_lty = "solid",
                    ci_pch = 15,
                    ci_col = c("#377eb8", "#4daf4a"),
                    footnote_col = "blue",
                    legend_name = "Cox Model", legend_position = "bottom",
                    legend_value = c("Fully adjusted   ", "Age and sex adjusted"),
                    vertline_lty = c("dashed", "dotted"),
                    vertline_col = c("#d6604d", "#bababa"))


v10_plot_one_cohort <- function(df,var_grp, cohort){
  # show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x, and 14 Space for CI, 12 = HR.y
  # 16 age-sex adjusted HR; 17 age-sex adjusted HR
  
  df <- df %>% filter(variable==var_grp)
  p <- forest(
    # 1-characteristic; 10-space for CI; 8-HR;9: age-sex adjusted HR pre-vax
    df[,c(1, 10, 8, 9)],  
    est = df$hazard_ratio,
    lower = df$conf.low,
    upper = df$conf.high,
    ci_column = 2,
    ref_line = 1,
    x_trans = "log10",
    vert_line = c(0.5, 2),
    xlim = c(0, 10),
    ticks_at = c(0.1, 0.5, 1, 2, 6, 10),
    nudge_y = 0.2,
    xlab = "\nFully adjusted hazard ratio",
    theme = tm1)
  ggsave(file=paste0("v10_plot_HR_",cohort, "_", var_grp,".svg"), path = paste0(output_dir, "figures"),
         plot=p, width=30, height=20)
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
                      age_sex_aHR.x, age_sex_aHR.y)) %>%
  rename(variable = variable.x) %>% rename(subgroup = subgroup.x) %>%
  rename("HR (95% CI)" = "HR (95% CI).x") %>%
  rename("HR (95% CI) " = "HR (95% CI).y")%>%
  rename(Characteristic = term) 

# Add two blank column for CI
df$`Pre-Vaccination` <- paste(rep(" ", 25), collapse = " ")
df$`Post-Vaccination` <- paste(rep(" ", 25), collapse = " ")
df$` ` <- paste(rep(" ", 5), collapse = " ")

df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

df <- df %>% rename("Fully aHR (95% CI)" = "HR (95% CI)") %>%
  rename("Fully aHR (95% CI) " = "HR (95% CI) ") %>%
  rename("Age-sex aHR (95% CI)" = age_sex_aHR.x) %>%
  rename("Age-sex aHR (95% CI) " = age_sex_aHR.y) 


v10_plot(df,var_grp="demographics", cohort)
v10_plot(df,var_grp="non_demographics", cohort)


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
                      age_sex_aHR.x, age_sex_aHR.y)) %>%
  rename(variable = variable.y) %>% rename(subgroup = subgroup.y) %>%
  rename("HR (95% CI)" = "HR (95% CI).x") %>%
  rename("HR (95% CI) " = "HR (95% CI).y")%>%
  rename(Characteristic = term) 

# Add two blank column for CI
df$`   Primary` <- paste(rep(" ", 25), collapse = " ")
df$`   Post-COVID` <- paste(rep(" ", 25), collapse = " ")
#df$` ` <- paste(rep(" ", nrow(df)), collapse = " ") # add empty space
df$` ` <- paste(rep(" ", 5), collapse = " ")

# df <- df %>% filter(variable=="Demographics")
df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

## created for the purpose of plot exercise - to be replaced by the real estimates
# df$age_sex_aHR <- df$`HR (95% CI)`
# df$age_sex_aHR2 <- df$`HR (95% CI) `

df <- df %>% rename("Fully aHR (95% CI)" = "HR (95% CI)") %>%
  rename("Fully aHR (95% CI) " = "HR (95% CI) ") %>%
  rename("Age-sex aHR (95% CI)" = age_sex_aHR.x) %>%
  rename("Age-sex aHR (95% CI) " = age_sex_aHR.y) 


v10_plot(df,var_grp="demographics", cohort)
v10_plot(df,var_grp="non_demographics", cohort)


#################################################################################
## Part 4. Vaccination time-dependent                                          ##
#################################################################################

cohort = "Vaccination time-dependent"

df <- tbl_hr_vaxtd_combined

df <- arrange(df,row.num)

# replace na with empty space
df[is.na(df$age_sex_aHR),"age_sex_aHR"] <- ""
df[is.na(df$`HR (95% CI)`),"HR (95% CI)"] <- ""


df <- df %>% select(c(term,row.num,variable, subgroup, hazard_ratio, conf.low, conf.high,
                      "HR (95% CI)", hazard_ratio, conf.low, conf.high, "HR (95% CI)",
                      age_sex_aHR, age_sex_aHR)) %>%
  rename(variable = variable) %>% rename(subgroup = subgroup) %>%
  rename(Characteristic = term) 

# Add two blank column for CI
df$`Vaccination time-dependent` <- paste(rep(" ", 25), collapse = " ")
df$` ` <- paste(rep(" ", 5), collapse = " ")

df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

df <- df %>% rename("Fully aHR (95% CI)" = "HR (95% CI)") %>%
  rename("Age-sex aHR (95% CI)" = age_sex_aHR)

df = df
var_grp="demographics"
cohort = "vtd"
v10_plot_one_cohort(df,var_grp="demographics", cohort)
v10_plot_one_cohort(df,var_grp="non_demographics", cohort)
