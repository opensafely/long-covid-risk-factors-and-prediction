# Purpose: Forest plot for hazard ratios with numerical numbers
# Programmed by Yinghui Wei
# 2022-10-18
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
source("analysis/externals/ext_figure_hazard_ratio_read_data_for_v12_plot.R")

#################################################################################
## Part 1. Version 12                                                         ##
#################################################################################
# Version 12: Demographics one graph; non-demographics another graph
#             fully adjusted HR on the plot; 
#             and age-sex-adjusted HR on a separate plot

# Set-up theme
tm2 <- forest_theme(base_size = 10,
                    refline_lty = "solid",
                    ci_pch = c(15,18),
                    ci_col = c("#377eb8", "#4daf4a"),
                    footnote_col = "blue",
                    legend_name = "Cox Model", legend_position = "bottom",
                    legend_value = c("Fully adjusted   ", "Age and sex adjusted"),
                    # vertline_lty = c("dashed", "dotted"),
                    # vertline_col = c("#d6604d", "#bababa")
                    vertline_lty = rep("dashed", 4),
                    vertline_col = rep("#d6604d", 4),
                    plot.margin=grid::unit(c(0,0,0,0), "mm")
)


v12_plot <- function(df,var_grp, cohort, model){
  # show in order: 1 Characteristic, 21 Space for CI, 8 = HR.x, 
  # 23 - small space; 22 - post-vaccination; 12 = HR.y
  # 19 age-sex adjusted HR; 20 age-sex adjusted HR;  17 small space
  
  df <- df %>% filter(variable==var_grp)
  if(model == "full"){
    col_to_display = c(1, 21, 8, 23,  22,  12) # 8 - fully adjusted HR; 12 - fully adjusted HR
    hr = list(df$hazard_ratio.x, df$hazard_ratio.y)
    hr_low = list(df$conf.low.x, df$conf.low.y) 
    hr_high = list(df$conf.high.x, df$conf.high.y)
    fig_lab = "Fully adjsuted"
  }
  if(model == "age_sex"){
    col_to_display = c(1, 21, 19, 23, 22,  20) # 13 - age-sex adjusted HR; # 14 - age-sex adjusted HR
    hr = list(df$as_hr.x, df$as_hr.y)
    hr_low = list(df$as_hr_low.x, df$as_hr_low.y) 
    hr_high = list(df$as_hr_high.x, df$as_hr_high.y)
    fig_lab = "Age and sex adjsuted"
  }
  p <- forest(
    #df[,c(1, 13, 8, 16, 15,  14,  12, 17)],  
    # 1-characteristic; 15-space for pre-vaccination CI; 8-HR.x; 13: age-sex adjusted HR pre-vax
    # 17: empty column space; 16: space for vax CI; 12: HR.y; 14: age-sex adjusted HR vax
    df[,col_to_display],  
    est = hr,
    lower = hr_low, 
    upper = hr_high,
    ci_column = c(2, 5),
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
  ggsave(file=paste0("v12_plot_HR_",cohort, "_", var_grp,"_", model, ".png"), path = paste0(output_dir, "figures"),
         plot=p, width=25, height=15)
  p
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
                    vertline_col = c("#d6604d", "#bababa"),
                    plot.margin=grid::unit(c(0,0,0,0), "mm")
                    )


v12_plot_one_cohort <- function(df,var_grp, cohort, model){
  # show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x, and 14 Space for CI, 12 = HR.y
  # 16 age-sex adjusted HR; 17 age-sex adjusted HR
  
  df <- df %>% filter(variable==var_grp)
  if(model == "full"){
    col_to_display = c(1, 13, 8) # 8 - fully adjusted HR; 12 - fully adjusted HR
    hr = df$hazard_ratio
    hr_low = df$conf.low
    hr_high = df$conf.high
  }
  if(model == "age_sex"){
    col_to_display = c(1, 13, 12) # 13 - age-sex adjusted HR; # 14 - age-sex adjusted HR
    hr = df$as_hr
    hr_low = df$as_hr_low
    hr_high = df$as_hr_high
  }
  p <- forest(
    # 1-characteristic; 10-space for CI; 8-HR;9: age-sex adjusted HR pre-vax
    #df[,c(1, 10, 8, 9)],  
    df[,col_to_display],
    est   = df$hazard_ratio,
    lower = df$conf.low,
    upper = df$conf.high,
    ci_column = 2,
    ref_line = 1,
    x_trans = "log10",
    vert_line = c(0.5, 2),
    xlim = c(0, 10),
    ticks_digits = 2L,
    ticks_at = c(0.1, 0.25, 0.5, 1, 2, 6, 10),
    nudge_y = 0.2,
    xlab = "\nFully adjusted hazard ratio",
    theme = tm1)
  ggsave(file=paste0("v12_plot_HR_",cohort, "_", var_grp,"_", model,".png"), 
         path = paste0(output_dir, "figures"),
         plot=p, width=25, height=15)
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
df$`                                   Pre-Vaccination` <- paste(rep(" ", 60), collapse = " ")
df$`                                   Post-Vaccination` <- paste(rep(" ", 60), collapse = " ")
df$` ` <- paste(rep(" ", 10), collapse = " ")

df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

df <- df %>% rename("Fully aHR (95% CI)" = "HR (95% CI)") %>%
  rename("Fully aHR (95% CI) " = "HR (95% CI) ") %>%
  rename("Age-sex aHR (95% CI)" = age_sex_aHR.x) %>%
  rename("Age-sex aHR (95% CI) " = age_sex_aHR.y) 

model = "full"
p1 <- v12_plot(df,var_grp="demographics", cohort, model)
p2 <- v12_plot(df,var_grp="non_demographics", cohort, model)

model = "age_sex"
p3 <- v12_plot(df,var_grp="demographics", cohort, model)
p4 <- v12_plot(df,var_grp="non_demographics", cohort, model)

# create two panel plots
var_grp = "demographics"
p <- grid.arrange(p3, p1, ncol=1)
ggsave(file=paste0("v12_two_panel_plot_HR_",cohort,"_", var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p, width=30, height=20)

var_grp = "non_demographics"
p <- grid.arrange(p4, p2, ncol=1)
ggsave(file=paste0("v12_two_panel_plot_HR_",cohort,"_" , var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p, width=30, height=20)

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
df$`                                  Primary` <- paste(rep(" ", 60), collapse = " ")
df$`                                  Post-COVID` <- paste(rep(" ", 60), collapse = " ")
#df$` ` <- paste(rep(" ", nrow(df)), collapse = " ") # add empty space
df$` ` <- paste(rep(" ", 10), collapse = " ")

# df <- df %>% filter(variable=="Demographics")
df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

## created for the purpose of plot exercise - to be replaced by the real estimates
# df$age_sex_aHR <- df$`HR (95% CI)`
# df$age_sex_aHR2 <- df$`HR (95% CI) `

df <- df %>% rename("Fully aHR (95% CI)" = "HR (95% CI)") %>%
  rename("Fully aHR (95% CI) " = "HR (95% CI) ") %>%
  rename("Age-sex aHR (95% CI)" = age_sex_aHR.x) %>%
  rename("Age-sex aHR (95% CI) " = age_sex_aHR.y) 

model = "full"
p1 <- v12_plot(df,var_grp="demographics", cohort, model)
p2 <- v12_plot(df,var_grp="non_demographics", cohort, model)

model = "age_sex"
p3 <- v12_plot(df,var_grp="demographics", cohort, model)
p4 <- v12_plot(df,var_grp="non_demographics", cohort, model)

# create two panel plots
var_grp = "demographics"
p <- grid.arrange(p3, p1, ncol=1)
ggsave(file=paste0("v12_two_panel_plot_HR_",cohort,"_", var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p, width=30, height=20)

var_grp = "non_demographics"
p <- grid.arrange(p4, p2, ncol=1)
ggsave(file=paste0("v12_two_panel_plot_HR_",cohort,"_" , var_grp,".png"), 
       path = paste0(output_dir, "figures"),
       plot=p, width=30, height=20)

#################################################################################
## Part 4. Vaccination time-dependent                                          ##
#################################################################################

cohort = "Vaccination time-dependent"

df <- tbl_hr_vaxtd_combined

df <- arrange(df,row.num)

# replace na with empty space
df[is.na(df$age_sex_aHR),"age_sex_aHR"] <- ""
df[is.na(df$`HR (95% CI)`),"HR (95% CI)"] <- ""


df <- df %>% select(c(term, row.num, variable, subgroup, hazard_ratio, conf.low, conf.high,
                      "HR (95% CI)", hazard_ratio, conf.low, conf.high, "HR (95% CI)",
                      as_hr, as_hr_low, as_hr_high, age_sex_aHR)) %>%
  rename(variable = variable) %>% rename(subgroup = subgroup) %>%
  rename(Characteristic = term) 

# Add two blank column for CI
df$`Vaccination time-dependent` <- paste(rep(" ", 60), collapse = " ")
df$` ` <- paste(rep(" ", 10), collapse = " ")

df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

df <- df %>% rename("Fully aHR (95% CI)" = "HR (95% CI)") %>%
  rename("Age-sex aHR (95% CI)" = age_sex_aHR)

df = df
var_grp="demographics"
cohort = "vtd"
model = "full"
v12_plot_one_cohort(df,var_grp="demographics", cohort, model)
v12_plot_one_cohort(df,var_grp="non_demographics", cohort, model)

model = "age_sex"
v12_plot_one_cohort(df,var_grp="demographics", cohort, model)
v12_plot_one_cohort(df,var_grp="non_demographics", cohort, model)