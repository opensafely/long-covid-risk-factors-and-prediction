# Purpose: to extract hazard ratios: 1) fully adjusted; 2) age-sex adjusted
# Programmed by Yinghui Wei
# Date: 2022-10-10

library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)
source("analysis/externals/ext_function_HR_df_v2.R")
common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"
results_dir = paste0(common_dir, "2022-10-10/")
output_dir <- paste0(common_dir, "2022-10-10/")

#-------------------------------------------------------------------------------------------------------------------
#- read in data
HR <- read.csv(paste0(results_dir, "HR_combined.csv"))

#- separate out data - full model with age splines
HR_full_model_age_spline_all.csv <- HR[HR$file =="full_model_age_spline_all",]                # primary
HR_full_model_age_spline_all_vax_c.csv <- HR[HR$file =="full_model_age_spline_all_vax_c",]    # pre-vax
HR_full_model_age_spline_vaccinated.csv <- HR[HR$file =="full_model_age_spline_vaccinated",]  # post-vax                                                # post-vax
HR_full_model_age_spline_infected.csv <- HR[HR$file =="full_model_age_spline_infected",]      # post-covid
HR_full_model_age_spline_all_vax_td.csv <- HR[HR$file =="full_model_age_spline_all_vax_td",]      # vaccination time-dependent

file_name1 <- c("HR_full_model_age_spline_all.csv", "HR_full_model_age_spline_all_vax_c.csv",
                "HR_full_model_age_spline_vaccinated.csv", "HR_full_model_age_spline_infected.csv",
                "HR_full_model_age_spline_all_vax_td.csv")

#- separate out data -  full model with age categorical
HR_full_model_categorical_all.csv <- HR[HR$file =="full_model_categorical_all",]                # primary
HR_full_model_categorical_all_vax_c.csv <- HR[HR$file =="full_model_categorical_all_vax_c",]    # pre-vax
HR_full_model_categorical_vaccinated.csv <- HR[HR$file =="full_model_categorical_vaccinated",]  # post-vax                                                # post-vax
HR_full_model_categorical_infected.csv <- HR[HR$file =="full_model_categorical_infected",]      # post-covid
HR_full_model_categorical_all_vax_td.csv <- HR[HR$file =="full_model_categorical_all_vax_td",]      # vaccination time-dependent

file_name2 <- c("HR_full_model_categorical_all.csv", "HR_full_model_categorical_all_vax_c.csv",
                "HR_full_model_categorical_vaccinated.csv", "HR_full_model_categorical_infected.csv",
                "HR_full_model_categorical_all_vax_td.csv")

#- separate out data - age-sex adjusted model
HR_age_sex_adjusted_all.csv        <- HR[HR$file =="age_sex_adjusted_all",]                # primary
HR_age_sex_adjusted_all_vax_c.csv  <- HR[HR$file =="age_sex_adjusted_all_vax_c",]          # pre-vax
HR_age_sex_adjusted_vaccinated.csv <- HR[HR$file =="age_sex_adjusted_vaccinated",]     # post-vax
HR_age_sex_adjusted_infected.csv   <- HR[HR$file =="age_sex_adjusted_infected",]       # post-covid
HR_age_sex_adjusted_all_vax_td.csv <- HR[HR$file =="age_sex_adjusted_all_vax_td",]         # vaccination time-dependent

file_name3 <- c("HR_age_sex_adjusted_all.csv", "HR_age_sex_adjusted_all_vax_c.csv",
                "HR_age_sex_adjusted_vaccinated.csv", "HR_age_sex_adjusted_infected.csv",
                "HR_age_sex_adjusted_all_vax_td.csv")

# - separate out data - age-sex model with age splines
HR_age_sex_model_all.csv <- HR[HR$file =="age_sex_model_all",]                    # primary
HR_age_sex_model_all_vax_c.csv  <- HR[HR$file =="age_sex_model_all_vax_c",]       # pre-vax
HR_age_sex_model_vaccinated.csv <- HR[HR$file =="age_sex_model_vaccinated",]      # post-vax
HR_age_sex_model_infected.csv   <- HR[HR$file =="age_sex_model_infected",]        # post-covid
HR_age_sex_model_all_vax_td.csv <- HR[HR$file =="age_sex_model_all_vax_td",]      # vaccination time-dependent

file_name4 <- c("HR_age_sex_model_all.csv", "HR_age_sex_model_all_vax_c.csv",
                "HR_age_sex_model_vaccinated.csv", "HR_age_sex_model_infected.csv",
                "HR_age_sex_model_all_vax_td.csv")

# - separate out data - age-sex model with categorical age
HR_age_sex_model_categorical_all.csv    <- HR[HR$file =="age_sex_model_categorical_all",]                 # primary
HR_age_sex_model_categorical_all_vax_c.csv  <- HR[HR$file =="age_sex_model_categorical_all_vax_c",]           # pre-vax
HR_age_sex_model_categorical_vaccinated.csv <- HR[HR$file =="age_sex_model_categorical_vaccinated",]      # post-vax
HR_age_sex_model_categorical_infected.csv   <- HR[HR$file =="age_sex_model_categorical_infected",]        # post-covid
HR_age_sex_model_categorical_all_vax_td.csv <- HR[HR$file =="age_sex_model_categorical_all_vax_td",]      # vaccination time-dependent

file_name5 <- c("HR_age_sex_categorical_all.csv", "HR_age_sex_categorical_all_vax_c.csv",
                "HR_age_sex_categorical_vaccinated.csv", "HR_age_sex_categorical_infected.csv",
                "HR_age_sex_categorical_all_vax_td.csv")

file_list <- c(file_name1, file_name2, file_name3, file_name4, file_name5)


#----------------------------------------------------------------------------------------------------------------------

df_list_full <- list(# full model with age splines
  HR_full_model_age_spline_all.csv,         # primary
  HR_full_model_age_spline_all_vax_c.csv,   # pre-vax
  HR_full_model_age_spline_vaccinated.csv,  # post-vax
  HR_full_model_age_spline_infected.csv,    # post-covid
  HR_full_model_age_spline_all_vax_td.csv   # vaccination time-dependent
)

df_list_full_categorical <- list(# full model with age categorical
  HR_full_model_categorical_all.csv,          # primary
  HR_full_model_categorical_all_vax_c.csv,    # pre-vax
  HR_full_model_categorical_vaccinated.csv,   # post-vax
  HR_full_model_categorical_infected.csv,     # post-covid
  HR_full_model_categorical_all_vax_td.csv    # vaccination time-dependent
)

df_list_age_sex_adjusted <- list(
  # age-sex adjusted model
  HR_age_sex_adjusted_all.csv,        # primary
  HR_age_sex_adjusted_all_vax_c.csv,  # pre-vax
  HR_age_sex_adjusted_vaccinated.csv, # post-vax
  HR_age_sex_adjusted_infected.csv,   # post-covid
  HR_age_sex_adjusted_all_vax_td.csv  # vaccination time-dependent
)

df_list_age_sex <-list(
  # age-sex model with age splines
  HR_age_sex_model_all.csv,        # primary
  HR_age_sex_model_all_vax_c.csv,  # pre-vax
  HR_age_sex_model_vaccinated.csv, # post-vax
  HR_age_sex_model_infected.csv,   # post-covid
  HR_age_sex_model_all_vax_td.csv  # vaccination time-dependent
)
df_list_age_sex_categorical <- list(
  # age-sex model with categorical age
  HR_age_sex_model_categorical_all.csv,        # primary
  HR_age_sex_model_categorical_all_vax_c.csv,  # pre-vax
  HR_age_sex_model_categorical_vaccinated.csv, # post-vax
  HR_age_sex_model_categorical_infected.csv,   # post-covid
  HR_age_sex_model_categorical_all_vax_td.csv  # vaccination time-dependent
) 

infected_cohort <- grep("infected",file_list)
td_analysis <- grep("td", file_list)

################################################################################
# Part 1. HR from fully adjusted models                                        #
################################################################################
#-------------------------------------------------------------------------------
# Primary cohort
#-------------------------------------------------------------------------------
csv_index = 1
cohort = "Primary"
df_hr_primary <- function_combine_hr(df_list_full, df_list_full_categorical, csv_index, cohort)
tbl_hr_primary <- function_HR_df_v2(df_hr_primary, csv_hr_order="hr_fixed2.csv",
                                    cohort = cohort, common_dir = common_dir)

#-------------------------------------------------------------------------------
# Pre-vaccination cohort
#-------------------------------------------------------------------------------
csv_index = 2
cohort = "Pre-vaccination"
df_hr_prevax <- function_combine_hr(df_list_full, df_list_full_categorical, csv_index, cohort)
tbl_hr_prevax <- function_HR_df_v2(df_hr_prevax, csv_hr_order="hr_fixed2.csv",
                                   cohort = cohort, common_dir = common_dir)

#-------------------------------------------------------------------------------
# Post-vaccination cohort
#-------------------------------------------------------------------------------
csv_index = 3
cohort = "Post-vaccination"
df_hr_vax <- function_combine_hr(df_list_full, df_list_full_categorical, csv_index, cohort)
tbl_hr_vax <- function_HR_df_v2(df_hr_vax, csv_hr_order="hr_fixed2.csv",
                                cohort = cohort, common_dir = common_dir)

#-------------------------------------------------------------------------------
# Post-COVID cohort
#-------------------------------------------------------------------------------
csv_index = 4
cohort = "Post-COVID"
df_hr_infected <- function_combine_hr(df_list_full, df_list_full_categorical, csv_index, cohort)
tbl_hr_infected <- function_HR_df_v2(df_hr_infected, csv_hr_order="hr_fixed2.csv",
                                     cohort = cohort, common_dir = common_dir)

#-------------------------------------------------------------------------------
# Primary cohort: Vaccination time-dependent
#-------------------------------------------------------------------------------
csv_index = 5
cohort = "Vaccination time-dependent"
df_hr_vaxtd <- function_combine_hr(df_list_full, df_list_full_categorical, csv_index, cohort)
tbl_hr_vaxtd <- function_HR_df_v2(df_hr_vaxtd, csv_hr_order="hr_fixed2.csv",
                                  cohort = cohort, common_dir = common_dir)

################################################################################
# Part 2. HR from age-sex adjusted models                                      #
################################################################################
# Primary cohort
csv_index = 1
cohort = "Primary"
df_hr_as_primary <- function_combine_as_hr_v2(df_list_age_sex_adjusted, df_list_age_sex, 
                                              df_list_age_sex_categorical, csv_index,
                                              cohort)
tbl_hr_as_primary <- function_HR_df_v2(df_hr_as_primary, csv_hr_order="hr_fixed2.csv",
                                       cohort = cohort, common_dir = common_dir)

tbl_hr_as_primary$age_sex_aHR = tbl_hr_as_primary$`HR (95% CI)`
# rename hazard ratio, lower and upper bounds of the 95%CI
tbl_hr_as_primary <- tbl_hr_as_primary %>% rename(as_hr = hazard_ratio) %>%
  rename(as_hr_low = conf.low) %>%
  rename(as_hr_high = conf.high)
tbl_hr_as_primary <- tbl_hr_as_primary %>% select(c(row.num, age_sex_aHR, as_hr, as_hr_low, as_hr_high))
tbl_hr_primary_combined <- merge(tbl_hr_primary, tbl_hr_as_primary, by="row.num", all.x=T)

# Pre-vaccination cohort
csv_index = 2
cohort = "Pre-Vaccination"
df_hr_as_prevax <- function_combine_as_hr_v2(df_list_age_sex_adjusted, df_list_age_sex, 
                                             df_list_age_sex_categorical, csv_index,
                                             cohort)
tbl_hr_as_prevax <- function_HR_df_v2(df_hr_as_prevax, csv_hr_order="hr_fixed2.csv",
                                      cohort = cohort, common_dir = common_dir)
tbl_hr_as_prevax$age_sex_aHR = tbl_hr_as_prevax$`HR (95% CI)`

# rename hazard ratio, lower and upper bounds of the 95%CI
tbl_hr_as_prevax <- tbl_hr_as_prevax %>% rename(as_hr = hazard_ratio) %>%
  rename(as_hr_low = conf.low) %>%
  rename(as_hr_high = conf.high)
tbl_hr_as_prevax <- tbl_hr_as_prevax %>% select(c(row.num, age_sex_aHR, as_hr, as_hr_low, as_hr_high))


tbl_hr_prevax_combined <- merge(tbl_hr_prevax, tbl_hr_as_prevax, by="row.num", all.x=T)

# Post-vaccination cohort
csv_index = 3
cohort = "Post-Vaccination"
df_hr_as_vax <- function_combine_as_hr_v2(df_list_age_sex_adjusted, df_list_age_sex, 
                                          df_list_age_sex_categorical, csv_index,
                                          cohort)
tbl_hr_as_vax <- function_HR_df_v2(df_hr_as_vax, csv_hr_order="hr_fixed2.csv",
                                   cohort = cohort, common_dir = common_dir)
tbl_hr_as_vax$age_sex_aHR = tbl_hr_as_vax$`HR (95% CI)`

# rename hazard ratio, lower and upper bounds of the 95%CI
tbl_hr_as_vax <- tbl_hr_as_vax %>% rename(as_hr = hazard_ratio) %>%
  rename(as_hr_low = conf.low) %>%
  rename(as_hr_high = conf.high)
tbl_hr_as_vax <- tbl_hr_as_vax %>% select(c(row.num, age_sex_aHR, as_hr, as_hr_low, as_hr_high))

tbl_hr_vax_combined <- merge(tbl_hr_vax, tbl_hr_as_vax, by="row.num", all.x=T)

# Post-COVID cohort
csv_index = 4
cohort = "Post-COVID"
df_hr_as_infected <- function_combine_as_hr_v2(df_list_age_sex_adjusted, df_list_age_sex, 
                                               df_list_age_sex_categorical, csv_index,
                                               cohort)
tbl_hr_as_infected <- function_HR_df_v2(df_hr_as_infected, csv_hr_order="hr_fixed2.csv",
                                        cohort = cohort, common_dir = common_dir)
tbl_hr_as_infected$age_sex_aHR = tbl_hr_as_infected$`HR (95% CI)`

# rename hazard ratio, lower and upper bounds of the 95%CI
tbl_hr_as_infected <- tbl_hr_as_infected %>% rename(as_hr = hazard_ratio) %>%
  rename(as_hr_low = conf.low) %>%
  rename(as_hr_high = conf.high)
tbl_hr_as_infected <- tbl_hr_as_infected %>% select(c(row.num, age_sex_aHR, as_hr, as_hr_low, as_hr_high))

tbl_hr_infected_combined <- merge(tbl_hr_infected, tbl_hr_as_infected, by="row.num", all.x=T)

# Vaccination time-dependent
csv_index = 5
cohort = "Vaccination time-dependent"
df_hr_as_vaxtd <- function_combine_as_hr_v2(df_list_age_sex_adjusted, df_list_age_sex, 
                                            df_list_age_sex_categorical, csv_index,
                                            cohort)
tbl_hr_as_vaxtd <- function_HR_df_v2(df_hr = df_hr_as_vaxtd, csv_hr_order="hr_fixed2.csv",
                                     cohort = cohort, common_dir = common_dir)
tbl_hr_as_vaxtd$age_sex_aHR = tbl_hr_as_vaxtd$`HR (95% CI)`

# rename hazard ratio, lower and upper bounds of the 95%CI
tbl_hr_as_vaxtd <- tbl_hr_as_vaxtd %>% rename(as_hr = hazard_ratio) %>%
  rename(as_hr_low = conf.low) %>%
  rename(as_hr_high = conf.high)
tbl_hr_as_vaxtd <- tbl_hr_as_vaxtd %>% select(c(row.num, age_sex_aHR, as_hr, as_hr_low, as_hr_high))


tbl_hr_vaxtd_combined <- merge(tbl_hr_vaxtd, tbl_hr_as_vaxtd, by="row.num", all.x=T)
