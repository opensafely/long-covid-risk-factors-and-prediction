# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Prepare variables
# Output:  input.rds
library(readr); library(dplyr); library("arrow"); library("data.table")

input <- read_feather("output/input.feather")

setnames(input,
         old = c("sex",
                 "smoking_status",
                 "age_group",
                 "imd",
                 "ethnicity",
                 "bmi",
                 "diabetes",
                 "cancer",
                 "haem_cancer",
                 "asthma",
                 "chronic_respiratory_disease",
                 "chronic_cardiac_disease",
                 "chronic_liver_disease",
                 "stroke_or_dementia",
                 "other_neuro",
                 "organ_transplant",
                 "dysplenia",
                 "ra_sle_psoriasis",
                 "other_immunosuppressive_condition",
                 "heart_failure",
                 "post_viral_fatigue",
                 "region"
                 ),
         new = c("cov_cat_sex",
                 "cov_cat_smoking_status",
                 "cov_cat_age",
                 "cov_cat_imd",
                 "cov_cat_ethnicity",
                 "cov_num_bmi",
                 "cov_cat_diabetes",
                 "cov_cat_cancer",
                 "cov_cat_haem_cancer",
                 "cov_cat_asthma",
                 "cov_cat_chronic_respiratory_disease",
                 "cov_cat_chronic_cardiac_disease",
                 "cov_cat_chronic_liver_disease",
                 "cov_cat_stroke_or_dementia",
                 "cov_cat_other_neuro",
                 "cov_cat_organ_transplant",
                 "cov_cat_dysplenia",
                 "cov_cat_psoriasis",
                 "cov_cat_other_immunosuppressive_condition",
                 "cov_cat_heart_failure",
                 "cov_cat_post_viral_fatigue",
                 "cov_cat_region"))
View(input)
names(input)
