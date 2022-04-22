## Purpose: Long COVID risk factors and prediction models
## Author:  Yinghui Wei
## Content: Prepare variables
## Output:  input_stage0.rds

## naming principles: 
## 1. variable names starting with "cov_cat_" indicate categorical covariates
## 2. variable names starting with "cov_num_" indicate numerical covariates
## 3. variable names starting with "out_" indicate outcome variables
## 4. variable names containing "_date" indicate date variables
## 5. variable names starting with "sub_" indicate patient characteristics which are not included
##    in the analysis / modelling, but are of interest in exploration

library(readr); library(dplyr); library("arrow"); library(lubridate); library(tidyr)

input <- read_feather("output/input.feather")
#View(input)

## define cohort start date:
index_date=as.Date("2020-12-01")
input$index_date = as.Date(index_date)

# Step 1. Define variables: COVID infection, long COVID-------------------------
# create an indicator variable for covid infection
input$out_covid <- ifelse(is.na(input$out_covid_date), FALSE, TRUE)

## create a categorical variable to indicate covid phenotype: -------------------
## no infection, non-hospitalised covid and hospitalised covid
## this variable is not included in the Cox model but will create and keep it for now
input$sub_cat_covid_phenotype <- ifelse(is.na(input$out_covid_date), 
                                        "no_infection", "non_hospitalised")
index = which(!is.na(input$hospital_covid))  # index for hospitalised covid
input$sub_cat_covid_phenotype[index] <- "hospitalised"

## Step 2. Remove variables which are not included in the prediction---------------
## remove variables start with snomed
snomed_vars <- names(input)[which(grepl("snomed", names(input))==TRUE)]
input = input[,!(names(input) %in% snomed_vars)]

tmp_vars <- names(input)[which(grepl("tmp", names(input))==TRUE)]
input = input[,!(names(input) %in% tmp_vars)]
vars_to_drop <- c("sgss_positive", "sgss_positive", "primary_care_covid", "hospital_covid",
                  "primary_care_death_date",  "ons_died_from_any_cause_date", 
                  "first_post_viral_fatigue_date")
input = input[,!(names(input) %in% vars_to_drop)]

## partial sorting by variable names in the data frame, keep patient_id and practice_id at the front
input <- input %>% dplyr::select(patient_id, practice_id, index_date, death_date,
                          colnames(input)[grepl("out_",colnames(input))],
                          colnames(input)[grepl("vax_",colnames(input))],
                          sort(tidyselect::peek_vars()))

## Step 3. define variable types: factor or numerical or date--------------------
# For categorical factors, specify references-----------------------------------
cat_factors <- colnames(input)[grepl("_cat_",colnames(input))]
input[,cat_factors] <- lapply(input[,cat_factors], function(x) factor(x, ordered = FALSE))

lapply(input[,cat_factors], is.factor)

## cov_cat_imd by quintile-------------------------------------------------------
table(input$cov_cat_imd)
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==0] <-"0 (missing)"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==1] <-"1 (most deprived)"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==2] <-"2"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==3] <-"3"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==4] <-"4"
levels(input$cov_cat_imd)[levels(input$cov_cat_imd)==5] <-"5 (least deprived)"

## ordered categorical factor, the first level is the reference
input$cov_cat_imd <- ordered(input$cov_cat_imd, 
                             levels = c("1 (most deprived)","2","3","4","5 (least deprived)", "0 (missing)"))

## for ordered factor, the first level is taken as reference level

## cov_cat_smoking_status-------------------------------------------------------
table(input$cov_cat_smoking_status)
levels(input$cov_cat_smoking_status) <- list("Ever smoker" = "E", "Missing" = "M", "Never smoker" = "N", "Current smoker" = "S")
input$cov_cat_smoking_status <- ordered(input$cov_cat_smoking_status, levels = c("Never smoker","Ever smoker","Current smoker","Missing"))
table(input$cov_cat_smoking_status)

## cov_cat_age_group-------------------------------------------------------------
## Define age groups
input$cov_cat_age_group <- "<=18"
input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_105", input$cov_cat_age_group)
input$cov_cat_age_group <- factor(input$cov_cat_age_group, ordered = TRUE)


################################################################################
## Part 3. define multimorbidity: need to be carefully checked                 #
################################################################################

condition_names <- names(input)[grepl("cov_cat", names(input))]
not_a_condition <- c("cov_cat_age_group", "cov_cat_sex","cov_cat_healthcare_worker",
                     "cov_cat_imd","cov_cat_region","cov_cat_smoking_status",
                     "cov_cat_covid_phenotype", "cov_cat_previous_covid",
                     "cov_cat_ethnicity")

condition_names <- condition_names[!condition_names%in%not_a_condition]

input_select <- input %>% dplyr::select(c(patient_id, condition_names))
input_select<- as_tibble(
  data.matrix(input_select)
)

for(i in condition_names){
  input_select[, i] =ifelse(input_select[,i]==1, 0, 1) 
}

input_select <- input_select %>% mutate(cov_num_multimorbidity = rowSums(.[ , condition_names]))

input_select <- input_select %>% mutate(cov_cat_multimorbidity =ifelse(cov_num_multimorbidity>=2, 2,
                                                                       ifelse(cov_num_multimorbidity==1, 1, 0)))

input_select <- input_select %>% dplyr::select(c(patient_id, cov_cat_multimorbidity))

#-----------------end of definition for multimorbidity -------------------------

## left join: keep all observations in input_select
input <- merge(x = input_select, y = input, by = "patient_id", all.x = TRUE)

rm(input_select)

## View(input_select)


## specify date variables in the format of "%Y-%m-%d"----------------------------
vars_dates <- grep("date", names(input))
vars_dates <- names(input)[vars_dates]

convert_to_date <- function(x){
  as.Date(x,format = "%Y-%m-%d")
}
input[vars_dates] = lapply(input[vars_dates], convert_to_date)
lapply(input[vars_dates], is.Date)

## define a variable covid_history to indicate if individuals have covid infection before the start of the cohort
input$sub_cat_covid_history <-ifelse(input$out_covid_date < input$index_date, TRUE, FALSE)

select_variables <- input %>% dplyr::select(c(sub_cat_covid_history, out_covid_date, index_date))

## store names of factor variables
cov_factor_names <- names(input)[grepl("_cat", names(input))]

input_factor_vars <- input[,cov_factor_names]

lapply(input[,cov_factor_names], is.factor)
lapply(input_factor_vars, is.factor)

## Table_0 is mainly for data check 
output_table_0 <- function(input_factor_vars){
    ## Summary of categorical variables
    print("construct table_0 for data check!")
    table_0<- data.frame()
    for(i in cov_factor_names){
      print(table(input_factor_vars[,i]))
      table_0[nrow(table_0)+1,1] <- i
      table_0[nrow(table_0),2] <- length(which(is.na(input_factor_vars[,i])))
      #table_0[nrow(table_0),3] <- is.factor(input_factor_vars[,i])
      table_0[nrow(table_0),3] <- NA
      len = length(names(table(input_factor_vars[,i])))
      index = nrow(table_0)+1
      for(j in 1:len){
        table_0[index-1,4+j-1]<-names(table(input_factor_vars[,i]))[j]
        table_0[index, 4+j-1] <-table(input_factor_vars[,i])[j]
        if(table(input_factor_vars[,i])[j] <=5){
          table_0[index, 4+j-1] <- "[redacted]"
        }
      }
    }
    names(table_0) <- c("factor variables", "number of missing observations", "is.factor")
    names(table_0)[4:ncol(table_0)] = rep("level name or number of observations", (ncol(table_0)-3))
    
    #table_0
    
    print("Finished constructing table_0 successfully!")
    
    write.csv(table_0, file="output/table_0.csv", row.names =F)
    rmarkdown::render("analysis/compiled_table0_results.Rmd",
                      output_file="table_0",output_dir="output")
}


# # For categorical variables, replace "na" with "Missing" as a category

## impose some NA and check if this works
#input_factor_vars$cov_cat_region[1:10] =NA
print("Replace missing values with a Missing category!")

## mutate is much quicker
input_factor_vars <- input_factor_vars %>% mutate(cov_cat_region = as.character(cov_cat_region)) %>%
  mutate(cov_cat_region = replace_na(cov_cat_region, "Missing")) %>%
  mutate(cov_cat_region = as.factor(cov_cat_region))

## cov_cat_smoking_status
input_factor_vars <- input_factor_vars %>% mutate(cov_cat_smoking_status = as.character(cov_cat_smoking_status)) %>%
  mutate(cov_cat_smoking_status = replace_na(cov_cat_smoking_status, "Missing")) %>%
  mutate(cov_cat_smoking_status = as.factor(cov_cat_smoking_status))

## sub_cat_covid_history: two categories: false and missing, as patients with covid history was excluded
input_factor_vars <- input_factor_vars %>% mutate(sub_cat_covid_history = as.character(sub_cat_covid_history)) %>%
  mutate(sub_cat_covid_history = replace_na(sub_cat_covid_history, "Missing")) %>%
  mutate(sub_cat_covid_history = as.factor(sub_cat_covid_history))

# for(i in cov_factor_names){
#   print(i)
#   index = which(is.na(input_factor_vars[,i]))
#   if(length(index)>0){
#     input_factor_vars[,i] = as.character(input_factor_vars[,i]) # convert factor to character to allow to change na to "Missing"
#     input_factor_vars[index,i]="Missing"
#     input_factor_vars[,i] = as.factor(input_factor_vars[,i]) # convert back to factor
#   }
# 
#   #print(table(input_factor_vars[,i]))
# }

print("Fished replacing missing values with a Missing category!")

input_factor_vars$cov_cat_region <- relevel(input_factor_vars$cov_cat_region, ref = "London")

table(input_factor_vars$cov_cat_region)

print("Finished replacing missing values with a Missing category successfully!")

input[,cov_factor_names] <- input_factor_vars

lapply(input[,cov_factor_names], is.factor)

print("checking after replacing na with missing!") 
output_table_0(input)

for(i in cov_factor_names){
  print(table(input[,i]))
}

saveRDS(input, file = "output/input_stage0.rds")
