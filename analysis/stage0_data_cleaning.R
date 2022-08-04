## Purpose: Long COVID risk factors and prediction models
## Author:  Yinghui Wei
## Content: Prepare variables
## Output:  input_stage0.rds; table_0.csv; table_0.html

##Variable naming principles: 
## 1. prefix "cov_cat_" indicate categorical covariates
## 2. prefix "cov_num_" indicate numerical covariates
## 3. prefix "out_" indicate outcome variables
## 4. containing "_date" indicate date variables
## 5. prefix "sub_" indicate patient characteristics which are not included
##    in the analysis / modelling, but are of interest in exploration

library(readr); library(dplyr); library("arrow"); library(lubridate); library(tidyr)

fs::dir_create(here::here("output", "not_for_review", "descriptives"))

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  #cohort <- "all"           # all eligible population
  #cohort <- "vaccinated"    # vaccinated population
  cohort <- "infected"       # infected population
}else{
  cohort <- args[[1]]
}

stage0_data_cleaning <- function(cohort){
  input <- read_feather(paste0("output/input_", cohort, ".feather"))
  
  # remove ra-sle-psoriasis because they have been included individually
  input <- input %>% select(-cov_cat_ra_sle_psoriasis)
  ################################################################################
  ## Part 1. define index date and remove variables, specify date variable       #
  ################################################################################
  ## define cohort start date:
  index_date=as.Date("2020-01-29")
  #RK - in your study defs you've set index date as "2020-01-29"? Does this need to change?
  #YW response - this is now changed to 2020-01-29
  input$index_date = as.Date(index_date)
  cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
  study_days = cohort_end - index_date
  input$cohort_end_date = cohort_end
  
  if(cohort == "vaccinated"){
    input <- input %>% filter(!is.na(vax_covid_date2) &vax_covid_date2 >= index_date & vax_covid_date2 <= cohort_end_date)
    input$index_date = input$vax_covid_date2 + 14 
  }
  if(cohort == "infected"){
    input <- input %>% filter(!is.na(out_covid_date) &out_covid_date>= index_date & out_covid_date <= cohort_end_date)
    input$index_date = input$out_covid_date
  }
  
  # Step 1. Define variables: COVID infection-------------------------------------
  # create an indicator variable for covid infection
  input$out_covid <- ifelse(is.na(input$out_covid_date), FALSE, TRUE)
  
  ## create a categorical variable to indicate covid phenotype: ------------------
  ## no infection, non-hospitalised covid and hospitalised covid
  ## this variable is not included in the Cox model but will create and keep it for now
  input$sub_cat_covid_phenotype <- ifelse(is.na(input$out_covid_date), "no_infection", "non_hospitalised")
  index = which(!is.na(input$hospital_covid))  # index for hospitalised covid
  input$sub_cat_covid_phenotype[index] <- "hospitalised"
  
  #RK - not that it matters if you're not using it but this isn't how hospitalised covid has been defined in our current post covid
  # events opensafely projects
  
  ## Step 2. Remove variables which are not included in the prediction------------
  ## remove variables start with snomed
  snomed_vars <- names(input)[which(grepl("snomed", names(input))==TRUE)]
  input = input[,!(names(input) %in% snomed_vars)]
  
  tmp_vars <- names(input)[which(grepl("tmp", names(input))==TRUE)]
  input = input[,!(names(input) %in% tmp_vars)]
  vars_to_drop <- c("sgss_positive", "primary_care_covid", "hospital_covid",
                    "primary_care_death_date",  "ons_died_from_any_cause_date", 
                    "first_post_viral_fatigue_date")
  input = input[,!(names(input) %in% vars_to_drop)]
  
  ## partial sorting by variable names in the data frame, keep patient_id and practice_id at the front
  input <- input %>% dplyr::select(patient_id, practice_id, index_date, death_date,
                            colnames(input)[grepl("out_",colnames(input))],
                            colnames(input)[grepl("vax_",colnames(input))],
                            sort(tidyselect::peek_vars()))
  
  ## Step 3. specify date variables in the format of "%Y-%m-%d"----------------------------
  vars_dates <- grep("date", names(input))
  vars_dates <- names(input)[vars_dates]
  
  convert_to_date <- function(x){
    as.Date(x,format = "%Y-%m-%d")
  }
  input[vars_dates] = lapply(input[vars_dates], convert_to_date)
  lapply(input[vars_dates], is.Date)
  
  #if you wanted to simplify the above you could use something like
  # for (i in colnames(input)[grepl("date",colnames(input))]) {
  # input[,i] <- as.Date(input[,i], format = "%Y-%m-%d")
  # }
  # Somehow this doesnt work on mine..and so have sticked to the above
  
  
  ################################################################################
  ## Part 2 define multimorbidity                                                #
  ################################################################################
  
  condition_names <- names(input)[grepl("cov_cat", names(input))]
  not_a_condition <- c("cov_cat_age_group", "cov_cat_sex","cov_cat_healthcare_worker",
                       "cov_cat_imd","cov_cat_region","cov_cat_smoking_status",
                       "cov_cat_covid_phenotype", "cov_cat_ethnicity")
  
  condition_names <- condition_names[!condition_names%in%not_a_condition]
  
  input_select <- input %>% dplyr::select(c(patient_id, all_of(condition_names)))
  
  input_select <- input_select %>% mutate(cov_cat_bmi = as.character(cov_cat_bmi)) %>%
    mutate(cov_cat_bmi = ifelse(cov_cat_bmi == "Not obese", FALSE, TRUE)) %>%
    mutate(cov_cat_bmi = as.factor(cov_cat_bmi))
  
  input_select <- input_select %>% mutate_if(is.factor, as.logical.factor)
  
  logical_cols <- input_select %>% dplyr::select(where(is.logical)) %>% names()
  
  input_select <- input_select %>% mutate(cov_num_multimorbidity = rowSums(.[ , logical_cols]))
  
  input_select <- input_select %>% mutate(cov_cat_multimorbidity =ifelse(cov_num_multimorbidity>=2, "2 (two or more diseases)",
                                                                         ifelse(cov_num_multimorbidity==1, "1 (one disease)", "0 (no disease)")))
  
  input_select <- input_select %>% dplyr::select(c(patient_id, cov_cat_multimorbidity))
  
  ## left join: keep all observations in input_select
  input <- merge(x = input_select, y = input, by = "patient_id", all.x = TRUE)
  rm(input_select)
  
  # For numerical variables, produce histogram for numerical variable
  num_var <- colnames(input)[grepl("cov_num_",colnames(input))]
  for(i in num_var){
    print(paste0("Summary statistics for ", i))
    print(summary(input[,i]))
    svglite::svglite(file = paste0("output/not_for_review/descriptives/histogram_", i,"_", cohort, ".svg"))
    if(i!="cov_num_gp_consultation"){
      hist(input[,i], main=paste0("Histogram of ", i), xlab =i)
    }else{
      input_frequent_consultation <- input[which(input[,i]>12),i]
      hist(input_frequent_consultation, main=paste0("Histogram of ", i), xlab =i)
    }
    dev.off()
  }
  ## cov_num_gp_consultation
  ## truncated gp consultation to 365 days
  input$cov_cat_gp_consultation <- ifelse(input$cov_num_gp_consultation > 365, "Greater than 365", "less than or equal to 365")
  input <- input%>%mutate(cov_num_gp_consultation_truncated = 
                            ifelse(cov_num_gp_consultation>365, 365, cov_num_gp_consultation))%>%
                  rename(sub_num_gp_consultation = cov_num_gp_consultation) %>% # rename so it is not included in modelling but only for exploration
                  rename(sub_cat_gp_consultation = cov_cat_gp_consultation) # rename so it is not included in modelling but only for exploration
  #a <- input%>%dplyr::select(contains("gp")); View(a)
  
  ################################################################################
  ## Part 3. define variable types: factor or numerical                          #
  ################################################################################
  
  ## For categorical factors, specify the most frequently occurred level as the reference group
  cat_factors <- colnames(input)[grepl("_cat_",colnames(input))]
  input[,cat_factors] <- lapply(input[,cat_factors], function(x) factor(x, ordered = FALSE))
  
  calculate_mode <- function(x) {
    uniqx <- unique(na.omit(x))
    uniqx[which.max(tabulate(match(x, uniqx)))]
  }
  
  input[,cat_factors] <- lapply(input[,cat_factors], 
                                function(x){
                                  x <- relevel(x, ref = as.character(calculate_mode(x)))
                                  x
                                  }
                                )
  
  ## cov_cat_imd by quintile------------------------------------------------------
  #RK - do you get any missing deprivations in your real data? I'm not sure it's something we've included
  #YW responses - Yes, I do have some deprivations in the real data, and they are included as a catgory.
  # in other projects as it shoudln't be missing I don't think. Is this just a dummy data thing?
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
  
  input$cov_cat_bmi<- ordered(input$cov_cat_bmi, 
                              levels = c("Not obese","Obese I (30-34.9)",
                                          "Obese II (35-39.9)", "Obese III (40+)"))
  
  input$cov_cat_multimorbidity <- ordered(input$cov_cat_multimorbidity, 
                                          levels = c("0 (no disease)", "1 (one disease)", "2 (two or more diseases)"))

  ## cov_cat_smoking_status-------------------------------------------------------
  table(input$cov_cat_smoking_status)
  input <- input %>% mutate(cov_cat_smoking_status = recode(cov_cat_smoking_status, "N" = "Not current smoker",
                                                            "S" = "Current smoker",
                                                            "E" = "Not current smoker",
                                                            "M" = "Missing"))
  #levels(input$cov_cat_smoking_status) <- list("Ever smoker" = "E", "Missing" = "M", "Never smoker" = "N", "Current smoker" = "S")
  #input$cov_cat_smoking_status <- ordered(input$cov_cat_smoking_status, levels = c("Never smoker","Ever smoker","Current smoker","Missing"))
  table(input$cov_cat_smoking_status)
  
  ## cov_cat_sex -----------------------------------------------------------------
  # set Male as the reference level for sex
  input$cov_cat_sex <- relevel(input$cov_cat_sex, ref = "M")

  ## cov_cat_age_group------------------------------------------------------------
  ## Define age groups
  input$cov_cat_age_group <- ""
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=40 & input$cov_num_age<=59, "40_59", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79", input$cov_cat_age_group)
  input$cov_cat_age_group <- ifelse(input$cov_num_age>=80, "80_105", input$cov_cat_age_group)
  input$cov_cat_age_group <- factor(input$cov_cat_age_group, ordered = TRUE)
  
  ## define a variable covid_history to indicate if individuals have covid infection before the start of the cohort
  input$sub_cat_covid_history <-ifelse(input$out_covid_date < input$index_date, TRUE, FALSE)

  #################################################################################
  ## Part 4. For categorical variables, replace "na" with "Missing" as a category #
  #################################################################################
  
  ## store names of factor variables
  cov_factor_names <- names(input)[grepl("_cat", names(input))]
  
  input_factor_vars <- input[,cov_factor_names]
  #
  lapply(input[,cov_factor_names], is.factor)
  lapply(input_factor_vars, is.factor)
  
  ## impose some NA and check if this works
  #input_factor_vars$cov_cat_region[1:10] =NA
  print("Replace missing values with a Missing category!")
  
  input_factor_vars <- input_factor_vars %>% mutate(cov_cat_region = as.character(cov_cat_region)) %>%
    mutate(cov_cat_region = replace_na(cov_cat_region, "Missing")) %>%
    mutate(cov_cat_region = as.factor(cov_cat_region))
  
  # cov_cat_ethnicity: merge "Missing" with "Other"
  input_factor_vars <- input_factor_vars %>% mutate(cov_cat_ethnicity = recode(cov_cat_ethnicity,
                                                        Missing ="Missing or Other",
                                                        Other = "Missing or Other"))
  # input_factor_vars <- input_factor_vars %>%
  #   mutate(cov_cat_ethnicity = case_when(cov_cat_ethnicity == "Missing" ~ "Missing or Other",
  #                                        cov_cat_ethnicity == "Other" ~ "Missing or Other"))
  
  input_factor_vars$cov_cat_ethnicity <- relevel(input_factor_vars$cov_cat_ethnicity, ref = "White")
  
  # cov_cat_smoking_status
  input_factor_vars <- input_factor_vars %>% mutate(cov_cat_smoking_status = as.character(cov_cat_smoking_status)) %>%
    mutate(cov_cat_smoking_status = replace_na(cov_cat_smoking_status, "Missing")) %>%
    mutate(cov_cat_smoking_status = as.factor(cov_cat_smoking_status))
    
  #reset the reference to not current smoker
  #input_factor_vars$cov_cat_smoking_status <- relevel(input_factor_vars$cov_cat_smoking_status, ref = "Never smoker")
  input_factor_vars$cov_cat_smoking_status <- relevel(input_factor_vars$cov_cat_smoking_status, ref = "Not current smoker")
  
  ## sub_cat_covid_history: two categories: false and missing, as patients with covid history was excluded
  input_factor_vars <- input_factor_vars %>% mutate(sub_cat_covid_history = as.character(sub_cat_covid_history)) %>%
    mutate(sub_cat_covid_history = replace_na(sub_cat_covid_history, "FALSE")) %>%
    mutate(sub_cat_covid_history = as.factor(sub_cat_covid_history))
  
  #RK - should sub_cat_covid_history be a TRUE/FALSE variable - I'm not sure that 'Missing' makes sense?
  #YW - agreed and changed "Missing" to "FALSE" for sub_cat_covid_history
  #For the all/vaccinated population you don't remove anyone so anyone without a covid date should be set to FALSE
  #and then for the infected population everyone with covid prior to start date is removed so everyone should be FALSE?
  
  print("Finished replacing missing values with a Missing category!")
  
  input_factor_vars$cov_cat_region <- relevel(input_factor_vars$cov_cat_region, ref = "London")
  
  table(input_factor_vars$cov_cat_region)
  
  print("Finished replacing missing values with a Missing category successfully!")
  
  input[,cov_factor_names] <- input_factor_vars
  
  lapply(input[,cov_factor_names], is.factor)
  
  print("checking after replacing na with missing!") 
  for(i in cov_factor_names){
    print(table(input[,i]))
  }
  
  ################################################################################
  ## Part 5. Output datasets                                                     #
  ################################################################################
  
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
          table_0[index, 4+j-1] <- "[redacted]" #
        }
      }
    }
    names(table_0) <- c("factor variables", "number of missing observations", "is.factor")
    names(table_0)[4:ncol(table_0)] = rep("level name or number of observations", (ncol(table_0)-3))
    #table_0
    
    print("Finished constructing table_0 successfully!")
    print(paste0("cohort is ", cohort))
    
    write.csv(table_0, file=paste0("output/not_for_review/descriptives/table_0_", cohort, ".csv"), row.names =F)
    rmarkdown::render("analysis/compilation/compiled_table0_results.Rmd",
                      output_file=paste0("table_0_", cohort),output_dir="output/not_for_review/descriptives")
  }
  
  output_table_0(input)
  print("table_0 is saved successfully!") 
  
  saveRDS(input, file = paste0("output/input_stage0_", cohort, ".rds"))
  
  print("input_stage0 is saved successfully!") 
}
if(cohort == "all_cohorts") {
  stage0_data_cleaning("all")
  stage0_data_cleaning("vaccinated")
  stage0_data_cleaning("infected")
} else{
  stage0_data_cleaning(cohort)
}
