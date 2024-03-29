## Purpose: Long COVID risk factors and prediction models
## Author:  Yinghui Wei
## Content: define eligible population
## Output:  input_stage1_all.rds; input_stage1_vaccinated.rds; input_stage1_infected.rds

library(readr); library(dplyr); library(htmlTable)
source("analysis/functions/redactor2.R")
fs::dir_create(here::here("output", "review", "descriptives"))
args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  #cohort <- "all"           # all eligible population
  #cohort <- "vaccinated"    # vaccinated population
  cohort <- "infected"     # infected population
}else{
  cohort <- args[[1]]
}

cohort_start = as.Date("2020-01-29", format="%Y-%m-%d") # this is the same as the index date column - do you need both?
cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
cohort_days = cohort_end - cohort_start + 1

stage1_eligibility <- function(cohort){
  input <- read_rds(paste0("output/input_stage0_", cohort, ".rds"))
  ## Specify study period
  cohort_start = as.Date("2020-01-29", format="%Y-%m-%d") # this is the same as the index date column - do you need both?
  ## YW: in some cohort the cohort start date is different from the index date
  ## for the vaccinated population and infected population, their index dates were 2nd vaccination + 14 days, and first covid dates during the follow-up
  cohort_end = as.Date("2022-03-31", format="%Y-%m-%d")
  input$cohort_end_date = cohort_end
  ##RK - I would move any hard coded dates to to the top of the script so you don't have to be searching through
  ##scripts to find/check what dates you've been using
  ##YW: this has now been moved to the top of the script
  ################################################################################
  ## Part 1. Define eligible population                                          #
  ################################################################################
  
  steps <- c("starting point","dead before index date", "missing region", "missing sex", 
             "missing age", "age <18y", "age>105y",  
             "covid before index date", "long covid before index date")
  
  ## starting point
  flow_chart_n <- nrow(input)
  
  ## Dead: removed if dead before index date
  input <- input%>%filter(death_date > index_date | is.na(death_date))
  flow_chart_n <- c(flow_chart_n, nrow(input))

  ## Region: remove if missing
  ##input <- input%>%filter(!is.na(cov_cat_region))
  input <- input%>%filter(cov_cat_region!="Missing") # keep if not missing
  table(input$cov_cat_region)
  flow_chart_n <- c(flow_chart_n, nrow(input))
  
  ##RK - in stage 0 you set NA's to 'Missing' so do you want to be removing 'Missing' instead?
  ##YW - this is now changed to remove the "Missing" category for region and sex
  ## Sex: remove if missing
  input <- input%>%filter(cov_cat_sex!="Missing")
  table(input$cov_cat_sex)
  flow_chart_n <- c(flow_chart_n, nrow(input))
  
  ## Age: remove if missing
  input <- input%>%filter(!is.na(cov_num_age))
  flow_chart_n <- c(flow_chart_n, nrow(input))
  
  ## Adult: remove if age < 18
  input <- input%>%filter(cov_num_age>=18)
  flow_chart_n <- c(flow_chart_n, nrow(input))
  
  ## Adult: remove if age > 105 years
  input <- input%>%filter(cov_num_age <=105)
  flow_chart_n <- c(flow_chart_n, nrow(input))
   
  ## make age at 10 years increment 
  # input <- input %>% 
  #   rename(sub_num_age = cov_num_age) %>% # rename to keep in exploration but not to include in the model
  #   mutate(cov_num_age = sub_num_age/10) 
  #YW 26/08/2022 no need to be at 10 year increment because we will present HR for categorical age
  #   and log HR against continuous age
  
  ### Ethnicity: remove if missing
  ## input <- input%>%filter(!is.na(cov_cat_ethnicity))
  ## flow_chart_n <- c(flow_chart_n, nrow(input))
  ##RK - same as for region, you set NA values to "Missing or Other" so what are you trying to remove?
  ##Do you want to remove people with missing ethnicity or just checking for NA values because there shouldn't be any
  ##and if there are any NA values then something has gone wrong in a previous step?
  ##YW response - You are right! Missing ethnicity is incorporated as a category, and it is also
  ## no supposed to be removed. I have commented these out
  
  ## COVID history
  input <- input%>%filter(!(sub_cat_covid_history==TRUE))
  input<- input%>% mutate(sub_cat_covid_history = as.character(sub_cat_covid_history)) %>%
    mutate(sub_cat_covid_history = as.factor(sub_cat_covid_history))
  flow_chart_n <- c(flow_chart_n, nrow(input))
  table(input$sub_cat_covid_history)
  ##RK - copied from stage 0 comment: should sub_cat_covid_history be a TRUE/FALSE variable - I'm not sure that 'Missing' makes sense?
  ##This is a date variable so if they don't have a date, its not that they're missing information (as you don't expect
  ##the whole population to have a covid date), they just never had covid so should they actually be FALSE?
  ##YW 2022/08/13: sub_cat_covid_history has been defined as a TRUE/FALSE variable in stage0 script
  
  ###table(input$cov_cat_previous_covid)
  ## input <- input%>%filter(sub_cat_previous_covid == "No COVID code")
  ## flow_chart_n <- c(flow_chart_n, nrow(input))
  
  ### for individuals whose first long covid date is after cohort end date, set first long covid date as na
  ## index <- which(input$out_first_long_covid_date > input$cohort_end_date)
  ## input$out_first_long_covid_date[index] <- NA
 
  ## sum(!is.na(input$out_first_long_covid_date))
  
  ##RK - fup_end_date is defined later in this script and hasn't been defined yet so index just returns
  ##empty
  ##YW response 2022/08/13: You are right, though now I think setting out_first_long_covid_date outside
  ## cohort period is not needed, as the fup_end_date is defined later to compute the survival time.
  ## So I have commented these out.
  
  ## previous long covid
  ##RK- in your study defs your index date is "2020-01-29"? Does this need to be changed? If you want to use any date variables
  ##it might be better to have them at the top of the script to make it easier to check which dates are being used?
  ##Should the below criteria be for individual index date rather than cohort start date?
  ##YW - the cohort start date is now changed to "2020-01-29", 
  ##also the specification of cohort start/end dates have now been moved to the top of the script
  
  input <- input%>%filter(out_first_long_covid_date >= index_date |
                            is.na(out_first_long_covid_date))
  flow_chart_n <- c(flow_chart_n, nrow(input))
  
  ## redefine factor level for region since NA is now removed
  input <- input %>% mutate(cov_cat_region = as.character(cov_cat_region)) %>%
    mutate(cov_cat_region = as.factor(cov_cat_region))
  input$cov_cat_region <- relevel(input$cov_cat_region, ref = "East")
  ##RK - need to check whether you want to be removing those who have region set to 'Missing'
  ##as by this script no one should have any NA values in region
  ##YW: 2022/08/13, you are right, na has been set to "Missing", and this is updated in above to remove missing region
  
  ## redefine age group
  input <- input %>% mutate(cov_cat_age_group = ifelse(input$cov_num_age>=18 & input$cov_num_age<=39, "18_39",
                                                       ifelse(input$cov_num_age>=40 & input$cov_num_age<=59,"40_59",
                                                              ifelse(input$cov_num_age>=60 & input$cov_num_age<=79, "60_79",
                                                                     "80_105"))))
  input$cov_cat_age_group <- factor(input$cov_cat_age_group, ordered = TRUE)
  
  ##RK - this was defined in stage 0 - why are you redefining it here?
  ##YW - A good question, this is because the dummy data have some observations under 18, and by removing them,
  ##     the number of levels in cov_cat_age_group has changed
  
  #################################################################################
  ##Part 2. Define follow-up end date and construct survival data for long covid ##
  #################################################################################
  
  ## To improve efficiency: keep only necessary variables
  variables_to_keep <-c("patient_id", "out_first_long_covid_date", "vax_covid_date1",
                        "vax_covid_date2", "death_date", "cohort_end_date", "index_date",
                        "out_covid_date",
                        "out_first_post_viral_fatigue_date_after_pandemic")
  
  input_select <- input%>% dplyr::select(all_of(variables_to_keep))
  
  ####################################################################################
  ## Cohort 1 - all eligible adults; Cohort 3: post-vaccination; Cohort 4: post-covid
  ## Construct time to long COVID 
  ## lcovid_cens: indicator for long covid  
  ## lcovid_surv: days from index date to long COVID, without censoring by vaccination 
  ## for the following cohorts
  ####################################################################################
  input_select <- input_select%>% rowwise() %>% mutate(fup_end_date=min(out_first_long_covid_date, 
                                                                        death_date, 
                                                                        cohort_end_date,
                                                                        na.rm = TRUE))
  input_select <- input_select %>% filter(fup_end_date >= index_date & fup_end_date != Inf)
  input_select$lcovid_surv<- as.numeric(input_select$fup_end_date - input_select$index_date)+1

  ## define event indicator, without censoring for vaccination
  input_select <- input_select %>% mutate(lcovid_cens = ifelse((out_first_long_covid_date <= fup_end_date & 
                                                                  out_first_long_covid_date >= index_date &
                                                                  !is.na(out_first_long_covid_date)), 1, 0)) %>%
    ## define 2nd vaccination as an intervening event - a time-dependent variable
    ## add 14 days as buffer to allow vaccination to take effects
    mutate(vax2_surv = ifelse((vax_covid_date2 +14)>= index_date & 
                                (vax_covid_date2 + 14) <= fup_end_date &
                                !is.na(vax_covid_date2) & 
                                ((vax_covid_date2+14) <= out_first_long_covid_date | is.na(out_first_long_covid_date)),
                              as.numeric(vax_covid_date2 - index_date)+14, NA))
  #RK - should line 139 vax_covid_date2 >= index_date instead be (vax_covid_date2 + 14) >= index_date otherwise you
  #might not be inlcuding people who were vaccinated prior to index but by index were classed as vaccinated as the 14 days had
  #passed by then
  #YW - agree, and have changed to (vax_covid_date2 +14) >= index_date
  ## Define survival data for eligible -----------------------------------------------------
  if(cohort == "all"){
    ################################################################################################
    ## Cohort 2: pre-vaccination cohort - all eligible adults but censored by the first vaccination 
    ## lcovid_surv_vax_c: days from index date to long COVID, censored by the first vaccination 
    ## lcovid_cens_vax_c: indicator for long covid  
    ###############################################################################################
    input_select <- input_select %>% rowwise() %>% mutate(fup_end_date_vax_c=min(out_first_long_covid_date, 
                                                                                 vax_covid_date1,
                                                                                 death_date, 
                                                                                 cohort_end_date,
                                                                                 na.rm = TRUE))
    input_select$lcovid_surv_vax_c<- as.numeric(input_select$fup_end_date_vax_c - input_select$index_date)+1
    input_select <- input_select %>% mutate(lcovid_cens_vax_c = ifelse((out_first_long_covid_date <= fup_end_date_vax_c & 
                                                                          out_first_long_covid_date >= index_date &
                                                                          !is.na(out_first_long_covid_date)), 1, 0))
  }
  #############################################################################################
  ## Define time to post-viral fatigue defined between 29 January 2020 and 29 November 2020  ##
  #############################################################################################
  if(cohort == "all"){
    input_select <- input_select %>% mutate(fup_end_date_fatigue=min(out_first_post_viral_fatigue_date_after_pandemic,
                                                                     death_date, 
                                                                     cohort_end_date,
                                                                     na.rm = TRUE))
    ## time to post-viral fatigue diagnosis date
    input_select$fatigue_surv<- as.numeric(input_select$fup_end_date_fatigue - input_select$index_date)+1
    input_select <- input_select %>% mutate(fatigue_cens = ifelse((out_first_post_viral_fatigue_date_after_pandemic <= fup_end_date_fatigue &
                                                                     out_first_post_viral_fatigue_date_after_pandemic >= out_covid_date &
                                                                     out_first_post_viral_fatigue_date_after_pandemic >= index_date &
                                                                     !is.na(out_first_post_viral_fatigue_date_after_pandemic) &
                                                                     !is.na(out_covid_date)), 1, 0))
  }
  ################################################################################
  ## Part 3. Create and output datasets                                         ##
  ################################################################################
  ## Create data set for analysis 1 and analysis 2
  
  if(cohort == "all"){
    variables_to_keep <-c("patient_id", "fup_end_date","index_date",
                          "lcovid_surv", "lcovid_cens","lcovid_surv_vax_c", "lcovid_cens_vax_c",
                          "fup_end_date_vax_c", "vax2_surv", "fatigue_surv","fatigue_cens")
  }else{
    variables_to_keep <-c("patient_id", "fup_end_date","index_date",
                          "lcovid_surv", "lcovid_cens", "vax2_surv")
  }
  
  ## warnings if non-neg follow-up time
  if (!all(input_select$lcovid_surv>=0)) warning("lcovid_surv should be  >= 0 in input_select")
  if (!all(input_select$lcovid_cens>=0)) warning("lcovid_surv should be  >= 0 in input_select")
  
  ## warnings if follow-up days greater than maximum cohort days
  if (!all(input_select$lcovid_surv <= cohort_days)) warning("lcovid_surv should be  <= cohort_days in input_select")
  if (!all(input_select$lcovid_cens <= cohort_days)) warning("lcovid_surv should be  <= cohort_days in input_select")
  
  input_select <- input_select[,variables_to_keep]
  
  input <- input%>%dplyr::select(-index_date)
  ## left join: keep all observations in input_select
  input <- merge(x = input_select, y = input, by = "patient_id", all.x = TRUE)
  
  rm(input_select)
  
  if(cohort == "infected"){
    input <- input %>% mutate(cov_cat_covid_phenotype = sub_cat_covid_phenotype)
  }
  
  saveRDS(input, file = paste0("output/input_stage1_", cohort, ".rds"))
  print(paste0("Stage 1 date set for ", cohort, " created successfully!"))
  ################################################################################
  ## Part 4. Flowchart output                                                   ##
  ################################################################################
  
  flow_chart<-data.frame(steps, flow_chart_n)
  flow_chart$drop <- rep(0, nrow(flow_chart))
  for(i in 2:nrow(flow_chart)){
    flow_chart$drop[i] = flow_chart$flow_chart_n[i-1] - flow_chart$flow_chart_n[i]
  }
  
  ## function for small number suppression
  flow_chart$drop[2:nrow(flow_chart)] <- redactor2(flow_chart$drop[2:nrow(flow_chart)])
  flow_chart[is.na(flow_chart$drop),2:3] = "[redacted]"

  write.csv(flow_chart, file=paste0("output/not_for_review/descriptives/flow_chart_", cohort, ".csv"), row.names = F)
  print(paste0("Flowchart table is saved successfully for ", cohort, "population!"))
  
  ## output GP-patient interaction quantiles
  df_gp <- quantile(input$sub_num_gp_patient_interaction, probs = seq(0, 1, 0.125), na.rm = TRUE)
  df_gp <- data.frame(df_gp, seq(0,1,0.125))
  names(df_gp) <- c("sub_num_gp_patient_interaction", "Quantile")
  rownames(df_gp) <- NULL
  write.csv(df_gp, file= paste0("output/not_for_review/descriptives/table_quantile_gp_patient_", cohort, ".csv"), row.names=F)
}

if (cohort == "all_cohorts") {
  stage1_eligibility("all")
  stage1_eligibility("vaccinated")
  stage1_eligibility("infected")
} else{
  stage1_eligibility(cohort)
}
