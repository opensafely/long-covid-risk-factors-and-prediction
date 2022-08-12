# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: survival data set up
# Output:  survival data for long COVID code

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available
fs::dir_create(here::here("output", "not_for_review", "model"))
fs::dir_create(here::here("output", "review", "model"))
outcome = "long_covid"
################################################################################
#  Specify analysis cohort                                                     #
################################################################################
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  #analysis <- "all"          # all eligible population
  #analysis <- "all_vax_c"        # all eligible population but censored them by the 1st vaccination
  #analysis <- "vaccinated"   # vaccinated population
  #analysis <- "all_vax_td"    # vaccination status is included as a time-dependent covariate
  analysis <- "infected"
}else{
  analysis <- args[[1]]
}

ratio_non_cases_to_cases = 10 # this is used in sampling non-cases to increase efficiency without loss of information
set.seed(123456) # to ensure reproducibility in the sampling

################################################################################
#  load data, define inverse probability weighting                             #
################################################################################
## Cohort 1: all eligible patients, without censoring individuals by vaccination
if(analysis == "all"){
  input <- read_rds("output/input_stage1_all.rds")
}

if(analysis == "all_vax_td"){
  input <- read_rds("output/input_stage1_all.rds")
}

## Cohort 2: all eligible patients, but censoring individuals by vaccination
if(analysis == "all_vax_c"){
  input <- read_rds("output/input_stage1_all.rds")
  input <- input %>% dplyr::select(-lcovid_surv, -lcovid_cens) %>%
    dplyr::rename(lcovid_surv = lcovid_surv_vax_c, lcovid_cens = lcovid_cens_vax_c)
}
## Cohort 3: time origin is the first vaccination 
if(analysis == "vaccinated"){
  input <- read_rds("output/input_stage1_vaccinated.rds")
}

## Cohort 4: time origin is the first covid infection
if(analysis == "infected"){
  input <- read_rds("output/input_stage1_infected.rds")
}

##--specify inverse probability weighting
cases <- input %>% filter(lcovid_cens==1)
non_cases <- input %>% filter(!patient_id %in% cases$patient_id)

if(nrow(cases)*ratio_non_cases_to_cases < nrow(non_cases)){
  non_cases <- non_cases[sample(1:nrow(non_cases), nrow(cases)*ratio_non_cases_to_cases,replace=FALSE), ]
}else if (nrow(cases)*ratio_non_cases_to_cases >= nrow(non_cases)){
  non_cases=non_cases
}
print(paste0("Number of cases: ", nrow(cases)))
print(paste0("Number of controls: ", nrow(non_cases)))
non_case_inverse_weight=(nrow(input)-nrow(cases))/nrow(non_cases)
## recreate input after sampling
input <- bind_rows(cases,non_cases)
## Add inverse probability weights for non-cases
noncase_ids <- unique(non_cases$patient_id)
input$weight <-1
input$weight <- ifelse(input$patient_id %in% noncase_ids,
                       non_case_inverse_weight, 1)

# create the time dependent variable for vaccination
if(analysis == "all_vax_td"){
  z <- ie.setup(input$lcovid_surv, input$lcovid_cens, input$vax2_surv)
  S <- z$S
  ie.status <- z$ie.status
  input <- input[z$subs,] # replicates all variables
  input$cov_cat_ie.status <- as.factor(ie.status)
}

## remove region as a covariate
input <- rename(input, sub_cat_region = "cov_cat_region")

## extract candidate predictors
covariate_names <- names(input)[grep("cov_", names(input))]

## remove categorical and continuous age, as continuous age will be added
## later as with a spline function
covariate_names <- covariate_names[-grep("age", covariate_names)]

print("candidate predictors")
print(covariate_names)
## for computational efficiency, only keep the variables needed in fitting the model
variables_to_keep <- c("patient_id", "practice_id",
                       "lcovid_surv", "lcovid_cens", covariate_names,
                       "cov_num_age", "weight", "sub_cat_region")

if(analysis == "all_vax_td"){
  variables_to_keep <- c(variables_to_keep, "vax2_surv")
}

input <- input %>% dplyr::select(all_of(variables_to_keep))
print("load data, define inverse probability weighting is completed!")

saveRDS(input, file = paste0("output/input_stage2_", analysis, ".rds"))
