# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: input set up, define survival formula through variables selection and AIC
# Output:  survival formula for selected model

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  #analysis <- "all"          # all eligible population
  #analysis <- "vax_c"        # all eligible population but censored them by the 2nd vaccination + 14 days
  #analysis <- "vaccinated"    # vaccinated population
  analysis <- "all_vax_td"   # vaccination status is included as a time-dependent covariate
}else{
  analysis <- args[[1]]
}

################################################################################
# Part 1: load data, define inverse probability weighting,                     #
################################################################################

## Analysis 1: all eligible patients, without censoring individuals by vaccination
if(analysis == "all" | analysis == "all_vax_td"){
  input <- read_rds("output/input_stage1_all.rds")
}

## Analysis 2: all eligible patients, but censoring individuals by vaccination
if(analysis == "vax_c"){
  input <- read_rds("output/input_stage1_all.rds")
  input <- input %>% dplyr::select(-lcovid_surv, -lcovid_cens) %>%
    dplyr::rename(lcovid_surv = lcovid_surv_vax_c, lcovid_cens = lcovid_cens_vax_c)
}
## Analysis 3: time origin is the first vaccination 
# (please ignore analysis 3 for now as the study definition needs to be revised for this population)
if(analysis == "vaccinated"){
  input <- read_rds("output/input_stage1_vaccinated.rds")
}

#--specify inverse probability weighting
cases <- input %>% filter(!is.na(out_first_long_covid_date) & 
                                  (out_first_long_covid_date == fup_end_date))

non_cases <- input %>% filter(!patient_id %in% cases$patient_id)

## sample non_cases, size = 5*nrow(cases) if 5*nrow(cases) < nrow(cases)
set.seed(123456) # to ensure reproducibility
if(nrow(cases)*5 < nrow(non_cases)){
  non_cases <- non_cases[sample(1:nrow(non_cases), nrow(cases)*5,replace=FALSE), ]
}else if (nrow(cases)*5 >= nrow(non_cases)){
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

if(analysis == "all_vax_td"){
  z <- ie.setup(input$lcovid_surv, input$lcovid_cens, input$vax1_surv)
  S <- z$S
  ie.status <- z$ie.status
  input <- input[z$subs,] # replicates all variables
  input$cov_cat_ie.status <- as.factor(ie.status)
}

## handling variables

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
  variables_to_keep <- c(variables_to_keep, "vax1_surv")
}

input <- input %>% dplyr::select(all_of(variables_to_keep))
################################################################################
# Part 2: define survival analysis formula                                     #
################################################################################
## linear predictors + a restricted cubic spline for age + clustering effect 
knot_placement=as.numeric(quantile(input$cov_num_age, probs=c(0.1,0.5,0.9)))
## for practice 
surv_formula <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ cluster(practice_id)"
)

## age is added as a linear predictor
surv_formula_lp <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+ cov_num_age", 
  "+ cluster(practice_id)"
)

## only predictors
surv_formula_predictors <- paste0(
  " ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ cluster(practice_id)"
)

## only linear predictors
surv_formula_predictors_lp <- paste0(
  " ~ ",
  paste(covariate_names, collapse = "+"),
  "+ cov_num_age", 
  "+ cluster(practice_id)"
)

if(analysis == "all_vax_td"){
  surv_formula <- paste0(
    "Surv(lcovid_surv, lcovid_cens) ~ ",
    paste(covariate_names, collapse = "+"),
    "+rms::rcs(cov_num_age,parms=knot_placement)", 
#    "+ ie.status",
    "+ cluster(practice_id)"
  )
  ## age is added as a linear predictor
  surv_formula_lp <- paste0(
    "Surv(lcovid_surv, lcovid_cens) ~ ",
    paste(covariate_names, collapse = "+"),
    "+ cov_num_age", 
#    "+ ie.status",
    "+ cluster(practice_id)"
  )
  ## only predictors
  surv_formula_predictors <- paste0(
    " ~ ",
    paste(covariate_names, collapse = "+"),
    "+rms::rcs(cov_num_age,parms=knot_placement)", 
#    "+ ie.status",
    "+ cluster(practice_id)"
  )
  ## only linear predictors
  surv_formula_predictors_lp <- paste0(
    " ~ ",
    paste(covariate_names, collapse = "+"),
    "+ cov_num_age", 
#    "+ ie.status",
    "+ cluster(practice_id)"
  )
}
print(paste0("survival formula: ", surv_formula))

## set up before using rms::cph
dd <<- datadist(input)
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment"))

################################################################################
# Part 3: Assess if non-linear term is needed for continuous age  
#         and redefine the survival analysis formula
################################################################################

fit_cox_model_splines <-rms::cph(formula= as.formula(surv_formula),
                         data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

fit_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_lp),
                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

if(AIC(fit_cox_model_linear) < AIC(fit_cox_model_splines)){
  surv_formula = surv_formula_lp
  surv_formula_predictors = surv_formula_predictors_lp
}

print(paste0("Does the model with lower AIC include splines for age? ",  grepl("rms::rcs", surv_formula)))
print(paste0("The formula for fitting Cox model is: ", surv_formula))
print(paste0("The predictors included in the Cox model are: ", surv_formula_predictors))

