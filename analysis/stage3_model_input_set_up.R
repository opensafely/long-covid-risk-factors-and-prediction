# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: input set up, define survival formula through variables selection and AIC
# Output:  survival formula for selected model

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available
fs::dir_create(here::here("output", "not_for_review", "model"))
fs::dir_create(here::here("output", "review", "model"))

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  analysis <- "all"          # all eligible population
  #analysis <- "all_vax_c"        # all eligible population but censored them by the 1st vaccination
  #analysis <- "vaccinated"   # vaccinated population
  #analysis <- "all_vax_td"    # vaccination status is included as a time-dependent covariate
  #analysis <- "infected"
}else{
  analysis <- args[[1]]
}

ratio_non_cases_to_cases = 10 # this is used in sampling non-cases to increase efficiency without loss of information
set.seed(123456) # to ensure reproducibility in the sampling

################################################################################
# Part 1: load data, define inverse probability weighting                      #
################################################################################
## Analyses 1 and 4: all eligible patients, without censoring individuals by vaccination
if(analysis == "all"){
  input <- read_rds("output/input_stage1_all.rds")
}

if(analysis == "all_vax_td"){
  input <- read_rds("output/input_stage1_all.rds")
}

## Analysis 2: all eligible patients, but censoring individuals by vaccination
if(analysis == "all_vax_c"){
  input <- read_rds("output/input_stage1_all.rds")
  input <- input %>% dplyr::select(-lcovid_surv, -lcovid_cens) %>%
    dplyr::rename(lcovid_surv = lcovid_surv_vax_c, lcovid_cens = lcovid_cens_vax_c)
}
## Analysis 3: time origin is the first vaccination 
if(analysis == "vaccinated"){
  input <- read_rds("output/input_stage1_vaccinated.rds")
}

## Analysis 6: time origin is the first covid infection
if(analysis == "infected"){
  input <- read_rds("output/input_stage1_infected.rds")
}
##--specify inverse probability weighting
cases <- input %>% filter(!is.na(out_first_long_covid_date) & 
                                  (out_first_long_covid_date == fup_end_date))

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

if(analysis == "all_vax_td"){
  z <- ie.setup(input$lcovid_surv, input$lcovid_cens, input$vax2_surv)
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
  variables_to_keep <- c(variables_to_keep, "vax2_surv")
}

input <- input %>% dplyr::select(all_of(variables_to_keep))
# readr::write_rds(input, paste0("output/input_samples_", analysis, ".rds")) 

print("Part 1: load data, define inverse probability weighting is completed!")

################################################################################
# Part 2: number of people in each covariate level                             #
################################################################################

df_summary <- data.frame(variable = character(),
                      number  = numeric(),
                      percent = numeric(),
                      mean    = numeric(),
                      sd      = numeric(), 
                      IQR   = numeric(),
                      stringsAsFactors = FALSE)

cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
cov_num_names <- names(input)[grepl("cov_num", names(input))]
# factor variables: number and percentage---------------------------------------
input_factor_vars <- input[, cov_factor_names]
for(i in 1:length(cov_factor_names)){
  levels = names(table(input_factor_vars[,i]))
  start = nrow(df_summary)+1
  df_summary[start,1] = cov_factor_names[i]
  start = nrow(df_summary)+1
  end = nrow(df_summary)+length(levels)
  df_summary[start:end,1] <- c(levels)            # variable name
  df_summary[start:end,2] <- c(table(input_factor_vars[,i]))  # number
  df_summary[start:end,3] <- 100*round(c(table(input_factor_vars[,i]))/nrow(input_factor_vars),4)  # percentage
  print(levels)
}

# numerical variables: number and percentage of observations, mean and standard deviations
input_num_vars <- input[,cov_num_names]
if(length(cov_num_names) == 1){
  index = nrow(df_summary)+1
  df_summary[index,1] <- cov_num_names
  df_summary[index,2] <- length(which(!is.na(unlist(input_num_vars)))) # number of observations
  df_summary[index,4] <- round(mean(unlist(input_num_vars)),2) # mean
  df_summary[index,5] <- round(sd(unlist(input_num_vars)),2) # sd
  df_summary[index,6] <- round(IQR(unlist(input_num_vars)),2)  # IQR
}
if(length(cov_num_names)>1){
  for(i in 1:length(cov_num_names)){
    index = nrow(df_summary)+1
    df_summary[index,1] <- cov_num_names[i]
    df_summary[index,2] <- length(which(!is.na(unlist(input_num_vars[,i])))) # number of observations
    df_summary[index,4] <- round(mean(unlist(input_num_vars[,i])),2) # mean
    df_summary[index,5] <- round(sd(unlist(input_num_vars[,i])),2) # sd
    df_summary[index,6] <- round(IQR(unlist(input_num_vars[,i])),2)  # IQR
  }
}

# Output as help file: number of observations from the sampled population in each covariate level
write.csv(df_summary, file=paste0("output/review/model/analysis_data_summary_", analysis,".csv"), row.names = F)
rmarkdown::render("analysis/compilation/compiled_analysis_data_summary.Rmd",
                  output_file=paste0("analysis_data_summary_", analysis),output_dir="output/review/model")

## set up before using rms::cph
dd <<- datadist(input) #
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment")) #

################################################################################
# Part 3: define survival analysis formula                                     #
################################################################################
## linear predictors + a restricted cubic spline for age + 
##  a restricted cubic spline for gp consultation rate + clustering effect for practice
knot_placement=as.numeric(quantile(input$cov_num_age, probs=c(0.1,0.5,0.9)))

## Age sex model
surv_formula_age_spl_sex <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  "cov_cat_sex",
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ strat(sub_cat_region)"
)

## Age sex model
surv_formula_age_linear_sex <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  "cov_cat_sex", "+ cov_num_age", "+ strat(sub_cat_region)"
)

## Age sex model
if(analysis == "all_vax_td"){
  surv_formula_age_spl_sex <- paste0(
    "Surv(lcovid_surv, lcovid_cens) ~ ",
    "cov_cat_sex", "+rms::rcs(cov_num_age,parms=knot_placement)", 
    "+ cov_cat_ie.status", "+ strat(sub_cat_region)"
  )
  surv_formula_age_linear_sex <- paste0(
    "Surv(lcovid_surv, lcovid_cens) ~ ",
    "cov_cat_sex", "+ cov_num_age", "+ cov_cat_ie.status", "+ strat(sub_cat_region)"
  )
}

## Full model
surv_formula <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ strat(sub_cat_region)"
)

## full model: age is added as a linear predictor
surv_formula_lp <- paste0(
  "Surv(lcovid_surv, lcovid_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+ cov_num_age", 
  "+ strat(sub_cat_region)"
)

# the following lines are not needed because ie.status has been specified as a covariate if analysis == "all_vax_td"
# if(analysis == "all_vax_td"){
#   surv_formula <- paste0(
#     "Surv(lcovid_surv, lcovid_cens) ~ ",
#     paste(covariate_names, collapse = "+"),
#     "+rms::rcs(cov_num_age,parms=knot_placement)", 
#     "+ strat(sub_cat_region)"
#   )
#   ## age is added as a linear predictor
#   surv_formula_lp <- paste0(
#     "Surv(lcovid_surv, lcovid_cens) ~ ",
#     paste(covariate_names, collapse = "+"),
#     "+ cov_num_age", 
#     "+ strat(sub_cat_region)"
#   )
# }
## only predictors
surv_formula_predictors <- stringr::str_extract(surv_formula, " ~.+")
## only linear predictors
surv_formula_predictors_lp <- stringr::str_extract(surv_formula_lp, " ~.+")

print(paste0("survival formula: ", surv_formula))

print("Part 3: define survival analysis formula is completed!")

print("End of stage3_model_input_setup.R")

