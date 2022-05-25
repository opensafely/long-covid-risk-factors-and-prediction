library(readr)
source("analysis/stage3_model_input_set_up.R")

# args <- commandArgs(trailingOnly=TRUE)
# 
# if(length(args)==0){
#   analysis <- "all"          # all eligible population
#   #analysis <- "vax_c"        # all eligible population but censored them by the 2nd vaccination + 14 days
#   #analysis <- "vaccinated"   # vaccinated population
#   #analysis <- "all_vax_td"    # vaccination status is included as a time-dependent covariate
# }else{
#   analysis <- args[[1]]
# }

# fit_cox_model <- read_rds(paste0("output/fit_cox_model_", analysis,".rds"))
# surv_formula  <- read_rds(paste0("output/surv_formula_",analysis, ".rds"))
# print("Fitting cox model:")
# 
# fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
#                          data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
# 
# print("Finished fitting cox model!")

## backward elimination
fit_cox_model_selected <- fastbw(fit_cox_model)

print("selected model:")
fit_cox_model_selected$names.kept

selected_covariate_names <- fit_cox_model_selected$names.kept

if(length(selected_covariate_names)>0){
  if("cov_num_age" %in% selected_covariate_names & grepl("rms::rcs", surv_formula) == TRUE){
    selected_covariate_names <- selected_covariate_names[-grep("age", selected_covariate_names)]
    surv_formula <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+rms::rcs(cov_num_age,parms=knot_placement)", 
      "+ cluster(practice_id)"
    )
  }
  if("cov_num_age" %in% selected_covariate_names & grepl("rms::rcs", surv_formula) == FALSE){
    selected_covariate_names <- selected_covariate_names[-grep("age", selected_covariate_names)]
    surv_formula <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+ cov_num_age", 
      "+ cluster(practice_id)"
    )
  }
  if(!("cov_num_age" %in% selected_covariate_names)){
    surv_formula <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+ cluster(practice_id)")
  }
  print("Selected models: survival formula is")
  print(surv_formula)
}
print("Finished stage3_model_selection.R")

