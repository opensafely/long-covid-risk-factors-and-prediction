library(readr)
source("analysis/stage3_model_input_set_up.R")

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

if(length(selected_covariate_names)>0){
  which_model = "selected"
}else{
  which_model = "full"
}

selection <- data.frame(analysis = analysis, which_model = which_model)

write.csv(selection, file = paste0("output/not_for_review/model/model_selection_",analysis,".csv"), row.names=FALSE)
print("Finished stage3_model_selection.R")
