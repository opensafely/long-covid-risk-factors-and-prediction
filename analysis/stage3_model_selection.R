source("analysis/stage3_model_input_set_up.R")

print("Fitting cox model:")

fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                         data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

print("Finished fitting cox model!")

## backward elimination
fit_cox_model_vs <- fastbw(fit_cox_model)

print("selected model:")
fit_cox_model_vs$names.kept

selected_covariate_names <- fit_cox_model_vs$names.kept

if(length(selected_covariate_names)>0){
  if("cov_num_age" %in% selected_covariate_names & grepl("rms::rcs", surv_formula) == TRUE){
    selected_covariate_names <- selected_covariate_name[-grep("age", selected_covariate_names)]
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
  print(surv_formula)
  # fit_cox_model_selected <-rms::cph(formula= as.formula(surv_formula),
  #                                   data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
}
