library(readr)
source("analysis/stage2_model_input_set_up.R")

################################################################################
# Part 1: Fit Cox model with spline for age                                    #
################################################################################

fit_cox_model_splines <-rms::cph(formula= as.formula(surv_formula),
                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

fit_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_lp),
                                data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

if(AIC(fit_cox_model_linear) < AIC(fit_cox_model_splines)){
  surv_formula = surv_formula_lp
  surv_formula_predictors = surv_formula_predictors_lp
  fit_cox_model <- fit_cox_model_linear
} else{
  fit_cox_model <- fit_cox_model_splines
}

print(paste0("Model set up completed for ", analysis, "!"))
print("Part 1 is completed!")

################################################################################
# Part 2: backward elimination                                                 #
################################################################################
## backward elimination: 
fit_cox_model_selected <- fastbw(fit_cox_model, rule= "p", sls=0.20)
# sls: Significance level for staying in a model if rule="p"

print("selected model:")
fit_cox_model_selected$names.kept

selected_covariate_names <- fit_cox_model_selected$names.kept

if(length(fit_cox_model_selected$names.kept)==0){
  selected_covariate_names <- "None"
}
write.csv(selected_covariate_names, 
          file = paste0("output/not_for_review/model/selected_variables_",analysis,".csv"), row.names=FALSE)

if(length(selected_covariate_names)>0){
  if("cov_num_age" %in% selected_covariate_names & grepl("rms::rcs", surv_formula) == TRUE){
    selected_covariate_names <- selected_covariate_names[-grep("age", selected_covariate_names)]
    surv_formula_selected <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+rms::rcs(cov_num_age,parms=knot_placement)"
    )
  }
  if("cov_num_age" %in% selected_covariate_names & grepl("rms::rcs", surv_formula) == FALSE){
    selected_covariate_names <- selected_covariate_names[-grep("age", selected_covariate_names)]
    surv_formula_selected <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+"),
      "+ cov_num_age" 
    )
  }
  if(!("cov_num_age" %in% selected_covariate_names)){
    surv_formula_selected <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(selected_covariate_names, collapse = "+")
      )
  }
  print("Selected models: survival formula is")
  print(surv_formula_selected)
}

if(length(selected_covariate_names)>0){
  which_model = "selected"
}else{
  which_model = "full"
}

selection <- data.frame(analysis = analysis, which_model = which_model)

write.csv(selection, file = paste0("output/not_for_review/model/model_selection_",analysis,".csv"), row.names=FALSE)
print("Finished stage3_model_selection.R")
