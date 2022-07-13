# Purpose: to fit Cox model: age and sex adjusted and add one other predictor at a time

library(readr)
source("analysis/stage3_model_input_set_up.R")
source("analysis/functions/function_cox_output.R")
source("analysis/functions/function_model_evaluation.R")

combined_results_hr <- data.frame()
combined_results_pm <- data.frame()
################################################################################
# Part 1: Extract names for factors and numerical predictors                   #
################################################################################
cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
cov_num_names <- names(input)[grepl("cov_num", names(input))]

cov_factor_names <- cov_factor_names[!cov_factor_names %in% c("cov_cat_sex","cov_cat_ie.status")] # remove sex and vaccination status
cov_num_names <- cov_num_names[! cov_num_names %in% "cov_num_age"] # remove age

cov_names <- c(cov_factor_names, cov_num_names)

################################################################################
# Part 2: Assess if non-linear term is needed for continuous age               #
#         and redefine the survival analysis formula                           #
################################################################################
for(i in 1:length(cov_names)){
  surv_formula_age_spl_sex_adjusted <- paste0(surv_formula_age_spl_sex,"+", cov_names[i])
  surv_formula_age_linear_sex_adjusted <- paste0(surv_formula_age_linear_sex,"+", cov_names[i])
  # age as splines
  fit_cox_model_splines <-rms::cph(formula= as.formula(surv_formula_age_spl_sex_adjusted),
                                   data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  
  # age as linear predictor
  fit_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_age_linear_sex_adjusted),
                                  data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  
  # a crude comparison
  if(AIC(fit_cox_model_linear) <= AIC(fit_cox_model_splines)){
    surv_formula = surv_formula_age_linear_sex
    #surv_formula_predictors = surv_formula_predictors_lp
    fit_cox_model <- fit_cox_model_linear
    print("Linear term is selected for age!")
  } else{
    surv_formula <- surv_formula_age_spl_sex
    fit_cox_model <- fit_cox_model_splines
    print("Restricted cubic splines is selected for age!")
  }
  print("The selected model is")
  print(fit_cox_model)
  ################################################################################
  # Part 3: Output results from the Cox Model                                    #
  ################################################################################
  which_model = cov_names[i]
  output_file = paste0("output/review/model/hazard_ratio_estimates_age_sex_adjusted_", which_model, "_", analysis)
  results <- cox_output2(fit_cox_model, which_model, output_file, save_output=FALSE)
  results$model <- "age_sex_adjusted"
  results$predictor <- cov_factor_names[i]
  combined_results_hr <- rbind(combined_results_hr, results)
  
  ################################################################################
  # Part 4: Model evaluation                                                     #
  ################################################################################
  subset_vars = "age_sex_adjusted_"
  which_model = cov_names[i]
  results_pm <- function_model_evaluation(input,fit_cox_model, which_model, analysis, subset_vars, graphics_output=FALSE, save_output=FALSE)
  results_pm$predictors = cov_names[i]
  print("Finished model valuation for age and sex adjusted model!")
  combined_results_pm <- rbind(combined_results_pm, results_pm)
}

# Output hazard ratio estimates
which_model = "age_sex_adjusted"
output_file = paste0("output/review/model/HR_estimates_", which_model, "_", analysis)
write.csv(combined_results_hr, file=paste0(output_file,".csv"), row.names=F)
rmarkdown::render(paste0("analysis/compilation/compiled_HR_results",".Rmd"), 
                  output_file=output_file,
                  output_dir="output/review/model")

# output performance measures
subset_vars = ""; which_model = "age_sex_adjusted"
write.csv(combined_results_pm, 
          file=paste0("output/review/model/performance_measures_", subset_vars,which_model, "_", analysis, ".csv"), 
          row.names=F)

rmarkdown::render(paste0("analysis/compilation/compiled_performance_measure_table",".Rmd"), 
                  output_file=paste0("performance_measures_", subset_vars, which_model,"_", analysis),
                  output_dir="output/review/model")