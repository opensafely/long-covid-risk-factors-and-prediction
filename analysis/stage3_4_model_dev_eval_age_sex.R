## Programmed by Yinghui Wei
## Purpose: to fit Cox model with age and sex as predictors

library(readr)
source("analysis/stage2_model_input_set_up.R")
source("analysis/functions/function_cox_output.R")
source("analysis/functions/function_model_evaluation.R")

fs::dir_create(here::here("output", "review", "model"))
fs::dir_create(here::here("output", "not_for_review", "model"))
######
# Part 1: Assess if non-linear term is needed for continuous age               #
#         and redefine the survival analysis formula                           #
################################################################################

# Age Sex model - age as splines
fit_cox_model <- fit_cox_model_splines <-rms::cph(formula= as.formula(surv_formula_age_spl_sex),
                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
saveRDS(fit_cox_model_splines, file=paste0("output/not_for_review/model/fit_cox_model_age_sex_model_splines_", analysis,".rds"))
# 
# # age as linear predictor
# fit_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_age_linear_sex),
#                                 data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
# 
# # a crude comparison
# if(AIC(fit_cox_model_linear) <= AIC(fit_cox_model_splines)){
#   surv_formula = surv_formula_age_linear_sex
#   #surv_formula_predictors = surv_formula_predictors_lp
#   fit_cox_model <- fit_cox_model_linear
#   print("Linear term is selected for age!")
# } else{
#   surv_formula <- surv_formula_age_spl_sex
#   fit_cox_model <- fit_cox_model_splines
#   print("Restricted cubic splines is selected for age!")
# }
# print("The selected model is")
# print(fit_cox_model)

# # save model as rds file
# readr::write_rds(
#   fit_cox_model,
#   paste0("output/not_for_review/model/fit_cox_model_age_sex_", analysis, ".rds")
#   #compress = "gz"
# )

print("Part 1. Finished fitting age and sex models: age splines and age linear!")

################################################################################
# Part 2: Fit models with categorical age                                       #
################################################################################
# Age sex model - age as categorical
fit_cox_model_categorical <-rms::cph(formula= as.formula(surv_formula_age_cat_sex),
                                data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

saveRDS(fit_cox_model_categorical, file=paste0("output/not_for_review/model/fit_cox_model_age_sex_model_categorical_", analysis,".rds"))

# Full model - age as categorical

fit_cox_model_full_categorical <-rms::cph(formula= as.formula(surv_formula_age_categorical),
                                     data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)


saveRDS(fit_cox_model_full_categorical, file=paste0("output/not_for_review/model/fit_cox_model_full_model_categorical_", analysis,".rds"))

print("Part 2. Finished fitting Cox models with categorical age!")

################################################################################
# Part 3: Output results from the Cox Model                                    #
################################################################################
which_model = "model"
output_file = paste0("output/review/model/HR_age_sex_", which_model, "_", analysis)
cox_output2(fit_cox_model, which_model, output_file, save_output = TRUE)

which_model = "model_categorical"
output_file = paste0("output/review/model/HR_age_sex_", which_model, "_", analysis)
cox_output2(fit_cox_model_categorical, which_model, output_file, save_output = TRUE)

which_model = "full_model_categorical" # this is a fully adjusted model, but put it in this script for convenience
output_file = paste0("output/review/model/HR_", which_model, "_", analysis) 
cox_output2(fit_cox_model_full_categorical, which_model, output_file, save_output = TRUE)

print("Part 3. Finished output results from Cox models!")
################################################################################
# Part 4: Model evaluation                                                     #
################################################################################
#model_file <- paste0("output/not_for_review/model/fit_cox_model_age_sex_", analysis, ".rds")
#fit_cox_model <- readr::read_rds(model_file)
#print("Part 1. Finished loading fitted age and sex cox model!")

subset_vars = "age_sex_"
which_model = "model"
function_model_evaluation(input,fit_cox_model, which_model, analysis, subset_vars = subset_vars, graphics_output=TRUE, save_output = TRUE)
print("Finished model valuation for age and sex model - age as splines!")

subset_vars = "age_sex_"
which_model = "model_categorical"
function_model_evaluation(input,fit_cox_model_categorical, which_model, analysis, subset_vars = subset_vars, graphics_output=TRUE, save_output = TRUE)
print("Finished model valuation for age and sex model - age categorical!")

subset_vars = "full_"
which_model = "model_categorical"
function_model_evaluation(input,fit_cox_model_full_categorical, which_model, analysis, subset_vars = subset_vars, graphics_output=TRUE, save_output = TRUE)
print("Finished model valuation for full model - age categorical!")
