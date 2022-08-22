# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: model development and some evaluation
# Output:  hazard ratios and 95% CI


library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available

####################################################################################################
# Part 1: load data, define inverse probability weighting, fit cox model and assess PH assumption  #
#         variable selection using backward elimination                                            #
####################################################################################################

# # load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_selection.R")
source("analysis/functions/function_cox_output.R")

print("Starting stage3_model_development.R")

if(which_model == "selected"){
  fit_cox_model_selected <-rms::cph(formula= as.formula(surv_formula_selected),
                                     data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  print("The selected model is")
  print(fit_cox_model_selected)
}

# Model 1
print("Output Cox model with age as spline:")
print(fit_cox_model_splines)
output_file = paste0("output/review/model/HR_", "full_model_age_spline", "_", analysis)
cox_output2(fit_cox_model_splines, "full_model_age_splines",output_file, save_output=TRUE)

# Model 2
print("Output Cox model with age linear:")
print(fit_cox_model_linear)
output_file = paste0("output/review/model/HR_", "full_model_age_linear", "_", analysis)
cox_output2(fit_cox_model_linear, "full_model_age_linear",output_file, save_output=TRUE)

# Model 3
if(which_model == "selected"){
  output_file = paste0("output/review/model/HR_", which_model, "_", analysis)
  cox_output2(fit_cox_model_selected, "selected_model", output_file, save_output = TRUE)
}
print("Finished stage3_model_development.R")

# Output AIC from all three models

df_aic <- data.frame(analysis,AIC(fit_cox_model_splines), AIC(fit_cox_model_linear))

if(which_model == "selected"){
  df_aic <- cbind(df_aic, AIC(fit_cox_model_selected))
}

df_aic[,2:4]<- round(df_aic[,2:4],2)

write.csv(df_aic, file=paste0("output/review/model/AIC_",analysis,".csv"))

#RK - you're getting large robust SE's in your results - I ran your model without +cluster(practice_id)
#and the robust SE's were then fine - potentially something to invetsigate?

#YW - That's a good point! I am now trying to use strat(sub_cat_region) instead - it does work well
# with strat(sub_cat_region). But we now use region as a covariate instead