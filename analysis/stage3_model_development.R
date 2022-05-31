# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Cox model: model development and some evaluation
# Output:  hazard ratios and 95% CI
#          two CSV files, Two HTML files, and TWO SVG files

library(readr); library(dplyr); library(rms); library(MASS)
# library(survcomp) ## not yet available

####################################################################################################
# Part 1: load data, define inverse probability weighting, fit cox model and assess PH assumption  #
#         variable selection using backward elimination                                            #
####################################################################################################

# # load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_selection.R")

print("Starting stage3_model_development.R")

if(which_model == "selected"){
  fit_cox_model_selected <-rms::cph(formula= as.formula(surv_formula),
                                     data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  print("The selected model is")
  print(fit_cox_model_selected)
}

#names(fit_cox_model)

##  extract and save cox model output------------------------------------------                                 

cox_output <- function(fit_cox_model, which_model){

  ## assess proportional hazards assumption
  ph_test_result <- cox.zph(fit_cox_model, "rank")$table
  ph_test_result[,c(1,3)] <- round(ph_test_result[,c(1,3)], 3)
  write.csv(ph_test_result, file=paste0("output/review/model/PH_test_", which_model, "_", analysis, ".csv"), row.names=F)
  print(paste0("Results from proportional hazards test are saved successfully for ", which_model, " ", analysis, "!"))
  
  print("Get robust estimation")
  ## get robust variance-covariance matrix so that robust standard errors can be used in constructing CI's
  robust_fit_cox_model=rms::robcov(fit_cox_model, cluster = input$practice_id)
  
  print("completed robust estimation")
  
  ## Result
  results=as.data.frame(names(fit_cox_model$coefficients))
  colnames(results)="term"
  
  # Hazard ratio and 95% CI, P-value and S.E.
  results$hazard_ratio=exp(fit_cox_model$coefficients)
  results$conf.low = exp(fit_cox_model$coefficients - 1.96* sqrt(diag(vcov(fit_cox_model))))
  results$conf.high = exp(fit_cox_model$coefficients + 1.96* sqrt(diag(vcov(fit_cox_model))))                                                   
  results$p.value = round(pnorm(abs(fit_cox_model$coefficients/sqrt(diag(fit_cox_model$var))),lower.tail=F)*2,3)
  results$std.error=exp(sqrt(diag(vcov(fit_cox_model))))
  
  # Hazard ratio and robust estimation for variance, and the resulting 95% CI, P-value and S.E.
  results$robust.conf.low=exp(confint(robust_fit_cox_model,level=0.95)[,1]) #use robust standard errors to calculate CI
  results$robust.conf.high=exp(confint(robust_fit_cox_model,level=0.95)[,2])
  results$robust.p.value = round(pnorm(abs(robust_fit_cox_model$coefficients/sqrt(diag(robust_fit_cox_model$var))),lower.tail=F)*2,3)
  results$robust.se=round(exp(sqrt(diag(vcov(robust_fit_cox_model)))),3)
  
  results$concordance <- results$concordance.lower <- results$concordance.upper <- NA
  
  results$concordance[1] <- round(concordance(fit_cox_model)$concordance,3) #
  results$concordance.lower[1] <- round(concordance(fit_cox_model)$concordance - 1.96*sqrt((concordance(fit_cox_model))$var),3)
  results$concordance.upper[1] <- round(concordance(fit_cox_model)$concordance + 1.96*sqrt((concordance(fit_cox_model))$var),3)
  
  results[,2:ncol(results)] <- round(results[,2:ncol(results)], 3)
  print("Print results")
  print(results) 
  
  # results <-results %>% dplyr::select(-contains("robust"))
  
  write.csv(results, file=paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis, ".csv"), 
                          row.names=F)
  rmarkdown::render(paste0("analysis/compilation/compiled_HR_results",".Rmd"), 
                    output_file=paste0("hazard_ratio_estimates_", which_model, "_", analysis),
                    output_dir="output/review/model")
  print(paste0("Hazard ratio estimates are saved successfully for ", which_model, " ", analysis, "!"))
}

print("The full model is")
print(fit_cox_model)
cox_output(fit_cox_model, "full")
if(which_model == "selected"){
  cox_output(fit_cox_model_selected, "selected")
}
print("Finished stage3_model_development.R")

