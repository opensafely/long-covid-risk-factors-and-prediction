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

# load data, with defined weight, and import formula for survival analysis 
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
    if("cov_num_age" %in% covariate_names){
      covariate_names <- covariate_names[-grep("age", covariate_names)]
      surv_formula <- paste0(
        "Surv(lcovid_surv, lcovid_cen) ~ ",
        paste(covariate_names, collapse = "+"),
        "+rms::rcs(cov_num_age,parms=knot_placement)", 
        "+ cluster(practice_id)"
      )
    }
    if(!("cov_num_age" %in% covariate_names)){
      surv_formula <- paste0(
        "Surv(lcovid_surv, lcovid_cen) ~ ",
        paste(covariate_names, collapse = "+"),
        "+ cluster(practice_id)")
    }
  print(surv_formula)
  fit_cox_model_selected <-rms::cph(formula= as.formula(surv_formula),
                                    data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
}

## assess proportional hazards assumption
cox.zph(fit_cox_model, "rank")

names(fit_cox_model)


##  extract and save cox model output------------------------------------------                                 

cox_output <- function(fit_cox_model, which_model){

  ## get robust variance-covariance matrix so that robust standard errors can be used in constructing CI's
  robust_fit_cox_model=rms::robcov(fit_cox_model, cluster = input$practice_id)
  
  print("Cox output")
  print(fit_cox_model)
  print("Finished fitting cox model")
  
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
  
  results[,2:9] <- round(results[,2:9], 3)
  print("Print results")
  print(results) 
  
  results$concordance <- NA
  
  results$concordance[1] <- round(concordance(fit_cox_model)$concordance,3)
  results$concordance[2] <- round(concordance(fit_cox_model)$concordance - 1.96*sqrt((concordance(fit_cox_model))$var),3)
  results$concordance[3] <- round(concordance(fit_cox_model)$concordance + 1.96*sqrt((concordance(fit_cox_model))$var),3)
  
  ## Calibration
  ## Predicted 365 day survival
  if(which_model == "full"){
  cal <- calibrate(fit_cox_model, cmethod=c('hare', 'KM'),
            method="boot", u=365, m=50,  B=20,
            what="observed-predicted"
            )
  
  svglite::svglite(file = paste0("output/calibration_development_cox_model_", which_model, ".svg"))
  plot(cal)
  dev.off()
  }
  
  # results <-results %>% dplyr::select(-contains("robust"))
  
  write.csv(results, file=paste0("output/hazard_ratio_estimates_", which_model, ".csv"), 
                          row.names=F)
  rmarkdown::render(paste0("analysis/compiled_HR_results",".Rmd"), 
                    output_file=paste0("hazard_ratio_estimates_", which_model),
                    output_dir="output")
}

cox_output(fit_cox_model, "full")
if(length(selected_covariate_names)>0){
  cox_output(fit_cox_model_selected, "selected")
}


