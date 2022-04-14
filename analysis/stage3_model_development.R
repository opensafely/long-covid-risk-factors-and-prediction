# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Development models
# Output:  hazard ratios and 95% CI
#          two CSV files, Two HTML files, and TWO SVG files

library(readr); library(dplyr); library(rms); library(MASS)

survival_data <- read_rds("output/survival_data.rds")

cases <- survival_data %>% filter(!is.na(out_first_long_covid_date) & 
                                  (out_first_long_covid_date == follow_up_end_date))

non_cases <- survival_data %>% filter(!patient_id %in% cases$patient_id)

## sample non_cases, size = 5*nrow(cases) if 5*nrow(cases) < nrow(cases)
if(nrow(cases)*5 < nrow(non_cases)){
  non_cases <- non_cases[sample(1:nrow(non_cases), nrow(cases)*5,replace=FALSE), ]
}else if (nrow(cases)*5 >= nrow(non_cases)){
  non_cases=non_cases
}

print(paste0("Number of cases: ", nrow(cases)))
print(paste0("Number of controls: ", nrow(non_cases)))

non_case_inverse_weight=(nrow(survival_data)-nrow(cases))/nrow(non_cases)

## recreate survival_data after sampling
survival_data <- bind_rows(cases,non_cases)

## extract candidate predictors
covariate_names <- names(survival_data)[grep("cov_", names(survival_data))]

## remove categorical and continuous age
covariate_names <- covariate_names[-grep("age", covariate_names)]

## remove previous covid history as a covariate
covariate_names <- covariate_names[-grep("cov_cat_previous_covid", covariate_names)]

## remove cov_cat_healthcare_worker
covariate_names <- covariate_names[-grep("cov_cat_healthcare_worker", covariate_names)]


print("candidate predictors")
covariate_names


## Add inverse probability weights for non-cases
noncase_ids <- unique(non_cases$patient_id)
survival_data$weight <-1
survival_data$weight <- ifelse(survival_data$patient_id %in% noncase_ids,
                                    non_case_inverse_weight, 1)
## for computational efficiency, only keep the variables needed in fitting the model
variables_to_keep <- c("patient_id", "practice_id",
                       "lcovid_surv_vax_c", "lcovid_i_vax_c", covariate_names,
                       "cov_num_age", "weight")

survival_data <- survival_data %>% dplyr::select(all_of(variables_to_keep))

knot_placement=as.numeric(quantile(survival_data$cov_num_age, probs=c(0.1,0.5,0.9)))
surv_formula <- paste0(
  "Surv(lcovid_surv_vax_c, lcovid_i_vax_c) ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ cluster(practice_id)"
)

surv_formula_predictors <- paste0(
  " ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)", 
  "+ cluster(practice_id)"
)
print(paste0("survival formula: ", surv_formula))

dd <<- datadist(survival_data)
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment"))

print("Fitting cox model:")

fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                        data= survival_data, weight=survival_data$weight,surv = TRUE,x=TRUE,y=TRUE)

## backward elimination
fit_cox_model_vs <- fastbw(fit_cox_model)


covariate_names <- fit_cox_model_vs$names.kept

if("cov_num_age" %in% covariate_names){
  covariate_names <- covariate_names[-grep("age", covariate_names)]
  surv_formula <- paste0(
    "Surv(lcovid_surv_vax_c, lcovid_i_vax_c) ~ ",
    paste(covariate_names, collapse = "+"),
    "+rms::rcs(cov_num_age,parms=knot_placement)", 
    "+ cluster(practice_id)"
  )
}
if(!("cov_num_age" %in% covariate_names)){
  surv_formula <- paste0(
    "Surv(lcovid_surv_vax_c, lcovid_i_vax_c) ~ ",
    paste(covariate_names, collapse = "+"),
    "+ cluster(practice_id)")
}

fit_cox_model_selected <-rms::cph(formula= as.formula(surv_formula),
                         data= survival_data, weight=survival_data$weight,surv = TRUE,x=TRUE,y=TRUE)


# validate(fit_cox_model,B=100,bw=TRUE) # repeats fastbw 100 times
# cal <-calibrate(fit_cox_model,B=100,bw=TRUE) # also repeats fastbw
# plot(cal)

# proportional hazards assumption
cox.zph(fit_cox_model, "rank")

names(fit_cox_model)

cox_output <- function(fit_cox_model, which_model){

  ## get robust variance-covariance matrix so that robust standard errors can be used in constructing CI's
  robust_fit_cox_model=rms::robcov(fit_cox_model, cluster = survival_data$practice_id)
  
  print("Cox output")
  print(fit_cox_model)
  print("Finished fitting cox model")
  
  ## Result
  results=as.data.frame(names(fit_cox_model$coefficients))
  colnames(results)="term"
  results$estimate=exp(fit_cox_model$coefficients)
  results$conf.low=exp(confint(robust_fit_cox_model,level=0.95)[,1]) #use robust standard errors to calculate CI
  results$conf.high=exp(confint(robust_fit_cox_model,level=0.95)[,2])
  results$std.error=exp(sqrt(diag(vcov(fit_cox_model))))
  results$robust.se=exp(sqrt(diag(vcov(robust_fit_cox_model))))
  
  results[,2:6] <- round(results[,2:6], 3)
  print("Print results")
  print(results)
  
  results$concordance <- NA
  
  results$concordance[1] <- round(concordance(fit_cox_model)$concordance,3)
  results$concordance[2] <- round(concordance(fit_cox_model)$concordance - 1.96*sqrt((concordance(fit_cox_model))$var),3)
  results$concordance[3] <- round(concordance(fit_cox_model)$concordance + 1.96*sqrt((concordance(fit_cox_model))$var),3)
  
  ## Calibration
  ## Predicted 365 day survival
  cal <- calibrate(fit_cox_model, cmethod=c('hare', 'KM'),
            method="boot", u=365, m=50,  B=20,
            what="observed-predicted"
            )
  
  svglite::svglite(file = paste0("output/calibration_development_cox_model_", which_model, ".svg"))
  plot(cal)
  dev.off()
  
  write.csv(results, file=paste0("output/hazard_ratio_estimates_", which_model, ".csv"), 
                          row.names=F)
  rmarkdown::render(paste0("analysis/compiled_HR_results_", which_model,".Rmd"), 
                    output_file=paste0("hazard_ratio_estimates_", which_model),
                    output_dir="output")
}

cox_output(fit_cox_model, "full")
cox_output(fit_cox_model_selected, "selected")
