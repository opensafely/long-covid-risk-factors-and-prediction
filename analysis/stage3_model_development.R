# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Development models
# Output:  hazard ratios and 95% CI

library(readr); library(dplyr); library(rms)

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

# extract candidate preditors
covariate_names <- names(survival_data)[grep("cov_", names(survival_data))]

# remove categorical age
covariate_names <- covariate_names[-grep("cov_cat_age_", covariate_names)]

# remove previous covid history as covariate
covariate_names <- covariate_names[-grep("cov_cat_previous_covid", covariate_names)]

print("candidate predictors")
covariate_names

surv_formula <- paste0(
  "Surv(lcovid_surv_vax_c, lcovid_i_vax_c) ~ ",
  paste(covariate_names, collapse = "+"),
  "+ cluster(practice_id)"
)

print(paste0("survival formula: ", surv_formula))

## for computational efficiency, only keep the variables needed in fitting the model
variables_to_keep <- c("patient_id", "practice_id", 
                       "lcovid_surv_vax_c", "lcovid_i_vax_c", covariate_names)

survival_data <- survival_data %>% select(all_of(variables_to_keep))

# have to remove the following line eventually
dd <<- datadist(survival_data)
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment"))

print("Fitting cox model:")


fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                        data= survival_data, weight=1,surv = TRUE,x=TRUE,y=TRUE)

# proportional hazards assumption
#cox.zph(fit_cox_model, "rank")

names(fit_cox_model)

# get robust variance-covariance matrix so that robust standard errors can be used in constructing CI's
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

write.csv(results, file="output/hazard_ratio_estimates.csv", row.names=F)

rmarkdown::render("analysis/compiled_HR_results.Rmd", output_file="hazard_ratio_estimates",output_dir="output")


