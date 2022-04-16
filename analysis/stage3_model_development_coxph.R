# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Development models
# Output:  hazard ratios and 95% CI
# This script use survival::coxph instead of rms::cph to fit Cox model

library(readr); library(dplyr); library(rms); library(MASS); library(survival)
library(boot);library(flexsurv);library(survcomp)

input <- read_rds("output/input_stage1.rds")

cases <- input %>% filter(!is.na(out_first_long_covid_date) & 
                                  (out_first_long_covid_date == follow_up_end_date))

non_cases <- input %>% filter(!patient_id %in% cases$patient_id)

## sample non_cases, size = 5*nrow(cases) if 5*nrow(cases) < nrow(cases)
if(nrow(cases)*5 < nrow(non_cases)){
  non_cases <- non_cases[sample(1:nrow(non_cases), nrow(cases)*5,replace=FALSE), ]
}else if (nrow(cases)*5 >= nrow(non_cases)){
  non_cases=non_cases
}

print(paste0("Number of cases: ", nrow(cases)))
print(paste0("Number of controls: ", nrow(non_cases)))

non_case_inverse_weight=(nrow(input)-nrow(cases))/nrow(non_cases)

## recreate input after sampling
input <- bind_rows(cases,non_cases)

## extract candidate predictors
covariate_names <- names(input)[grep("cov_", names(input))]

## remove categorical age
covariate_names <- covariate_names[-grep("cov_cat_age", covariate_names)]

## remove previous covid history as a covariate
covariate_names <- covariate_names[-grep("cov_cat_previous_covid", covariate_names)]

## remove cov_cat_healthcare_worker
covariate_names <- covariate_names[-grep("cov_cat_healthcare_worker", covariate_names)]

print("candidate predictors")
covariate_names

##Add inverse probability weights for non-cases
noncase_ids <- unique(non_cases$patient_id)
input$weight <-1
input$weight <- ifelse(input$patient_id %in% noncase_ids,
                                    non_case_inverse_weight, 1)
## for computational efficiency, only keep the variables needed in fitting the model
variables_to_keep <- c("patient_id", "practice_id",
                       "lcovid_surv_vax_c", "lcovid_i_vax_c", covariate_names,
                       "cov_num_age", "weight")

input <- input %>% dplyr::select(all_of(variables_to_keep))


## define survival formula
surv_formula <- paste0(
    "Surv(lcovid_surv_vax_c, lcovid_i_vax_c) ~ ",
    paste(covariate_names, collapse = "+"),
    "+ cluster(practice_id)"
  )

print(paste0("survival formula: ", surv_formula))

## fit Cox model
  
print("Fitting cox model using coxph:")

fit_cox_model <- coxph(formula= as.formula(surv_formula),
                        data= input, weight=input$weight,
                        x=TRUE,y=TRUE, robust=T)

names(fit_cox_model)


# k10 <- qchisq(0.20,1,lower.tail=FALSE)
# backward_fit_cox_model <- stepAIC(fit_cox_model,k=k10,
#                           scope=list(upper= as.formula((surv_formula_predictors)),
#                                      lower=~1),direction="backward",trace=TRUE)

names(fit_cox_model)

print("Cox output")
print(fit_cox_model)
print("Finished fitting cox model")

## Assess proportional hazards assumption---------------------------------------
cox.zph(fit_cox_model, "rank")

##--Assess model performance --------------------------------------------------

## Obtain the linear predictor
pred_LP <- predict(fit_cox_model,type="lp",reference="sample")

mean(pred_LP)
sd(pred_LP)
min(pred_LP)
max(pred_LP)


# C statistics
concordance(fit_cox_model)
summary(fit_cox_model)$concordance[1]
summary(fit_cox_model)$concordance[1]-(1.96*summary(fit_cox_model)$concordance[2])
summary(fit_cox_model)$concordance[1]+(1.96*summary(fit_cox_model)$concordance[2])

# Calculate & interpret the D statistic, R_D^2 and R_D^2 
# To obtain the Royston & Sauerbrei's D
D <- D.index(pred_LP,surv.time=input$lcovid_surv_vax_c,surv.event=input$lcovid_i_vax_c)$coef
D

# And R^2D
kap <- sqrt(8/pi)
sig2 <- (pi^2)/6
r2D <- (D^2 / kap^2) / (sig2 + (D^2 / kap^2))
r2D

## Assess calibration----------------------------------------------------------

## Calibration slope
fit_cox_model2<- coxph(Surv(input$lcovid_surv_vax_c,input$lcovid_i_vax_c)~pred_LP)
fit_cox_model2$coef

# # Plot of apparent separation across 4 groups
# centile_LP <- cut(pred_LP,breaks=quantile(pred_LP, prob = c(0,0.16,0.50,0.84,1)),
#                   labels=c(1:4),include.lowest=TRUE)
# 
# 
# ## KM curves are not allowed??
# # Graph the KM curves in the 4 risk groups to visually assess separation
# plot(survfit(Surv(input$lcovid_surv_vax_c,input$lcovid_i_vax_c)~centile_LP),
#      main="Kaplan-Meier survival estimates",
#      xlab="analysis time",col=c(1:4))
# legend(10,1,c("group=1","group=2","group=3","group=4"),col=c(1:4),lty=1,bty="n")

## Result

# results=as.data.frame(names(fit_cox_model$coefficients))
# colnames(results)="term"
# results$estimate=exp(fit_cox_model$coefficients)
# results$conf.low=exp(confint(robust_fit_cox_model,level=0.95)[,1]) #use robust standard errors to calculate CI
# results$conf.high=exp(confint(robust_fit_cox_model,level=0.95)[,2])
# results$std.error=exp(sqrt(diag(vcov(fit_cox_model))))
# results$robust.se=exp(sqrt(diag(vcov(robust_fit_cox_model))))
# 
# results[,2:6] <- round(results[,2:6], 3)
# print("Print results")
# print(results)
# 
# results$concordance <- NA
# 
# results$concordance[1] <- round(concordance(fit_cox_model)$concordance,3)
# results$concordance[2] <- round(concordance(fit_cox_model)$concordance - 1.96*sqrt((concordance(fit_cox_model))$var),3)
# results$concordance[3] <- round(concordance(fit_cox_model)$concordance + 1.96*sqrt((concordance(fit_cox_model))$var),3)
# 
# 
# write.csv(results, file="output/hazard_ratio_estimates.csv", row.names=F)
# 
# rmarkdown::render("analysis/compiled_HR_results.Rmd", output_file="hazard_ratio_estimates",output_dir="output")
# 

