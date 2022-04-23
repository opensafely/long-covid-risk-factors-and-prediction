# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Development models
# Output:  hazard ratios and 95% CI
# This script use survival::coxph instead of rms::cph to fit Cox model

library(readr); library(dplyr); library(rms); library(MASS); library(survival)
library(boot);library(flexsurv);library(survcomp)

####################################################################################################
# Part 1: load data, define inverse probability weighting, fit cox model and assess PH assumption  #
####################################################################################################

# load data set
input <- read_rds("output/input_stage1.rds")

# work out number of cases (long covid) and non-cases (without long covid)
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

## work out proportion: number of people without long covid / number of sampled (subset) observations without long covid
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
                       "lcovid_surv_vax_c", "lcovid_cens_vax_c", covariate_names,
                       "cov_num_age", "weight")

input <- input %>% dplyr::select(all_of(variables_to_keep))

## define survival formula
surv_formula <- paste0(
    "Surv(lcovid_surv_vax_c, lcovid_cens_vax_c) ~ ",
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

# surv_formula_predictors <- paste0(
#   " ~ ",
#   paste(covariate_names, collapse = "+"),
#   "+rms::rcs(cov_num_age,parms=knot_placement)", 
#   "+ cluster(practice_id)"
# )
# k10 <- qchisq(0.20,1,lower.tail=FALSE)
# backward_fit_cox_model <- stepAIC(fit_cox_model,k=k10,
#                           scope=list(upper= as.formula((surv_formula_predictors)),
#                                      lower=~1),direction="backward",trace=TRUE)


print("Cox output")
print(fit_cox_model)
print("Finished fitting cox model")

## Assess proportional hazards assumption---------------------------------------
cox.zph(fit_cox_model, "rank")


##########################################################
# Part 2: calculate apparent discrimination performance  #
##########################################################

# Calculate apparent discrimination performance

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
D <- D.index(pred_LP,surv.time=input$lcovid_surv_vax_c,surv.event=input$lcovid_cens_vax_c)$coef
D

# And R^2D
kap <- sqrt(8/pi)
sig2 <- (pi^2)/6
r2D <- (D^2 / kap^2) / (sig2 + (D^2 / kap^2))
r2D


##################################################
# Part 3.  assess the models apparent calibration#
##################################################

## Calibration slope
#fit_cox_model2<- coxph(Surv(input$lcovid_surv_vax_c,input$lcovid_cens_vax_c)~pred_LP)
fit_cox_model2<- cph(Surv(input$lcovid_surv_vax_c,input$lcovid_cens_vax_c)~pred_LP, x=TRUE,y=TRUE)
fit_cox_model2$coef

####################
# Part 4. Plotting #
####################

# Compare the bootstrap shrinkage estimate to the heuristic shrinkage previously calculated

# Plot of apparent separation across 4 groups
centile_LP <- cut(pred_LP,breaks=quantile(pred_LP, prob = c(0,0.16,0.50,0.84,1)),
                  labels=c(1:4),include.lowest=TRUE)

# Graph the KM curves in the 4 risk groups to visually assess separation
plot(survfit(Surv(input$lcovid_surv_vax_c,input$lcovid_cens_vax_c)~centile_LP),
     main="Kaplan-Meier survival estimates",
     xlab="analysis time",col=c(1:4))
legend(1,0.5,c("group=1","group=2","group=3","group=4"),col=c(1:4),lty=1,bty="n")


###############################
# Part 5. Assess for Optimism #
###############################

## assessment of optimism in the model development process

## Obtain chi-square value: compare fitted model and the null model
chi2_fit_cox_model = summary(fit_cox_model)$logtest[1]
df_fit_cox_model = summary(fit_cox_model)$logtest[2]

names(summary(fit_cox_model))

# obtain the heuristic shrinkage 

vanH <- (chi2_fit_cox_model - df_fit_cox_model)/chi2_fit_cox_model
vanH

# revise the final model

heuristic_lp = vanH*pred_LP

# summarise the original & shrunken lp and compare the mean/SD/range
mean(pred_LP)
sqrt(var(pred_LP))
min(pred_LP)
max(pred_LP)

mean(heuristic_lp)
sqrt(var(heuristic_lp))
min(heuristic_lp)
max(heuristic_lp)

# Now recalculate the calibration slope using the shrunken linear predictor
fit_cox_model3 <- coxph(Surv(input$lcovid_surv_vax_c,input$lcovid_cens_vax_c)~heuristic_lp)
fit_cox_model3$coef

# plot original predictions (before shrinkage) versus our shrunken model predictions
# To do this we can plot the KM curve for one high risk patient, and one low risk patient using the original and shrunken model lp
lpdat <- cbind(input,pred_LP)
patient_high <- subset(lpdat, pred_LP == max(pred_LP)) 
patient_low <- subset(lpdat,pred_LP == min(pred_LP))

# Calculate shrunken LP for these patients
patient_high_shrunk <- patient_high
patient_high_shrunk$pred_LP <- patient_high$pred_LP*vanH
patient_low_shrunk <- patient_low
patient_low_shrunk$pred_LP <- patient_low$pred_LP*vanH

plot(survfit(fit_cox_model,newdata=data.frame(patient_high)),main="Cox proportional hazards regression",xlab="analysis time",ylab="Survival",col=1,conf.int=FALSE)
lines(survfit(fit_cox_model,newdata=data.frame(patient_high_shrunk)),col=2,conf.int=FALSE)
lines(survfit(fit_cox_model,newdata=data.frame(patient_low)),col=3,conf.int=FALSE)
lines(survfit(fit_cox_model,newdata=data.frame(patient_low_shrunk)),col=4,conf.int=FALSE)
legend(1,0.4,c("Original LP - High risk","Shrunken LP - High risk","Original LP - Low risk","Shrunken LP - Low risk"),col=c(1:4),lty=1,bty="n")

# Linear predictor values
patient_high$pred_LP
patient_high_shrunk$pred_LP

# obtain an estimate of the baseline survival at a specific time point, for the shrunken model
# First obtain an estimate of the baseline survival at 180 days for the original model
day180_Cox <- summary(survfit(fit_cox_model),time=180)$surv
day180_Cox

# Now calculate the shrunken models baseline survival prob at 5 years by setting the shrunken lp as a offset and predicting the baseline survival
shrunk_mod <- coxph(Surv(input$lcovid_surv_vax_c,input$lcovid_cens_vax_c)~offset(heuristic_lp))
day180_Cox_shrunk <- summary(survfit(shrunk_mod),time=5)$surv
day180_Cox_shrunk

# Estimate the predicted survival probability at 180 days for the high risk patient above
prob_HR <- day180_Cox^exp(patient_high$pred_LP)
prob_HR
prob_HR <- day180_Cox^exp(patient_high$pred_LP)
prob_HR
prob_HR_shrunk <- day180_Cox_shrunk^exp(patient_high_shrunk$pred_LP)
prob_HR_shrunk

# We can plot the two baseline survival curves
plot(survfit(fit_cox_model),main="Cox proportional hazards regression",xlab="analysis time",ylab="Survival",col=1,conf.int=FALSE)
lines(survfit(shrunk_mod),col=2,lty=2,conf.int=FALSE)
legend(7.5,0.9,c("Original LP - High risk","Shrunken LP - High risk"),col=c(1:2),lty=1,bty="n")

# abline(h=) adds a line crossing the y-axis at the baseline survival probabilities
abline(h=day180_Cox,col="black")
abline(h=day180_Cox_shrunk,col="red")
abline(v=5,col="red")

# Re-plot the high risk patient curves & draw on lines corresponding to the patients survival probability at 5yrs 
# as calculated above to check they match the predicted survival curves
plot(survfit(fit_cox_model2,newdata=data.frame(patient_high)),main="Cox proportional hazards regression",xlab="analysis time",ylab="Survival",col=1,conf.int=FALSE)
lines(survfit(fit_cox_model2,newdata=data.frame(patient_high_shrunk)),col=2,conf.int=FALSE)
legend(10,0.9,c("Original LP - High risk","Shrunken LP - High risk"),col=c(1:2),lty=1,bty="n")
abline(h=prob_HR,col="black")
abline(h=prob_HR_shrunk,col="red")
abline(v=180,col="red")

##########################################################
# Part 6. Internal validation using boostrap validation  #
##########################################################

#  perform internal validation using bootstrap validation. 
library(rms)
dd <<- datadist(input)
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment"))
fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                         data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

set.seed(12345) # to ensure reproducibility
boot_1 <- validate(fit_cox_model,B=100) 
# cal <-calibrate(fit_cox_model,B=100,bw=TRUE) # also repeats fastbw
# plot(cal)
boot_1

# Note that this gives Dxy rather than c, however Dxy = 2*(c-0.5), i.e. c=(Dxy/2)+0.5
(boot_1[1,1]+1)/2
(boot_1[1,5]+1)/2


###############################################################
# Part 7. Shrinkage & Optimism adjusted performance measures #
###############################################################

# Shrinkage & optimism adjusted C-stat, C-slope etc. using bootstrapping 
# with predictor selection methods

# In the above bootstrap we have fit the final model to each bootstrap sample, however 
# TRIPOD recommends that internal validation should replicate the entire model development 
# process. This includes the model selection procedure, so we will now extend the 
# bootstrapping methodology from above to include predictor selection using backward elemination. 

fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                         data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

# Shrinkage & optimism adjusted AUC, CITL etc. using bootstrapping with predictor selection methods
k10 <- qchisq(0.20,1,lower.tail=FALSE)
set.seed(12345) # to ensure reproducibility
boot_2 <- validate(fit_cox_model,B=100,rule="aic",aics=k10)
#boot_2 <- validate(fit_cox_model,B=100,rule="aic", bw=TRUE,aics=k10)
boot_2

# Convert Dxy to c-index
(boot_2[1,1]+1)/2
(boot_2[1,5]+1)/2


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

