# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: internal validation - leave one region out at a time
# Output:  

library(rms); library(fastDummies)

# load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_input_set_up.R")

pm <- data.frame(c_stat = numeric(),
                 c_stat_lower = numeric(),
                 c_stat_upper = numeric(),
                 cal_slope = numeric)

region <- levels(input$sub_cat_region)

region

input_train <- input %>% filter(sub_cat_region != region[1])
input_test <- input %>% filter(sub_cat_region == region[1])

train_cox_model <-rms::cph(formula= as.formula(surv_formula),
                         data= input_train, weight=input_train$weight,surv = TRUE,x=TRUE,y=TRUE)

# in the dummy data, linear term is selected for age, 
# what if, instead, spline function is selected for age?

# names of covariates and factor levels
covariates <- names(train_cox_model$coefficients)

#pred_level = sub(".*=", "", covariates)  # keep all characters after =
pred_level = sapply(strsplit(covariates, '='), `[`, 2)
pred_names = sub("=.*", "", covariates)  # keep all characters before =
pred_name_level <- paste0(pred_names, "_", pred_level)
pred_name_level = gsub("_NA", "", pred_name_level)

predictors <- data.frame(pred_name_level, train_cox_model$coefficients)
covariates <- unique(pred_names)
factor_covars <- covariates[grepl("cat", covariates)]
input <- input %>% dplyr::select(-all_of(covariates))
input_test2 <- dummy_cols(input_test, select_columns = factor_covars)
start = ncol(input_test)+1
names(input_test2)[start:ncol(input_test2)]

input_test2 <- input_test2 %>% dplyr::select(c(patient_id, all_of(pred_name_level)))
predictors_wide=as.data.frame(transpose(data.frame(train_cox_model$coefficients)))
names(predictors_wide) = paste0(pred_name_level,".coeff")

for(i in pred_name_level){
  print(i)
  row.index = which(pred_name_level ==i)
  input_test2[,i] = input_test2[,i]*predictors$train_cox_model.coefficients[row.index]
}
cov_cols <- names(input_test2)[grep("cov", names(input_test2))]

input_test2 <- input_test2 %>% mutate(lin_pred = rowSums(.[ , cov_cols])) %>%
                dplyr::select(c(patient_id,lin_pred))

## left join: keep all observations in input_select
input <- merge(x = input_test2, y = input, by = "patient_id", all.x = TRUE)

# Calibration slope
test_cox_model <- cph(Surv(lcovid_surv,lcovid_cens)~lin_pred,data = input, method="breslow")
test_cox_model$coef

# Calculate the C-statistic for the discrimination of the model in the validation dataset
# Harrell's C-statistic 
round(concordance(test_cox_model)$concordance,3)
round(concordance(test_cox_model)$concordance - 1.96*sqrt((concordance(test_cox_model))$var),3)
round(concordance(test_cox_model)$concordance + 1.96*sqrt((concordance(test_cox_model))$var),3)

# Calibration plot for the validation data
# Calculate predicted survival probability at 1 year
time_point = 365
y1_cox <- summary(survfit(train_cox_model),time=time_point)$surv
y1_cox

pred_surv_prob = y1_cox^exp(input_test2$lin_pred)

val_ests <- val.surv(est.surv = pred_surv_prob,
                     S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
                     u=time_point,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))

plot(val_ests,xlab="Expected Survival Probability",ylab="Observed Survival Probability") 
groupkm(pred_surv_prob, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
        g=10,u=time_point, pl=T, add=T,lty=0,cex.subtitle=FALSE)
legend(0.0,0.8,c("Risk groups","Reference line","95% CI"),lty=c(0,2,1),pch=c(19,NA,NA),bty="n")

# Recalibration of the baseline survival function
recal_mod <- coxph(Surv(input_test$lcovid_surv,input_test$lcovid_cens)~offset(input_test2$lin_pred))
y_recal_1y <- summary(survfit(recal_mod),time=180)$surv
y_recal_1y

# Calculate new predicted probabilities at 5 years (linear predictor stays the same but needs centering)
pred_surv_prob2=y_recal_1y^exp(input_test2$lin_pred-mean(input_test2$lin_pred))
mean(pred_surv_prob2)
sd(pred_surv_prob2)

# Redo calibration plot
val_ests2 <- val.surv(est.surv = pred_surv_prob2,
                      S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
                      u=180,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))

plot(val_ests2,xlab="Expected Survival Probability",ylab="Observed Survival Probability") 
groupkm(pred_surv_prob2, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
        g=10,u=180, pl=T, add=T,lty=0,cex.subtitle=FALSE)
legend(0.0,0.9,c("Risk groups","Reference line","95% CI"),lty=c(0,2,1),pch=c(19,NA,NA),bty="n")
