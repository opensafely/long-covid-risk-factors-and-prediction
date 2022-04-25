# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: internal validation - leave one region out at a time
# Output:  

library(rms); library(fastDummies)

# load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_input_set_up.R")

region <- levels(input$sub_cat_region)

region

input_train <- input %>% filter(sub_cat_region != region[1])
input_test <- input %>% filter(sub_cat_region == region[1])

train_cox_model <-rms::cph(formula= as.formula(surv_formula),
                         data= input_train, weight=input_train$weight,surv = TRUE,x=TRUE,y=TRUE)

# in the dummy data, linear term is selected for age, 
# if, instead, spline function is selected for age, how to spell out the combination of the spline function?

# names of covariates and factor levels
covariates <- names(train_cox_model$coefficients)

sapply(strsplit(covariates, '='), `[`, 2)

#pred_level = sub(".*=", "", covariates)  # keep all characters after =
pred_level = sapply(strsplit(covariates, '='), `[`, 2)
pred_names = sub("=.*", "", covariates)  # keep all characters before =
pred_name_level = paste0(pred_names,"_", pred_level)
pred_name_level = gsub("_NA", "", pred_name_level)
pred_name_level = gsub(" ", "_", pred_name_level)
predictors <- data.frame(pred_name_level, train_cox_model$coefficients)

input$pred_LP = 0

covariates <- unique(pred_names)

factor_covars <- covariates[grepl("cat", covariates)]
input <- input %>% dplyr::select(-all_of(covariates))
input_test2 <- dummy_cols(input_test, select_columns = factor_covars)
ncol(input_test)
ncol(input_test2)
start = ncol(input_test)+1
names(input_test2)[start:ncol(input_test2)]

input_test2 <- input_test2 %>% dplyr::select(c(patient_id, all_of(pred_name_level)))

predictors_wide=as.data.frame(transpose(data.frame(train_cox_model$coefficients)))
names(predictors_wide) = paste0(pred_name_level,".coeff")
i = "cov_cat_multimorbidity_1" 
grep(i, colnames(input_test2))

i = "cov_cat_bmi_Obese I (30-34.9)"
grep(i, colnames(input_test2))


for(i in pred_name_level){
  row.index = grep(i, predictors)
  input_test2[,i] = input_test2[,i]*predictors$train_cox_model.coefficients[row.index]
}
