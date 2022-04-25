# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: internal validation - leave one region out at a time
# Output:  

library(rms)

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






