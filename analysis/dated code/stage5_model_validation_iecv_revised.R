# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: internal-external validation:
#          1. prognostic model developed based on the whole data set: all predictors (do not select variables)
#          2. leave one region out at a time, estimate beta based on data without region k, no new test for significance or variable selections
#          3. test the prognostic model on the left out region to obtain C_k
#          4. test the prognostic model on the all regions without k to obtain C_(k)
#          repeat 2-4 for each region
#          5. calculate the difference between C_k and C_(k), diff_c_k = C_k - C_(k)
#          6. calculate difference between the two sets of estimates, calculate overall diff_c using weighted average 
#          7. Calculate the weighted performance measure C_iecv
# Output:  One table: val_performance_measures.csv
#          val_cal_plot.svg for each region
#          val_re_cal_plot.svg for each region
# to do: validate the full model

library(rms); library(fastDummies)

print("Starting stage_5_model_validation.R")

#load data, with defined weight, and import formula for survival analysis
source("analysis/stage3_model_selection.R")

which_model = "full"

print("Finished loading fitted cox model!")

splines_for_age = grepl("rms::rcs", surv_formula)

region <- levels(input$sub_cat_region)
region
make_df <- function(nrow) {
  if (missing(nrow)) {
    nrow <- 0
  }
  temp_df <- data.frame(
    analysis = character(),
    which_model = character(),
    region  = character(),
    c_stat_without_k  = numeric(),  # this is C_(k)
    c_stat_var_without_k = numeric(), 
    c_stat_lower_without_k = numeric(),
    c_stat_upper_without_k = numeric(),
    c_stat  = numeric(),  # this is C_k
    c_stat_var = numeric(), 
    c_stat_lower = numeric(),
    c_stat_upper = numeric(),
    cal_slope = numeric(),
    stringsAsFactors = FALSE
  )
  if (nrow > 0) {
    temp_df[1:nrow,] <- NA
  }
  temp_df
} 
pm_val <- make_df(length(region))

pm_val$analysis = analysis
pm_val$which_model = which_model

print("Data frame for pm is created successfully!")

# for testing
# region_i = "London"
int_ext_cross_validation <- function(region_i, fit_cox_model){
  
  print(paste0("---- START Region: ", region_i, " ----"))
  
  pm_val$region = region_i
  #input_train <- input %>% filter(sub_cat_region != region_i)
  input_test <- input %>% filter(sub_cat_region == region_i)
  if(analysis == "all_vax_td"){
    input_test$patient_id <- seq.int(nrow(input_test))  # reset patient id as they are not unique in this case
  }
  
  # names of covariates and factor levels
  covariates <- names(fit_cox_model$coefficients)
  
  #pred_level = sub(".*=", "", covariates)  # keep all characters after =
  pred_level = sapply(strsplit(covariates, '='), `[`, 2)
  pred_names = sub("=.*", "", covariates)  # keep all characters before =
  pred_name_level <- paste0(pred_names, "_", pred_level)
  pred_name_level = gsub("_NA", "", pred_name_level)
  
  predictors <- data.frame(pred_name_level, fit_cox_model$coefficients)
  covariates <- unique(pred_names)
  factor_covars <- covariates[grepl("cat", covariates)]
  #input_test <- input_test %>% dplyr::select(-all_of(covariates))
  input_test_select <- dummy_cols(input_test, select_columns = factor_covars)
  start = ncol(input_test)+1
  #names(input_test_select)[start:ncol(input_test_select)]
  print("extract covariate name if splines are selected for age!")
  if(splines_for_age == TRUE){
    x1 = input_test_select$cov_num_age 
    r1 = rcs(x1,parms=knot_placement)
    a <- data.matrix(r1)
    a1 <- a[,1]
    a2 <- a[,2]
    a <- data.frame(a1,a2)
    names(a) <- c("rcs1","rcs2")
    input_test_select$cov_num_age_rcs2 = c(a[,2])
    rm(a)
    names(input_test_select)[grep("cov_num_age_rcs2", names(input_test_select))] = "cov_num_age'"
    print("Covariate names for age (splines) have been extracted successfully!")
  }
  input_test_select <- input_test_select %>% dplyr::select(c(patient_id, all_of(pred_name_level)))
  
  for(j in pred_name_level){
    print(j)
    row.index = which(pred_name_level ==j)
    input_test_select[,j] = input_test_select[,j]*predictors$fit_cox_model.coefficients[row.index]
  }
  cov_cols <- names(input_test_select)[grep("cov", names(input_test_select))]
  
  # Here the linear predictor is calculated for the testing data, using the linear combination of covariates
  if(length(cov_cols)>1){
    input_test_select <- input_test_select %>% mutate(lin_pred = rowSums(.[ , cov_cols])) %>%
      dplyr::select(c(patient_id,lin_pred))
  }
  if(length(cov_cols)==1){
    input_test_select <- input_test_select %>% mutate(lin_pred = input_test_select[,cov_cols]) %>%
      dplyr::select(c(patient_id,lin_pred))
  }
  
  ## left join: keep all observations in input_select
  input_test <- merge(x = input_test_select, y = input_test, by = "patient_id", all.x = TRUE)
  # Cox model for testing data
  test_cox_model <- cph(Surv(lcovid_surv,lcovid_cens)~lin_pred,data = input_test, method="breslow")
  
  # Calculate the C-statistic for the discrimination of the model in the validation dataset
  # Harrell's C-statistic for C_k: data from region k
  c_stat = round(concordance(test_cox_model)$concordance,3)
  c_stat_lower = round(concordance(test_cox_model)$concordance - 1.96*sqrt((concordance(test_cox_model))$var),3)
  c_stat_upper = round(concordance(test_cox_model)$concordance + 1.96*sqrt((concordance(test_cox_model))$var),3)
  c_stat_var = round((concordance(test_cox_model))$var,6)
  
  print("Calculation for the C-statistic is completed!")
  
  # Calibration slope
  cal_slope = round(test_cox_model$coef,3)
  # Calibration plot for the validation data
  # Calculate predicted survival probability at 1 year
  time_point = 365
  y1_cox <- summary(survfit(fit_cox_model),time=time_point)$surv
  y1_cox
  
  pred_surv_prob = y1_cox^exp(input_test_select$lin_pred)
  
  val_ests <- val.surv(est.surv = pred_surv_prob,
                       S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
                       u=time_point,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))
  print("val_ests is now specified!")
  
  svg(paste0("output/review/model/val_cal_plot_",region_i,"_", analysis, ".svg"))
  plot(val_ests,xlab="Expected Survival Probability",ylab="Observed Survival Probability")
  groupkm(pred_surv_prob, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens),
          g=10,u=time_point, pl=T, add=T,lty=0,cex.subtitle=FALSE)
  legend(0.0,0.8,c("Risk groups","Reference line","95% CI"),lty=c(0,2,1),pch=c(19,NA,NA),bty="n")
  # temp <- data.frame(groupkm(pred_surv_prob, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), u=time_point))
  # lb <- temp$KM - 1.96*temp$std.err
  # groupkm(pred_surv_prob, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
  #         g=10,u=time_point, pl=T, lty=0,cex.subtitle=FALSE,
  #         xlim = c(min(pred_surv_prob), 1), ylim=c(min(lb),1),
  #         xlab="Predicted Survival Probability",ylab="Observed Survival Probability") 
  # # + abline(coef = c(0,1))
  dev.off()
  print("Calibration plot is created successfully!")
  
  # Recalibration of the baseline survival function
  recal_mod <- coxph(Surv(input_test$lcovid_surv,input_test$lcovid_cens)~offset(input_test_select$lin_pred))
  y_recal_1y <- summary(survfit(recal_mod),time=time_point)$surv
  y_recal_1y
  
  # Calculate new predicted probabilities at 1 year
  pred_surv_prob2=y_recal_1y^exp(input_test_select$lin_pred-mean(input_test_select$lin_pred))
  mean(pred_surv_prob2)
  sd(pred_surv_prob2)
  print("Calculation of new predicted probabilities at 1 year is completed successfully!")
  
  # Redo calibration plot
  val_ests2 <- val.surv(est.surv = pred_surv_prob2,
                        S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
                        u=time_point,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))
  
  print("val_ests2 is now specified!")
  svg(paste0("output/review/model/val_re_cal_plot_",region_i,"_", analysis, ".svg"))
  plot(val_ests2,xlab="Expected Survival Probability",ylab="Observed Survival Probability")
  groupkm(pred_surv_prob2, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens),
          g=10,u=time_point, pl=T, add=T,lty=0,cex.subtitle=FALSE)
  legend(0.0,0.9,c("Risk groups","Reference line","95% CI"),lty=c(0,2,1),pch=c(19,NA,NA),bty="n")
  # temp <- data.frame(groupkm(pred_surv_prob2, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), u=time_point))
  # lb <- temp$KM - 1.96*temp$std.err
  # groupkm(pred_surv_prob2, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
  #         g=10,u=time_point, pl=T, lty=0,cex.subtitle=FALSE,
  #         xlim = c(min(pred_surv_prob2), 1), ylim=c(min(lb),1),
  #         xlab="Predicted Survival Probability",ylab="Observed Survival Probability") 
  dev.off()
  print("Re-calibration plot is created successfully!")
  print(paste0("---- END Region: ", region_i, " ----"))
  return(c(c_stat, c_stat_var, c_stat_lower, c_stat_upper,  cal_slope))
}

for(i in 1:length(region)){
  print(i)
  pm_val[i,1] = region[i]
  input_train <- input %>% filter(sub_cat_region != region[i])
  fit_cox_model <-rms::cph(formula= as.formula(surv_formula), data= input_train, 
                           weight=input_train$weight,surv = TRUE,x=TRUE,y=TRUE)
  # Calculate the C-statistic for the discrimination of the model in the validation dataset
  # Harrell's C-statistic for C_k: data from region k
  c_stat_without_k = round(concordance(fit_cox_model)$concordance,4)
  c_stat_lower_without_k = round(concordance(fit_cox_model)$concordance - 1.96*sqrt((concordance(fit_cox_model))$var),4)
  c_stat_upper_without_k = round(concordance(fit_cox_model)$concordance + 1.96*sqrt((concordance(fit_cox_model))$var),4)
  c_stat_var_without_k = round((concordance(fit_cox_model))$var,6)
  temp = try(int_ext_cross_validation(region[i], fit_cox_model))
  pm_val[i,4:7] <- c(c_stat_without_k, c_stat_lower_without_k, c_stat_upper_without_k, c_stat_var_without_k)
  if(is.numeric(temp)){
    pm_val[i,8:ncol(pm_val)] = temp
  }
}

# weight for each region
pm_val$weight = 1/pm_val$c_stat_var
# standardized weight for each region
pm_val$s_weight = pm_val$weight/(mean(pm_val$weight))
# an internal-external cross-validation estimate of C Statistic
c_iecv  = sum(pm_val$c_stat*pm_val$s_weight)/nrow(pm_val)
# standard error of the overall C statistic
c_iecv_var = mean(pm_val$s_weight*(pm_val$c_stat - c_iecv)^2)/(nrow(pm_val)-1)
c_iecv_se = sqrt(c_iecv_var)

pm_val$c_iecv = pm_val$c_iecv_lower = pm_val$c_iecv_upper = NA
pm_val$c_iecv[1] = round(c_iecv,4)
pm_val$c_iecv_lower[1] = round(c_iecv - 1.96*c_iecv_se,4)
pm_val$c_iecv_upper[1] = round(c_iecv + 1.96*c_iecv_se,4)

pm_val <- pm_val %>% dplyr::select(!region)
pm_val$c_diff <- pm_val$c_stat_without_k - pm_val$c_stat   # this is C_k - C_(k) in Royston et al (2004)
pm_val$c_diff_weight_star =  1/(pm_val$c_stat_var_without_k + pm_val$c_stat_var) 
pm_val$c_diff_average_weight = mean(pm_val$c_diff_weight_star)
pm_val$c_diff_weight = pm_val$c_diff_weight_star / pm_val$c_diff_average_weight
#pm_val$c_diff_overall = pm_val$c_diff_overall_se = pm_val$c_diff_overall_lower = pm_val$c_diff_overall_upper = NA
pm_val$c_diff_overall[1] = sum(pm_val$c_diff * pm_val$c_diff_weight)/(nrow(pm_val)-1)
pm_val$c_diff_overall_se[1] = sum(pm_val$c_diff_weight *(pm_val$c_diff - pm_val$c_diff_overall[1])^2)/(nrow(pm_val)-1)/(nrow(pm_val)-2)
pm_val$c_diff_overall_se[1] = sqrt(pm_val$c_diff_overall_se[1])
pm_val$c_diff_overall_lower[1] = round(pm_val$c_diff_overall[1] - 1.96*pm_val$c_diff_overall_se[1],4)
pm_val$c_diff_overall_upper[1] = round(pm_val$c_diff_overall[1] + 1.96*pm_val$c_diff_overall_se[1],4)

pm_val$c_diff_average_weight[2:nrow(pm_val)] =pm_val$c_diff_overall[2:nrow(pm_val)] = pm_val$c_diff_overall_se [2:nrow(pm_val)] = pm_val$c_diff_overall_lower[2:nrow(pm_val)] = pm_val$c_diff_overall_upper[2:nrow(pm_val)] = NA
pm_val[3:ncol(pm_val)] = round(pm_val[3:ncol(pm_val)], 4)
write.csv(pm_val, file = paste0("output/review/model/val_performance_measures_", analysis,".csv"), row.names = F)

rmarkdown::render(paste0("analysis/compilation/compiled_val_pm_table",".Rmd"), 
                  output_file=paste0("val_performance_measures_", analysis),
                  output_dir="output/review/model")
