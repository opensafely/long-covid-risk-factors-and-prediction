# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: internal-external validation - leave one region out at a time
# Output:  One table: val_performance_measures.csv
#          val_cal_plot.svg for each region
#          val_re_cal_plot.svg for each region

library(rms); library(fastDummies)

# load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_input_set_up.R")

# Assumption: linear term is selected for age
#surv_formula = surv_formula_lp

# Assumption: spline function is selected for age - extract estimated parameters from fit_cox_model_splines
#splines_for_age = TRUE
splines_for_age = grepl("rms::rcs", surv_formula)


region <- levels(input$sub_cat_region)
region
make_df <- function(nrow) {
  if (missing(nrow)) {
    nrow <- 0
  }
  temp_df <- data.frame(
    region_left_out  = character(),
    c_stat = numeric(),
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

print("Data frame for pm is created successfully!")

for(i in 1:length(region)){
    pm_val$region_left_out[i] = region[i]
    input_train <- input %>% filter(sub_cat_region != region[i])
    input_test <- input %>% filter(sub_cat_region == region[i])
    if(analysis == "all_vax_td"){
      input_test$patient_id <- seq.int(nrow(input_test))  # reset patient id as they are not unique in this case
    }
    
    
    train_cox_model <-rms::cph(formula= as.formula(surv_formula),
                             data= input_train, weight=input_train$weight,surv = TRUE,x=TRUE,y=TRUE)
    
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
    
    # predictors_wide=as.data.frame(transpose(data.frame(train_cox_model$coefficients)))
    # names(predictors_wide) = paste0(pred_name_level,".coeff")
    
    for(j in pred_name_level){
      print(j)
      row.index = which(pred_name_level ==j)
      input_test_select[,j] = input_test_select[,j]*predictors$train_cox_model.coefficients[row.index]
    }
    cov_cols <- names(input_test_select)[grep("cov", names(input_test_select))]
    
    # Here the linear predictor is calculated for the testing data, using the linear combination of covariates
    input_test_select <- input_test_select %>% mutate(lin_pred = rowSums(.[ , cov_cols])) %>%
                    dplyr::select(c(patient_id,lin_pred))
    
    ## left join: keep all observations in input_select
    input_test <- merge(x = input_test_select, y = input_test, by = "patient_id", all.x = TRUE)
    # Cox model for testing data
    test_cox_model <- cph(Surv(lcovid_surv,lcovid_cens)~lin_pred,data = input_test, method="breslow")
    
    # Calculate the C-statistic for the discrimination of the model in the validation dataset
    # Harrell's C-statistic 
    pm_val$c_stat[i] = round(concordance(test_cox_model)$concordance,3)
    pm_val$c_stat_lower[i] = round(concordance(test_cox_model)$concordance - 1.96*sqrt((concordance(test_cox_model))$var),3)
    pm_val$c_stat_upper[i] = round(concordance(test_cox_model)$concordance + 1.96*sqrt((concordance(test_cox_model))$var),3)
    
    print("Calculation for the C-statistic is completed!")
    # Calibration slope
    pm_val$cal_slope[i] = round(test_cox_model$coef,3)
    
    # Calibration plot for the validation data
    # Calculate predicted survival probability at 1 year
    time_point = 365
    y1_cox <- summary(survfit(train_cox_model),time=time_point)$surv
    y1_cox
    
    pred_surv_prob = y1_cox^exp(input_test_select$lin_pred)
    
    val_ests <- val.surv(est.surv = pred_surv_prob,
                         S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
                         u=time_point,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))
    
    svg(paste0("output/val_cal_plot_",region[i],"_", analysis, ".svg"))
    plot(val_ests,xlab="Expected Survival Probability",ylab="Observed Survival Probability") 
    groupkm(pred_surv_prob, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
            g=10,u=time_point, pl=T, add=T,lty=0,cex.subtitle=FALSE)
    legend(0.0,0.8,c("Risk groups","Reference line","95% CI"),lty=c(0,2,1),pch=c(19,NA,NA),bty="n")
    dev.off()
    print("Calibration plot is created successfully!")
    # Recalibration of the baseline survival function
    recal_mod <- coxph(Surv(input_test$lcovid_surv,input_test$lcovid_cens)~offset(input_test_select$lin_pred))
    y_recal_1y <- summary(survfit(recal_mod),time=time_point)$surv
    y_recal_1y
    
    # Calculate new predicted probabilities at 1 year (linear predictor stays the same but needs centering)
    pred_surv_prob2=y_recal_1y^exp(input_test_select$lin_pred-mean(input_test_select$lin_pred))
    mean(pred_surv_prob2)
    sd(pred_surv_prob2)
    
    # Redo calibration plot
    val_ests2 <- val.surv(est.surv = pred_surv_prob2,
                          S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
                          u=time_point,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))
    
    svg(paste0("output/val_re_cal_plot_",region[i],"_", analysis, ".svg"))
    plot(val_ests2,xlab="Expected Survival Probability",ylab="Observed Survival Probability") 
    groupkm(pred_surv_prob2, S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
            g=10,u=time_point, pl=T, add=T,lty=0,cex.subtitle=FALSE)
    legend(0.0,0.9,c("Risk groups","Reference line","95% CI"),lty=c(0,2,1),pch=c(19,NA,NA),bty="n")
    dev.off()
    print("Re-calibration plot is created successfully!")
}

write.csv(pm_val, file = paste0("output/val_performance_measures_", analysis,".csv"), row.names = F)
