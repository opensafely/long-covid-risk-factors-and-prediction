# for testing
# region_i = "London"
function_int_ext_cross_validation <- function(input, region_i, fit_cox_model){
  
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
  
  pred_risk = 1 - pred_surv_prob
  
  val_ests <- val.surv(est.surv = pred_surv_prob,
                       S = Surv(input_test$lcovid_surv,input_test$lcovid_cens), 
                       u=time_point,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))
  print("val_ests is now specified!")
  
  ### Start: Adapt from SR's code for calibration plot ----------------------------------------------
  ###Calibration plot with pseudos
  #Calculate pseudo values using pseudo package
  pseudovalues<- pseudosurv(input_test$lcovid_surv, input_test$lcovid_cens, tmax = 365)
  pseudos<- data.frame(pseudo = pseudovalues$pseudo, risk = pred_risk)
  pseudos_sorted<- arrange(pseudos, risk) #Sort by risk
  
  #Smooth pseudo values using weighted local regression (LOESS)
  loess_pseudo<- predict(loess(pseudo ~ risk, data = pseudos_sorted,
                               degree = 1, #Fit polynomial of degree 1 (linear) between groups
                               span = 0.3 #Proportion of closest points to use in fit (span*number of obs)
  ),
  se = T)
  
  #PLOT
  #Setting up
  axislim<- 1
  spike_bounds <- c(-0.05, 0)
  bin_breaks <- seq(0, axislim, length.out = 100 + 1)
  freqs <- table(cut(pseudos_sorted$risk, breaks = bin_breaks))
  bins <- bin_breaks[-1]
  freqs_valid <- freqs[freqs > 0]
  freqs_rescaled <- spike_bounds[1] + (spike_bounds[2] - spike_bounds[1]) * 
    (freqs_valid - min(freqs_valid)) / (max(freqs_valid) - min(freqs_valid))
  
  # save the calibration plot
  svg(paste0("output/review/model/iecv_calibration_plot_",region_i,"_", analysis, ".svg"))
  #Start with a blank plot
  plot(x = pseudos_sorted$risk, y = pseudos_sorted$pseudovalue,
       xlim = c(0, axislim), ylim = c(spike_bounds[1], axislim), #Lower y axis must leave space for histogram
       axes = F, xaxs="i",
       xlab = "Predicted risks", ylab = "Observed outcome proprtion",
       frame.plot = FALSE, type = "n")
  axis(side = 1, at = seq(0, axislim, by=0.1))
  axis(side = 2, at = seq(0, axislim, by=0.1))
  #Add confidence intervals
  polygon(x = c(pseudos_sorted$risk, rev(pseudos_sorted$risk)),
          y = c(pmax(loess_pseudo$fit - qt(p = 0.975, df = loess_pseudo$df) * loess_pseudo$se.fit, 0), #Stop confidence interval dipping below 0
                pmax(rev(loess_pseudo$fit + qt(p = 0.975, df = loess_pseudo$df) * loess_pseudo$se.fit),1)),
          col = "lightgrey")
  #Add diagonal line for reference of perfect calibration
  abline(a=0, b=1, col="red", lty=2)
  #Add line of calibration
  lines(x = pseudos_sorted$risk, y = loess_pseudo$fit,
        col = "black", lwd = 2)
  #Add histogram of predicted risk underneath x axis
  segments(x0 = bins[freqs > 0], x1 = bins[freqs > 0],
           y0 = spike_bounds[1], y1 = freqs_rescaled)
  
  dev.off()
  ### End: Adapt from SR's code for calibration plot ----------------------------------------------
  print("Calibration plot is created successfully!")
  
  print(paste0("---- END Region: ", region_i, " ----"))
  return(c(c_stat, c_stat_var, c_stat_lower, c_stat_upper,  cal_slope))
}
