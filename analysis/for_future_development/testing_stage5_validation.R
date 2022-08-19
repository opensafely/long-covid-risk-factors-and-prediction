# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Test validation code using a simple script for post-viral fatigue

library(readr); library(dplyr); library(rms); library(MASS); library(pseudo);library(fastDummies)
# library(survcomp) ## not yet available
fs::dir_create(here::here("output", "not_for_review", "model"))
fs::dir_create(here::here("output", "review", "model"))
source("analysis/functions/function_cox_output.R")
source("analysis/functions/function_df_summary.R")
source("analysis/functions/function_int_ext_cross_validation.R")
analysis = "fatigue_all"
ratio_non_cases_to_cases = 20 # this is used in sampling non-cases to increase efficiency without loss of information
set.seed(123456) # to ensure reproducibility in the sampling

analysis = "age_sex_fatigue_model"
################################################################################
# Part 1: load data, define inverse probability weighting                      #
################################################################################
input <- read_rds("output/input_stage1_all.rds")

##--specify inverse probability weighting
cases <- input %>% filter(fatigue_cens==1)

non_cases <- input %>% filter(!patient_id %in% cases$patient_id)

if(nrow(cases)*ratio_non_cases_to_cases < nrow(non_cases)){
  non_cases <- non_cases[sample(1:nrow(non_cases), nrow(cases)*ratio_non_cases_to_cases,replace=FALSE), ]
}else if (nrow(cases)*ratio_non_cases_to_cases >= nrow(non_cases)){
  non_cases=non_cases
}
print(paste0("Number of cases: ", nrow(cases)))
print(paste0("Number of controls: ", nrow(non_cases)))
non_case_inverse_weight=(nrow(input)-nrow(cases))/nrow(non_cases)
## recreate input after sampling
input <- bind_rows(cases,non_cases)
## Add inverse probability weights for non-cases
noncase_ids <- unique(non_cases$patient_id)
input$weight <-1
input$weight <- ifelse(input$patient_id %in% noncase_ids,
                       non_case_inverse_weight, 1)

## remove region as a covariate
input <- rename(input, sub_cat_region = "cov_cat_region")

## extract candidate predictors
covariate_names <- names(input)[grep("cov_", names(input))]

## remove categorical and continuous age, as continuous age will be added
## later as with a spline function
covariate_names <- covariate_names[-grep("age", covariate_names)]

print("candidate predictors")
print(covariate_names)
## for computational efficiency, only keep the variables needed in fitting the model
variables_to_keep <- c("patient_id", "practice_id",
                       "fatigue_surv", "fatigue_cens", covariate_names,
                       "cov_num_age", "weight", "sub_cat_region")


input <- input %>% dplyr::select(all_of(variables_to_keep))
print("Part 1: load data, define inverse probability weighting is completed!")

################################################################################
# Part 2: number of people in each covariate level                             #
################################################################################
function_df_summary(input, analysis)
## set up before using rms::cph
dd <<- datadist(input) #
options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment")) #

################################################################################
# Part 3: define survival analysis formula                                     #
################################################################################
## linear predictors + a restricted cubic spline for age + 
##  a restricted cubic spline for gp consultation rate + clustering effect for practice
knot_placement=as.numeric(quantile(input$cov_num_age, probs=c(0.1,0.5,0.9)))

## Age sex model
surv_formula_age_spl_sex <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  "cov_cat_sex",
  "+rms::rcs(cov_num_age,parms=knot_placement)"
)

## Age sex model
surv_formula_age_linear_sex <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  "cov_cat_sex", "+ cov_num_age"
)

## Full model
surv_formula <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+rms::rcs(cov_num_age,parms=knot_placement)"
)

## full model: age is added as a linear predictor
surv_formula_lp <- paste0(
  "Surv(fatigue_surv, fatigue_cens) ~ ",
  paste(covariate_names, collapse = "+"),
  "+ cov_num_age"
)

print("Part 3: define survival analysis formula is completed!")

print("End of stage3_model_input_setup.R")

################################################################################
# Part 4: Fit Cox model with post-viral fatigue as an outcome                  #
################################################################################
# age sex model age spline

fit_age_sex_cox_model_splines <-rms::cph(formula= as.formula(surv_formula_age_spl_sex),
                                         data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

# age sex model age linear
fit_age_sex_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_age_linear_sex),
                                        data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

# full model age spline
fit_full_cox_model_splines <-rms::cph(formula= as.formula(surv_formula),
                                      data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)

# test the simplest model
surv_formula = surv_formula_age_linear_sex
which_model = "age_linear_sex"
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
  test_cox_model <- cph(Surv(fatigue_surv,fatigue_cens)~lin_pred,data = input_test, method="breslow")
  
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

  print("val_ests is now specified!")
  
  ### Start: Adapt from SR's code for calibration plot ----------------------------------------------
  ###Calibration plot with pseudos
  #Calculate pseudo values using pseudo package
  pseudovalues<- pseudosurv(input_test$fatigue_surv, input_test$fatigue_cens, tmax = 365)
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

for(i in 1:length(region)){
  print(i)
  pm_val[i,1] = region[i]
  input_train <- input %>% filter(sub_cat_region != region[i])
  fit_cox_model <-rms::cph(formula= as.formula(surv_formula), data= input_train, 
                           weight=input_train$weight,surv = TRUE,x=TRUE,y=TRUE)
  # Calculate C-statistic for the discrimination of the model in the validation dataset
  # Harrell's C-statistic for C_k: data from region k, k=i
  c_stat_without_k = round(concordance(fit_cox_model)$concordance,4)
  c_stat_lower_without_k = round(concordance(fit_cox_model)$concordance - 1.96*sqrt((concordance(fit_cox_model))$var),4)
  c_stat_upper_without_k = round(concordance(fit_cox_model)$concordance + 1.96*sqrt((concordance(fit_cox_model))$var),4)
  c_stat_var_without_k = round((concordance(fit_cox_model))$var,6)
  temp = try(function_int_ext_cross_validation(input,region[i], fit_cox_model))
  pm_val[i,4:7] <- c(c_stat_without_k, c_stat_var_without_k, c_stat_lower_without_k, c_stat_upper_without_k)
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
write.csv(pm_val, file = paste0("output/review/model/iecv_performance_measures_", analysis,".csv"), row.names = F)

outfile = "iecv_performance_measures_"
rmarkdown::render(paste0("analysis/compilation/compiled_val_pm_table",".Rmd"), 
                  output_file=paste0("iecv_performance_measures_", analysis),
                  output_dir="output/review/model")
