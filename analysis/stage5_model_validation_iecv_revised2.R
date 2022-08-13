# Purpose: Long COVID risk factors and prediction models
# Programmed by:  Yinghui Wei
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

library(rms); library(fastDummies); library(pseudo)

print("Starting stage_5_model_validation.R")

#load data, with defined weight, and import formula for survival analysis
source("analysis/stage3_model_selection.R")
source("analysis/functions/function_int_ext_cross_validation.R")

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

for(i in 1:length(region)){
  print(i)
  pm_val[i,1] = region[i]
  input_train <- input %>% filter(sub_cat_region != region[i])
  fit_cox_model <-rms::cph(formula= as.formula(surv_formula), data= input_train, 
                           weight=input_train$weight,surv = TRUE,x=TRUE,y=TRUE)
  # Calculate the C-statistic for the discrimination of the model in the validation dataset
  # Harrell's C-statistic for C_k: data from region k, k = i
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
