# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: internal-external validation:
#          1. prognostic model developed based on the whole data set: selected covariates
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