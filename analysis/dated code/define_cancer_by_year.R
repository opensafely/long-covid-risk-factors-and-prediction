################################################################################
## Part 2.0 define cancer                                                      #
################################################################################
# input <- input %>% mutate(cov_cat_cancer 
#                           = ifelse(pre_cov_lung_cancer_date <= index_date - 365*5 | pre_cov_other_cancer_date <= index_date - 365*5,
#                                    "Cancer 5 years ago", 
#                                    ifelse(((pre_cov_lung_cancer_date <= index_date - 365) & 
#                                           (pre_cov_lung_cancer_date > index_date - 365*5)) |
#                                           ((pre_cov_other_cancer_date <= index_date - 365) & 
#                                            (pre_cov_other_cancer_date > index_date - 365*5)),
#                                                   "Cancer 2 to 5 years ago", 
#                                           ifelse((pre_cov_lung_cancer_date > index_date - 365) &
#                                                   (pre_cov_lung_cancer_date < as.Date(index_date))|
#                                                   ((pre_cov_other_cancer_date > index_date - 365) &
#                                                   (pre_cov_other_cancer_date < as.Date(index_date))) ,
#                                                  "cancer last year", "Never"))))
# a <- input %>% select(pre_cov_lung_cancer_date, cov_cat_cancer)
# View(a)
################################################################################
## Part 2.0 define haematological malignancy                                   #
################################################################################

################################################################################
## Part 2.0 define reduced kidney function                                     #
################################################################################

################################################################################
## Part 2.0 define asthma                                                      #
################################################################################