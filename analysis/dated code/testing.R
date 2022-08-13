# data check
input_select <- input %>% select(out_post_viral_fatigue, out_first_post_viral_fatigue_date)
input_select2 <- input %>% select(cov_cat_post_viral_fatigue, first_post_viral_fatigue_date)
sum(!is.na(input_select2$first_post_viral_fatigue_date))
table(input_select2$cov_cat_post_viral_fatigue)
input_select3 <- input %>% select(out_long_covid,out_first_long_covid_date)
View(input_select3)
