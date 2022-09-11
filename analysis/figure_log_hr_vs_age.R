## Programmed by Yinghui Wei
## Content: plot of long hazard ratio against continuous age

library(readr); library(dplyr); library(rms); library(MASS)
fs::dir_create(here::here("output", "review", "model"))

source("analysis/stage2_model_input_set_up.R")

input <- input %>% filter(input$cov_num_age<=100)

# Load data
hr_est <- read.csv(paste0("output/review/model/HR_full_model_age_spline_", analysis,".csv"))
hr_est <- hr_est %>% filter(term == "cov_num_age" |  term == "cov_num_age'")

# set age reference
age_ref <- 55

# output supporting document
write.csv(table(input$cov_num_age),file=paste0("output/review/model/support_loghr_vs_age_table_age_", analysis, ".csv"))

# compute restricted cubic splines
c1 <- rms::rcs(input$cov_num_age,parms=knot_placement)
c1 <- as.vector(c1[,2])
cm <- rms::rcs(age_ref,parms=knot_placement)
cm <- as.vector(cm[,2])

b <- input$cov_num_age-age_ref

# extract log HR
est_a1 <- log(hr_est$hazard_ratio[1]) # loghr for age
est_a2 <- log(hr_est$hazard_ratio[2]) # loghr for the second basis function

# se for log HR
se_logHR1 <- (est_a1 - log(hr_est$robust.conf.low[1]))/1.96
se_logHR2 <- (est_a2 - log(hr_est$robust.conf.low[2]))/1.96
var_est_a1 <- se_logHR1^2
var_est_a2 <- se_logHR2^2
# se_logHR1 <-log(hr_est$robust.se[1]) 
# se_logHR1 <-log(hr_est$robust.se[2]) 

# calculate log hazard ratio by age
log_hr_age <- est_a1*b+est_a2*(c1-cm)

var_log_hr_age <- var_est_a1*b^2 + var_est_a2*(c1-cm)^2
se_log_hr_age <- sqrt(var_log_hr_age)

log_hr_age_low <- log_hr_age - 1.96 * se_log_hr_age
log_hr_age_high <- log_hr_age + 1.96 * se_log_hr_age

df<-data.frame(input$cov_num_age,log_hr_age, log_hr_age_low, log_hr_age_high)

# output underlying data
write.csv(df, file=paste0("output/review/model/table_loghr_age_", 
                                 analysis, ".csv"), row.names = F)

p = ggplot(df)+aes(input$cov_num_age,log_hr_age)+geom_line()+ 
  geom_hline(aes(yintercept = 0), linetype = 3)

p <- p +    scale_x_continuous(breaks = seq(20, 100, by = 20))  +
  theme(axis.text = element_text(size =20),
        axis.title = element_text(size =20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  labs( x = '\nAge in years', y = 'Log hazard ratio compared to age 55 years\n')

ggsave(file=paste0("output/review/model/figure_loghr_age_",analysis,".svg"), 
       plot=p, width=10, height=7)


p <- p + geom_ribbon(aes(ymin = log_hr_age_low, ymax = log_hr_age_high), alpha = 0.1) +
  labs( x = '\nAge in years', y = 'Log hazard ratio compared to age 55 years\n')

ggsave(file=paste0("output/review/model/figure_loghr_age_ci_",analysis,".svg"), 
       plot=p, width=10, height=7)

p <- p + geom_ribbon(aes(ymin = log_hr_age_low, ymax = log_hr_age_high), alpha = 0.1) +
  labs( x = 'Age in years', y = 'Log hazard ratio compared to age 55 years')


ggsave(file=paste0("output/review/model/figure_loghr_age_ci_",analysis,".png"), 
       plot=p, width=10, height=7)
