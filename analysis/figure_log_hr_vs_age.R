## Programmed by Yinghui Wei
## Content: plot of long hazard ratio against continuous age

library(readr); library(dplyr); library(rms); library(MASS)
fs::dir_create(here::here("output", "review", "model"))

source("analysis/stage2_model_input_set_up.R")

# Load data
hr_est <- read.csv(paste0("output/review/model/HR_full_model_age_spline_", analysis,".csv"))
hr_est <- hr_est %>% filter(term == "cov_num_age" |  term == "cov_num_age'")

# set age reference
age_ref <- 55

# compute restricted cubic splines
c1 <- rms::rcs(input$cov_num_age,parms=knot_placement)
c1 <- as.vector(c1[,2])
cm <- rms::rcs(age_ref,parms=knot_placement)
cm <- as.vector(cm[,2])

b <- input$cov_num_age-age_ref

# extract log HR
est_a1 <- log(hr_est$hazard_ratio[1]) # loghr for age
est_a2 <- log(hr_est$hazard_ratio[2]) # loghr for the second basis function

# calculate log hazard ratio by age
log_hr_age <- est_a1*b+est_a2*(c1-cm)
df<-data.frame(input$cov_num_age,log_hr_age)

p = ggplot(df)+aes(input$cov_num_age,log_hr_age)+geom_line()+
  labs( x = '\nAge in years', y = 'Log Hazard ratio compared to age 55 years\n')

p <- p +    scale_x_continuous(breaks = seq(20, 100, by = 20)) +
            theme(legend.text=element_text(size=20),
            axis.text = element_text(size =20),
            axis.title = element_text(size =20),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))

ggsave(file=paste0("output/review/model/figure_loghr_age_",analysis,".svg"), 
       plot=p, width=15, height=10)
