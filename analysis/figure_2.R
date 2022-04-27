# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Figure 2. time from covid to long covid
# Output:  

library(readr); library(dplyr); library(ggplot2)

# function for small number suppression
source("analysis/functions/redactor2.R")


# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1_all.rds")

input <- input %>% select(out_first_long_covid_date, out_covid_date) %>%
  mutate(days_covid_to_long = ifelse(out_first_long_covid_date > out_covid_date & !is.na(out_first_long_covid_date) & !is.na(out_covid_date),
                                     out_first_long_covid_date - out_covid_date,
                                     ifelse((is.na(out_covid_date)|out_covid_date > out_first_long_covid_date)
                                            & !is.na(out_first_long_covid_date), 28, NA)))
  

summary(input$days_covid_to_long)

results <- NULL
results$min[1] = min(input$days_covid_to_long, na.rm=T)
results$max[1] = max(input$days_covid_to_long, na.rm=T)
results$Q1[1] = quantile(input$days_covid_to_long, probs = 0.25, na.rm = T)
results$Q3[1] = quantile(input$days_covid_to_long, probs = 0.75, na.rm = T)
results$median[1] = quantile(input$days_covid_to_long, probs = 0.5, na.rm = T)
results$mean[1] = mean(input$days_covid_to_long, na.rm=T)
results$sd[1] = sd(input$days_covid_to_long, na.rm=T)

write.csv(data.frame(results), file="output/summary_days_c_to_long.csv", row.names = F)

# hist(input$days_covid_to_long, main = "",
#      xlab = "Days from COVID infection to long COVID diagnosis",
#      ylab = "Count")
# 

figure_hist<-ggplot(input, aes(x=days_covid_to_long)) + 
  geom_histogram(color="black", fill="white") +
  labs(title="",x="Days from COVID infection to long COVID diagnosis", y = "Count")+
  theme_classic()
figure_hist

ggsave(file="output/figure_hist.svg", 
       plot=figure_hist, width=16, height=8)
