# Purpose: Long covid coding
# Author:  Yinghui Wei
# Content: Figure: histogram for days from covid to long covid
# Output:  one CSV table and one SVG figure

library(readr); library(dplyr); library(ggplot2)

fs::dir_create(here::here("output", "review", "descriptives"))

index_date=as.Date("2020-12-01")
pandemic_start = as.Date("2020-01-29")
cohort_end = as.Date("2022-03-31")
# cohort_end - pandemic_start  # maximum days

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1_all.rds")

# We can only calculate days from covid to long covid if the long covid diagnosis date is not NA
# It is possible that for people with long covid diagnosis there is no record of their covid infection date
input <- input%>% dplyr::select(out_first_long_covid_date, out_covid_date) %>%
                  filter(((out_covid_date>= pandemic_start & out_covid_date <= cohort_end)|
                           is.na(out_covid_date))&
                          ((out_first_long_covid_date >= pandemic_start & 
                             out_first_long_covid_date <= cohort_end)))

# If long covid date is recorded, but covid date is not recorded, input days from c to long c as 91 days = 13 weeks
# Long COVID is defined as long lasting symptoms post infection, usually longer than 12 weeks. 
# Assuming one week delay, we impute missing days from c to long c as 13 weeks.
input <- input %>% mutate(days_covid_to_long = ifelse(out_first_long_covid_date > out_covid_date & 
                                       !is.na(out_first_long_covid_date) & 
                                       !is.na(out_covid_date),
                                     out_first_long_covid_date - out_covid_date,                         
                                     ifelse((is.na(out_covid_date)|out_covid_date > out_first_long_covid_date)
                                            & !is.na(out_first_long_covid_date), 91, NA)))

days <- sort(input$days_covid_to_long, decreasing = F)
# truncation - make the ten smallest days = the 11th smallest day, to avoid potential disclosure issues
days[1:10]<- head(days,11)[11] # the 11th smallest number
# truncation - make the ten largest days = the 11th largest day, to avid potential disclosure issues
days[(length(days)-9):length(days)] = head(tail(days, 11),1) # the 11th largest number

results <- NULL
results$min[1] = min(days, na.rm=T)
results$max[1] = max(days, na.rm=T)

results$Q1[1] = quantile(days, probs = 0.25, na.rm = T)
results$Q3[1] = quantile(days, probs = 0.75, na.rm = T)
results$median[1] = quantile(days, probs = 0.5, na.rm = T)
results$mean[1] = mean(days, na.rm=T)
results$sd[1] = sd(days, na.rm=T)

results <- data.frame(results)
results <- round(results, 3)
write.csv(results, file="output/review/descriptives/summary_days_c_to_long.csv", row.names = F)

figure_hist<-ggplot(as.data.frame(days), aes(x=days)) +
  geom_histogram(color="black", fill="gray", bins = 15) +
  scale_x_continuous(breaks = seq(0, 800, by = 100)) +
  labs(title="",x="Days from COVID infection to long COVID diagnosis", y = "Count") +
  theme_classic() +  
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.1,"inches"),type = "closed")))
figure_hist

bin_count <- ggplot_build(figure_hist)$data[[1]]$count
bin_count <- data.frame(seq(1:length(bin_count)), bin_count)
names(bin_count) <- c("bin", "count")
ggsave(file="output/review/descriptives/figure_hist.svg", 
       plot=figure_hist, width=16, height=8)

write.csv(bin_count, file="output/review/descriptives/hist_bin_count.csv", row.names = F)
outfile="hist_bin_count"
rmarkdown::render("analysis/compilation/compiled_hist_bin_count.Rmd", 
                  output_file=outfile,output_dir="output/review/descriptives")
