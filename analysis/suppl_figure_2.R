# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Supplementary figure 2. Monthly long COVID cases by region
# Output:  suppl_figure_2.svg

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate); library(htmlTable);library(ggplot2)

source("analysis/function_lc_count.R")


#View(input)

#############################################
#Part 1. Monthly long covid count by region #
#############################################

# calculate_long_covid_monthly_cases<-function(input){
#   # Create a data frame ---------------------------------------------------
#   data<- data.frame(matrix(nrow=16, ncol=4))  # 16 months
#   colnames(data) <- c("year", "month", "year_month", "count")
#   data$year <- c(2020, rep(2021, 12), rep(2022,3))
#   data$month <-c(12, seq(1:12), seq(1:3))
#   
#   data$year_month <- seq(as.Date("2020/12/01"), as.Date("2022/03/01"), by = "month")
#   data$year_month <- as.Date(data$year_month, format = "%Y-%M")
#   
#   # Calculate the number of long COVID cases by month -----------------------------
#   
#   for(i in 1:nrow(data)){
#     data$count[i]<-length(which(input$year == data$year[i] & input$month == data$month[i]))
#   }
#   #---small number suppression------------------
#   data$count[which(data$count <=5)] = NA
#   return(data)
# }

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

# keep only observations where long covid indicator is 1
input <- input %>% filter(lcovid_i_vax_c == 1)

# computational efficiency: only keep the needed variable
input <- input %>% select(c("out_first_long_covid_date", "cov_cat_region"))


region <- names(table(input$cov_cat_region))
# Create an empty data frame ---------------------------------------------------
table_lc_monthly_count <- data.frame(year = numeric(),
                                     month  = numeric(),
                                     year_month = Date(),
                                     count    = numeric(), 
                                     region = character())
for(i in region){
  input_region <- input%>%filter(cov_cat_region == i)
  #table_lc_monthly_count[,1:4] <- calculate_long_covid_monthly_cases(input_region)
  start = nrow(table_lc_monthly_count)+1
  end = nrow(table_lc_monthly_count) + 16
  table_lc_monthly_count[start:end, 1:4] <- calculate_long_covid_monthly_cases(input_region)
  table_lc_monthly_count[start:end,5] = rep(i, 16)
}

index = which(table_lc_monthly_count$count <6)
table_lc_monthly_count$count[index] = NA

# Multiple time series in one plot  --------------------------------------------
ymax = max(table_lc_monthly_count$count)
suppl_figure_2 <- ggplot(table_lc_monthly_count,
                         aes(x=year_month,y=count,colour=region,group=region)) + 
  geom_point(size = 1.5)+ geom_line() +
  scale_x_date(breaks = seq.Date(from = ymd("2020/12/01"), # Specify limits by hand
                                 to = ymd("2022/03/01"),
                                 by = "months"), 
               date_labels = "%b-%y", 
               minor_breaks = NULL,
               limits = ymd("2020-12-01", "2022-03-01"))+ # Specify limits by hand
  lims(y = c(0, ymax)) + 
  #geom_hline(yintercept=6, linetype="dashed", color = "red") +
  #
  xlab(label='\nDates')+
  #
  ylab(label='Monthly New Long COVID Cases\n')+
  #
  # Specify a theme
  #
  theme_bw() +
  #
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right")


# Output underlying count data for supplementary figure 2 ----------------------

#---small number suppression ---------------------------------------------------
index = which(is.na(table_lc_monthly_count$count))
table_lc_monthly_count$count[index] = "redacted"

write.csv(table_lc_monthly_count, file="output/long_covid_count_monthly_by_region.csv")

time_interval = "monthly"
region = "by_region"
rmarkdown::render("analysis/compiled_long_covid_count.Rmd",
                  output_file="long_covid_count_monthly_by_region",output_dir="output")
# rmarkdown::render("analysis/compiled_suppl_fig2_data.Rmd", 
#                   output_file="suppl_figure_2_data",output_dir="output")


# Output supplementary figure 2
ggsave(file=paste0("output/suppl_figure_2_monthly_by_region", ".svg"), plot=suppl_figure_2, width=16, height=8)



############################################
#Part 2. weekly long covid count by region #
############################################

# calculate_long_covid_weekly_cases<-function(input){
#   # Create a data frame ---------------------------------------------------
#   input$week <- week(ymd(input$out_first_long_covid_date))
#   
#   diff_in_weeks = difftime(ymd("2022/03/31"), ymd("2020/12/01"), units = "weeks") # weeks
#   no_weeks = round(diff_in_weeks,0)+1
#   
#   data_weekly<- data.frame(matrix(NA, nrow=no_weeks))  # 70 weeks
#   
#   
#   data_weekly$year <- c(rep(2020,52-48+1), rep(2021, 52), rep(2022,13))
#   data_weekly$week <-c(48:52, seq(1:52), seq(1:13))
#   
#   data_weekly$year_week <- seq(as.Date("2020/12/01"), as.Date("2022/03/31"), by = "week")
#   data_weekly$year_week <- as.Date(data_weekly$year_week, format = "%Y-%M")
#   
#   #View(data_weekly)
#   # Calculate the number of long COVID cases by month -----------------------------
#   
#   for(i in 1:nrow(data_weekly)){
#     data_weekly$count[i]<-length(which(input$year == data_weekly$year[i] & 
#                                          input$week == data_weekly$week[i]))
#   }
#   
#   data_weekly <- data_weekly[,2:5]
#   
#   #---small number suppression------------------
#   data_weekly$count[which(data_weekly$count <=5)] = NA
# }


# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

# keep only observations where long covid indicator is 1
input <- input %>% filter(lcovid_i_vax_c == 1)

# computational efficiency: only keep the needed variable
input <- input %>% select(c("out_first_long_covid_date", "cov_cat_region"))
region <- names(table(input$cov_cat_region))
# Create an empty data frame ---------------------------------------------------
table_lc_weekly_count <- data.frame(year = numeric(),
                                     week  = numeric(),
                                     year_weekly = Date(),
                                     count    = numeric(), 
                                     region = character())
for(i in region){
  print(i)
  input_region <- input%>%filter(cov_cat_region == i)
  #table_lc_monthly_count[,1:4] <- calculate_long_covid_monthly_cases(input_region)
  start = nrow(table_lc_weekly_count)+1
  end = nrow(table_lc_weekly_count) + 70
  table_lc_weekly_count[start:end, 1:4] <- calculate_long_covid_weekly_cases(input_region)
  table_lc_weekly_count[start:end,5] = rep(i, 70)
}


# Multiple time series in one plot  --------------------------------------------
ymax = max(table_lc_weekly_count$count)
suppl_figure_2_weekly <- ggplot(table_lc_weekly_count,aes(x=year_month,y=count,colour=region,group=region)) + 
  geom_point(size = 1.5)+ geom_line() +
  scale_x_date(breaks = seq.Date(from = ymd("2020/12/01"), # Specify limits by hand
                                 to = ymd("2022/03/01"),
                                 by = "months"), 
               date_labels = "%b-%y", 
               minor_breaks = NULL,
               limits = ymd("2020-12-01", "2022-03-01"))+ # Specify limits by hand
  lims(y = c(0, ymax)) + 
  #geom_hline(yintercept=6, linetype="dashed", color = "red") +
  #
  xlab(label='\nDates')+
  #
  ylab(label='Monthly New Long COVID Cases\n')+
  #
  # Specify a theme
  #
  theme_bw() +
  #
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right")

#---small number suppression ---------------------------------------------------
index = which(is.na(table_lc_weekly_count$count))
table_lc_weekly_count$count[index] = "redacted"

write.csv(table_lc_weekly_count, file="output/long_covid_count_weekly_by_region.csv")

time_interval = "weekly"
region = "by_region"
rmarkdown::render("analysis/compiled_long_covid_count.Rmd",
                  output_file="long_covid_count_weekly_by_region",output_dir="output")

# rmarkdown::render("analysis/compiled_suppl_fig2_data.Rmd", 
#                   output_file="suppl_figure_2_weekly_long_covid_count_by_region",output_dir="output")
# 

# Output supplementary figure 2
ggsave(file=paste0("output/suppl_figure_2_weekly", ".svg"), plot=suppl_figure_2, width=16, height=8)

