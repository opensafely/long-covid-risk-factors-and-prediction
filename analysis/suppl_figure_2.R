# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Supplementary figure 2. Monthly long COVID cases by region
# Output:  suppl_figure_1_*.svg,long_covid_count_*_by_region.csv, 
#          and long_covid_count_*_by_region.html
#          for monthly and weekly counts by region

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate); library(htmlTable);library(ggplot2)

## Load functions to calculate long covid cases

source("analysis/function_long_covid_count.R")

#############################################
#Part 1. Monthly long covid count by region #
#############################################

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

# keep only observations where long covid indicator is 1
input <- input %>% filter(lcovid_i_vax_c == 1)

# computational efficiency: only keep the needed variable
input <- input %>% select(c("out_first_long_covid_date", "cov_cat_region"))

region <- names(table(input$cov_cat_region))

# Create an empty data frame ---------------------------------------------------
table_lc_monthly_count <- data.frame(year   = numeric(),
                                     month  = numeric(),
                                     year_month = Date(),
                                     count  = numeric(), 
                                     region = character())
for(i in region){
  input_region <- input%>%filter(cov_cat_region == i)
  start = nrow(table_lc_monthly_count)+1
  end = nrow(table_lc_monthly_count) + 16
  table_lc_monthly_count[start:end, 1:4] <- calculate_long_covid_monthly_cases(input_region)
  table_lc_monthly_count[start:end,5] = rep(i, 16)
}

#---small number suppression ---------------------------------------------------
table_lc_monthly_count$count[which(table_lc_monthly_count$count <=5)] = NA 

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

#---small number suppression ---------------------------------------------------
table_lc_monthly_count$count[which(table_lc_monthly_count$count <=5)] = "[redacted]"

# Output underlying count data for supplementary figure 2 ----------------------
write.csv(table_lc_monthly_count, file="output/long_covid_count_monthly_by_region.csv")

time_interval = "monthly"
region = "by_region"
rmarkdown::render("analysis/compiled_long_covid_count.Rmd",
                  output_file="long_covid_count_monthly_by_region",
                  output_dir="output")

# Output supplementary figure 2
ggsave(file=paste0("output/suppl_figure_2_monthly_by_region", ".svg"), 
       plot=suppl_figure_2, width=16, height=8)


############################################
#Part 2. weekly long covid count by region #
############################################

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
  start = nrow(table_lc_weekly_count)+1
  end = nrow(table_lc_weekly_count) + 70
  table_lc_weekly_count[start:end, 1:4] <- calculate_long_covid_weekly_cases(input_region)
  table_lc_weekly_count[start:end,5] = rep(i, 70)
}

# Multiple time series in one plot  --------------------------------------------
ymax = max(table_lc_weekly_count$count)
suppl_figure_2_weekly <- ggplot(table_lc_weekly_count,
                                aes(x=year_month,y=count, colour=region,group=region)) + 
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
table_lc_weekly_count$count[index] = "[redacted]"

# Output underlying count data for supplementary figure 2 ----------------------
write.csv(table_lc_weekly_count, file="output/long_covid_count_weekly_by_region.csv")

time_interval = "weekly"
region = "by_region"
rmarkdown::render("analysis/compiled_long_covid_count.Rmd",
                  output_file="long_covid_count_weekly_by_region",output_dir="output")

# Output supplementary figure 2-------------------------------------------------
ggsave(file=paste0("output/suppl_figure_2_weekly", ".svg"), 
       plot=suppl_figure_2, width=16, height=8)

