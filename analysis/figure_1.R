# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Figure 1. Number of long COVID diagnosis over time
# Output:  figure_1_*.svg,long_covid_count_*.csv, and long_covid_count_*.html
#          for monthly and weekly counts

library(readr); library(dplyr); library(arrow); library(data.table) 
library(lubridate); library(htmlTable);library(ggplot2)

## Load functions to calculate long covid cases
source("analysis/function_long_covid_count.R")

# function for small number suppression
source("analysis/functions/redactor2.R")

#############################################
#Part 1. Monthly long covid count           #
#############################################

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1_all.rds")

# keep only observations where long covid indicator is 1
input <- input %>% filter(lcovid_cens == 1)

# computational efficiency: only keep the needed variable
input <- input %>% select("out_first_long_covid_date")


input$year <- format(input$out_first_long_covid_date, format = "%Y")
input$month <- month(ymd(input$out_first_long_covid_date))

keep <- c("year", "month", "out_first_long_covid_date")
input <- input[,keep]

# Create a data frame ----------------------------------------------------------

table_lc_monthly_count <- data.frame(year = numeric(),
                                     month  = numeric(),
                                     year_month = Date(),
                                     count    = numeric()
                                     )

table_lc_monthly_count <- calculate_long_covid_monthly_cases(input)


#---small number suppression ----------------------------------------------------
# use redactor function as it suppresses small numbers until total suppressed is > threshold
table_lc_monthly_count$count <- redactor2(table_lc_monthly_count$count)

# # Work out the limit for y axis ------------------------------------------------
# count_min <- min(data$count)
# count_max <- max(data$count)
# 
# y_min = count_min - count_min%%10
# y_max = count_max - count_max%%10 + 10
# interval = round((y_max - y_min)/10,0)

# Produce Figure 1--------------------------------------------------------------
figure_1 <- ggplot(table_lc_monthly_count, aes(x=year_month, 
                 y=count))+
                # Add the number of new long COVID cases as points
                #
                geom_point(size = 1.5) +
                # Add the number of new long COVID cases as a line
                #
                geom_line()+ 
                #
                #geom_hline(yintercept=6, linetype="dashed", color = "red") +
                #
                scale_x_date(breaks = seq.Date(from = ymd("2020/12/01"), # Specify limits by hand
                                               to = ymd("2022/03/01"),
                                               by = "months"), 
               date_labels = "%b-%y", 
               minor_breaks = NULL,
               limits = ymd("2020-12-01", "2022-03-01")) + # Specify limits by hand
               #
               #(breaks = seq(y_min, y_max, by = interval), 
               #                   minor_breaks = NULL) +
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
                     legend.position = "bottom")

# figure_1 - long covid monthly
ggsave(file="output/figure_1_long_covid_monthly_count.svg", 
       plot=figure_1, width=16, height=8)

#############################################
#Part 2. Weekly long covid count            #
#############################################

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1_all.rds")

# keep only observations where long covid indicator is 1
input <- input %>% filter(lcovid_cens == 1)

# computational efficiency: only keep the needed variable
input <- input %>% select("out_first_long_covid_date")

# create a data frame
table_lc_weekly_count <- data.frame(year = numeric(),
                                    week  = numeric(),
                                    year_weekly = Date(),
                                    count    = numeric()
                                    )

table_lc_weekly_count <- calculate_long_covid_weekly_cases(input)

#---small number suppression ---------------------------------------------------
# use redactor function as it suppresses small numbers until total suppressed is > threshold
table_lc_weekly_count$count <- redactor2(table_lc_weekly_count$count)

# Produce Figure 1 Weekly count-------------------------------------------------
figure_1_weekly <- ggplot(table_lc_weekly_count, aes(x=year_week, 
                             y=count))+
                          # Add the number of new long COVID cases as points
                          #
                          geom_point(size = 1.5) +
                          # Add the number of new long COVID cases as a line
                          #
                          geom_line()+ 
                          #
                          #geom_hline(yintercept=6, linetype="dashed", color = "red") +
                          #
                          scale_x_date(breaks = seq.Date(from = ymd("2020/12/01"), # Specify limits by hand
                                                         to = ymd("2022/03/01"),
                                                         by = "months"), 
                          date_labels = "%b-%y", 
                          minor_breaks = NULL,
                          limits = ymd("2020-12-01", "2022-03-01")) + # Specify limits by hand
                          #
                          xlab(label='\nDates')+
                          #
                          ylab(label='Weekly New Long COVID Cases\n')+
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
                                legend.position = "bottom")

figure_1_weekly

# figure_1_weekly_count
ggsave(file="output/figure_1_long_covid_weekly_count.svg", 
       plot=figure_1_weekly, width=16, height=8)


#-- output monthly count table as additional check ----------------------------
time_interval = "monthly"
region="all"  # all regions
table_lc_monthly_count$count[is.na(table_lc_monthly_count$count)] = "[redacted]"
write.csv(table_lc_monthly_count,file="output/long_covid_count_monthly_all.csv", 
          row.names = F)

rmarkdown::render("analysis/compiled_long_covid_count.Rmd",
                  output_file="long_covid_count_monthly_all",output_dir="output")


#-- output weekly count table as additional check -----------------------------
time_interval = "weekly"
region="all"  # all regions
table_lc_weekly_count$count[is.na(table_lc_weekly_count$count)] = "[redacted]"
write.csv(table_lc_weekly_count,file="output/long_covid_count_weekly_all.csv", 
          row.names = F)
rmarkdown::render("analysis/compiled_long_covid_count.Rmd",
                  output_file="long_covid_count_weekly_all",output_dir="output")
