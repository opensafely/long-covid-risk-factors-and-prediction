# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Figure 1. Number of long COVID diagnosis over time
# Output:  figure_1.svg

library(readr); library(dplyr); library(arrow); library(data.table) 
library(lubridate); library(htmlTable);library(ggplot2)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

# keep only observations where long covid indicator is 1
input <- input %>% filter(lcovid_i_vax_c == 1)

# computational efficiency: only keep the needed variable
input <- input %>% select("out_first_long_covid_date")

# monthly count

input$year <- format(input$out_first_long_covid_date, format = "%Y")
input$month <- month(ymd(input$out_first_long_covid_date))

keep <- c("year", "month", "out_first_long_covid_date")
input <- input[,keep]

#View(input)
# Create a data frame ---------------------------------------------------
data<- data.frame(matrix(nrow=16, ncol=4))  # 16 months
colnames(data) <- c("year", "month", "year_month", "count")
data$year <- c(2020, rep(2021, 12), rep(2022,3))
data$month <-c(12, seq(1:12), seq(1:3))

data$year_month <- seq(as.Date("2020/12/01"), as.Date("2022/03/01"), by = "month")
data$year_month <- as.Date(data$year_month, format = "%Y-%M")

# Calculate the number of long COVID cases by month -----------------------------

for(i in 1:nrow(data)){
  data$count[i]<-length(which(input$year == data$year[i] & input$month == data$month[i]))
}

# Work out the limit for y axis ------------------------------------------------
count_min <- min(data$count)
count_max <- max(data$count)

y_min = count_min - count_min%%10
y_max = count_max - count_max%%10 + 10
interval = round((y_max - y_min)/10,0)


# Produce Figure 1--------------------------------------------------------------
figure_1 <- ggplot(data, aes(x=year_month, 
                 y=count))+
  # Add the number of new long COVID cases as points
  #
  geom_point(size = 1.5) +
  # Add the number of new long COVID cases as a line
  #
  geom_line()+ 
  #
  geom_hline(yintercept=6, linetype="dashed", color = "red") +
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

# figure_1
ggsave(file="output/figure_1_monthly_count.svg", plot=figure_1, width=16, height=8)

#-------------------------------------------------------------------------------
# weekly long COVID count

input$week <- week(ymd(input$out_first_long_covid_date))

diff_in_weeks = difftime(ymd("2022/03/31"), ymd("2020/12/01"), units = "weeks") # weeks
no_weeks = round(diff_in_weeks,0)+1

data_weekly<- data.frame(matrix(NA, nrow=no_weeks))  # 70 weeks


data_weekly$year <- c(rep(2020,52-48+1), rep(2021, 52), rep(2022,13))
data_weekly$week <-c(48:52, seq(1:52), seq(1:13))

data_weekly$year_week <- seq(as.Date("2020/12/01"), as.Date("2022/03/31"), by = "week")
data_weekly$year_week <- as.Date(data_weekly$year_week, format = "%Y-%M")

#View(data_weekly)
# Calculate the number of long COVID cases by month -----------------------------

for(i in 1:nrow(data_weekly)){
  data_weekly$count[i]<-length(which(input$year == data_weekly$year[i] & 
                                    input$week == data_weekly$week[i]))
}

data_weekly <- data_weekly[,2:5]
data_weekly$count[which(data_weekly$count <=5)] = NA

plot(data_weekly$count)
# Produce Figure 1 Weekly count----------------------------------------------------
figure_1_weekly <- ggplot(data_weekly, aes(x=year_week, 
                             y=count))+
  # Add the number of new long COVID cases as points
  #
  geom_point(size = 1.5) +
  # Add the number of new long COVID cases as a line
  #
  geom_line()+ 
  #
  geom_hline(yintercept=6, linetype="dashed", color = "red") +
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
ggsave(file="output/figure_1_weekly_count.svg", plot=figure_1_weekly, width=16, height=8)

