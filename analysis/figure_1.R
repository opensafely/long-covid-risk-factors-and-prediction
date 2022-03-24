# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Figure 1. Number of long COVID diagnosis over time
# Output:  figure_1.svg

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate); library(htmlTable);library(ggplot2)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

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

# Caculate the number of long COVID cases by month -----------------------------

for(i in 1:nrow(data)){
  data$count[i]<-length(which(input$year == data$year[i] & input$month == data$month[i]))
}

#plot(data$year_month, data$count, ylab="New long COVID cases", xlab="Date")

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
  ylab(label='New Long COVID Cases\n')+
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

#figure_1
ggsave(file="output/figure_1.svg", plot=figure_1, width=16, height=8)


