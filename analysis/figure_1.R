# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Figure 1. Number of long COVID diagnosis over time
# Output:  figure1.svg

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate); library(htmlTable)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

input$year <- format(input$out_first_long_covid_date, format = "%Y")
input$month <- month(ymd(input$out_first_long_covid_date))

keep <- c("year", "month", "out_first_long_covid_date")
input <- input[,keep]


# Create a data frame ---------------------------------------------------
data<- data.frame(matrix(nrow=16, ncol=4))  # 16 months
colnames(data) <- c("year", "month", "year_month", "count")
data$year <- c(2020, rep(2021, 12), rep(2022,3))
data$month <-c(12, seq(1:12), seq(1:3))




length(which(input$year == 2021 & input$month == 1))
for(i in 1:nrow(data)){
  data$year_month[i] <- paste0(data$year[i], "-", data$month[i])
  data$count[i]<-length(which(input$year == data$year[i] & input$month == data$month[i]))
}

plot(data$count, ylab="New long COVID cases", xlab="Date")
