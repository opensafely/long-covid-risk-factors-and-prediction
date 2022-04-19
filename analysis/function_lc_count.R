# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: define functions for long covid counts: monthly and weekly, respectively
# Output:  defined functions


calculate_long_covid_monthly_cases<-function(input){
  input$year <- format(input$out_first_long_covid_date, format = "%Y")
  input$month <- month(ymd(input$out_first_long_covid_date))
  keep <- c("year", "month", "out_first_long_covid_date")
  input <- input[,keep]
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
  #---small number suppression------------------
  data$count[which(data$count <=5)] = NA
  return(data)
}


calculate_long_covid_weekly_cases<-function(input){
  
  input$year <- year(ymd(input$out_first_long_covid_date))
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
  
  data_weekly <- data_weekly[,2:ncol(data_weekly)]
  
  # #---small number suppression------------------
  data_weekly$count[which(data_weekly$count <=5)] = NA
  return(data_weekly)
}

