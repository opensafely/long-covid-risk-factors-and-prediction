# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Supplementary figure 2. Monthly long COVID cases by region
# Output:  suppl_figure_2.svg

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate); library(htmlTable);library(ggplot2)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

input$year <- format(input$out_first_long_covid_date, format = "%Y")
input$month <- month(ymd(input$out_first_long_covid_date))

keep <- c("year", "month", "out_first_long_covid_date", "cov_cat_region")
input <- input[,keep]

#View(input)

calculate_long_covid_count<-function(input){
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
  return(data)
}

region <- names(table(input$cov_cat_region))
# Create an empty data frame ---------------------------------------------------
table_long_covid_count <- data.frame(year = numeric(),
                                     month  = numeric(),
                                     year_month = Date(),
                                     count    = numeric(), 
                                     region = character())
for(i in region){
  input_region <- input%>%filter(cov_cat_region == i)
  start = nrow(table_long_covid_count)+1
  end = nrow(table_long_covid_count) + 16
  table_long_covid_count[start:end, 1:4] <- calculate_long_covid_count(input_region)
  table_long_covid_count[start:end,5] = rep(i, 16)
}

index = which(table_long_covid_count$count <6)
table_long_covid_count$count[index] = NA

# Multiple time series in one plot  --------------------------------------------
ymax = max(table_long_covid_count$count)
suppl_figure_2 <- ggplot(table_long_covid_count,aes(x=year_month,y=count,colour=region,group=region)) + 
  geom_point(size = 1.5)+ geom_line() +
  scale_x_date(breaks = seq.Date(from = ymd("2020/12/01"), # Specify limits by hand
                                 to = ymd("2022/03/01"),
                                 by = "months"), 
               date_labels = "%b-%y", 
               minor_breaks = NULL,
               limits = ymd("2020-12-01", "2022-03-01"))+ # Specify limits by hand
  lims(y = c(0, ymax)) +
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
        legend.position = "right")


# Output underlying count data for supplementary figure 2 ----------------------
index = which(is.na(table_long_covid_count$count))
table_long_covid_count$count[index] = "redacted"

write.csv(table_long_covid_count, file="output/suppl_figure_2_data.csv")
htmlTable(table_long_covid_count, file="output/suppl_figure_2_data.html")

# Outpuf supplementary figure 2

ggsave(file=paste0("output/suppl_figure_2", ".svg"), plot=suppl_figure_2, width=16, height=8)