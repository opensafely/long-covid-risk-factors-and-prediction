# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Supplementary figure 1. Pie chart of snomed code for long COVID
# Output:  suppl_figure_1.svg

library(readr); library(dplyr); library("arrow"); library("data.table"); 
library(lubridate); library(htmlTable); library(ggplot2)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

snomed_code <- input$out_first_long_covid_code


count_data <-table(snomed_code)

count_data <- data.frame(count_data)
names(count_data) <- c("snomed_code", "count")
count_data
count_data$percent = round(count_data$count / sum(count_data$count),3)
count_data
percent_function <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
#library(scales)

#label_percent()(x)
#count_data$labels = label_percent()(count_data$percent)
count_data$labels = percent_function(count_data$percent)
suppl_figure1 <- ggplot(count_data, aes(x = "", y = percent, fill = snomed_code)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.ticks = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) 
 suppl_figure1

#supplementary figure 1
ggsave(file="output/suppl_figure_1.svg", plot=suppl_figure1, width=16, height=8)




