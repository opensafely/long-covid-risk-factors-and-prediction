# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Supplementary figure 1. Pie chart of snomed code for long COVID
# Output:  suppl_table_1.csv

# function for small number suppression
source("analysis/functions/redactor2.R")

library(readr); library(dplyr); library(ggplot2)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

# keep only observations where long covid indicator is 1
input <- input %>% filter(lcovid_i == 1)

# computational efficiency: only keep the needed variable
input <- input %>% select("out_first_long_covid_code")

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

count_data$labels = percent_function(count_data$percent)

count_data$count <- redactor2(count_data$count)
index = which(is.na(count_data$count))
col_names <- c("count","percent","labels")
count_data[index,col_names]= NA

# use redactor for small number suppression
index <- which(count_data$count < 6)
count_data$count[index] = count_data$percent[index] = count_data$labels[index] = NA

count_data_active = count_data%>%filter(count>5)
# Pie Chart
suppl_figure_pie <- ggplot(count_data_active, aes(x = "", y = count, fill = snomed_code)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(x = "", y = "", fill = "SNOMED Code") + 
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(hjust = 0.5, face="bold", size = 10), 
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) 

suppl_figure_pie

#supplementary figure 1
ggsave(file="output/suppl_figure_pie.svg", plot=suppl_figure_pie, width=16, height=8)

# output underlying count data for supplementary figure 1

# small number suppression - indicate NA as redacted
count_data[which(is.na(count_data$count)),col_names]="[redacted]" 

write.csv(count_data, file="output/suppl_table_1.csv")

rmarkdown::render("analysis/compiled_snomed_count.Rmd",
                  output_file="suppl_table_1",output_dir="output")
 
