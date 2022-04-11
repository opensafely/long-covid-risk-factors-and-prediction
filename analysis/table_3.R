# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Table 3: Sequence count for 1.COVID-VAX-Long COVID; 
#                                      2.COVID-Long COVID -VAX; 
#                                      3.VAX-COVID-Long COVID; 
#                                      4.COVID-VAX;
#                                      5.VAX-COVID
#                                      6.VAX-No COVID
# Output:  table_3.csv, table_3.html

library(readr); library(dplyr)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

# construct table_3
table_3 <- as.data.frame(matrix(nrow=6, ncol=3))

names(table_3) <- c("sequence_no","sequence","count")

table_3$sequence_no <- 1:6
table_3$sequence <- c("COVID-VAX-Long COVID", "COVID-Long COVID-VAX", "VAX-COVID-Long COVID",
                      "COVID-VAX","VAX-COVID", "VAX-No COVID")

# sequence 1: COVID-VAX-Long COVID
table_3$count[1] <- length(which(input$out_covid_date > input$vax_covid_date1 & 
                        input$out_covid_date  < input$out_first_long_covid_date &
                        input$vax_covid_date1 < input$out_first_long_covid_date &
                        !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & !is.na(input$out_first_long_covid_date)))

# sequence 2: COVID-Long COVID -VAX
table_3$count[2] <-length(which(input$out_covid_date < input$vax_covid_date1 & 
                       input$out_covid_date  < input$out_first_long_covid_date &
                       input$vax_covid_date1 > input$out_first_long_covid_date &
                       !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & !is.na(input$out_first_long_covid_date)))

# sequence 3: VAX-COVID-Long COVID
table_3$count[3] <- length(which(input$out_covid_date > input$vax_covid_date1 & 
                       input$out_covid_date  < input$out_first_long_covid_date &
                       input$vax_covid_date1 < input$out_first_long_covid_date &
                       !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & !is.na(input$out_first_long_covid_date)))

# sequence 4: COVID-VAX without long COVID
table_3$count[4] <- length(which(input$out_covid_date > input$vax_covid_date1 & 
                       !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))


# sequence 5: VAX-COVID without long COVID
table_3$count[5] <-length(which(input$out_covid_date < input$vax_covid_date1 & 
                        !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))

# sequence 6: COVID, no VAX and no long COVID
table_3$count[6] <-length(which(!is.na(input$out_covid_date) & is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))


# small number suppression
table_3$count[which(table_3$count <=5)] = "redacted"

#table_3

write.csv(table_3,"output/table_3.csv",row.names=F)

rmarkdown::render("analysis/compiled_table3_results.Rmd",
                  output_file="table_3",output_dir="output")

