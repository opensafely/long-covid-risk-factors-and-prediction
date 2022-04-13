# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Reviewer: Elsie Horne
# Content: Table 3: Sequence count for 1.COVID-VAX-Long COVID
#                                      2.COVID-Long COVID -VAX
#                                      3.VAX-COVID-Long COVID
#                                      4.COVID-VAX
#                                      5.VAX-COVID
#                                      6.VAX-No COVID
#                                      7.checking: long COVID
#                                      8.checking: long COVID-COVID
#                                      9.checking: same day COVID and long COVID recording
#                                      10. checking: COVID-long COVID
#                                      11. checking: COVID na but long COVID is not na
#                                      12. checking: COVID is not na but long COVID is na
#                                      13. validate long covid count
# Output:  table_3.csv, table_3.html

library(readr); library(dplyr)

# Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

# construct table_3
table_3 <- as.data.frame(matrix(nrow=13, ncol=3))

names(table_3) <- c("sequence","count")

#table_3$sequence_no <- 1:13
table_3$sequence <- c("1.COVID-VAX-Long COVID",
                      "2.COVID-Long COVID -VAX",
                      "3.VAX-COVID-Long COVID",
                      "4.COVID-VAX",
                      "5.VAX-COVID",
                      "6.VAX-No COVID",
                      "7.checking: long COVID-COVID",
                      "8.checking: same day COVID and long COVID",
                      "9.checking: COVID-long COVID",
                      "10. checking: COVID == na but long COVID != na",
                      "11. checking: COVID != na & long COVID == na",
                      "12.checking: long COVID != na",
                      "13. validate long covid count (sum sequence count 7:10)")

# sequence 1: COVID-VAX-Long COVID
table_3$count[1] <- length(which(input$out_covid_date < input$vax_covid_date1 & 
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
table_3$count[4] <- length(which(input$out_covid_date < input$vax_covid_date1 & 
                       !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))


# sequence 5: VAX-COVID without long COVID
table_3$count[5] <-length(which(input$out_covid_date > input$vax_covid_date1 & 
                        !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))

# sequence 6: COVID, no VAX and no long COVID
table_3$count[6] <-length(which(!is.na(input$out_covid_date) & is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))

# 7. counting: long COVID - COVID
table_3$count[7] <-length(which(input$out_covid_date  > input$out_first_long_covid_date &
                                  !is.na(input$out_covid_date) & !is.na(input$out_first_long_covid_date)))

# 8. counting: same day COVID and long COVID
table_3$count[8] <-length(which(input$out_covid_date  == input$out_first_long_covid_date &
                                  !is.na(input$out_covid_date) & !is.na(input$out_first_long_covid_date)))

# 9. checking: COVID-long COVID
table_3$count[9] <-length(which(input$out_covid_date  < input$out_first_long_covid_date &
                                   !is.na(input$out_covid_date) & !is.na(input$out_first_long_covid_date)))


# 10. checking: COVID na but long COVID is not na

table_3$count[10] <-length(which(is.na(input$out_covid_date) &
                                 !is.na(input$out_first_long_covid_date)))

# 11. checking: long COVID na but COVID is not na
table_3$count[11] <-length(which(!is.na(input$out_covid_date) &
                                   is.na(input$out_first_long_covid_date)))
# 12. checking: long COVID
table_3$count[12] <-length(which(!is.na(input$out_first_long_covid_date)))

# small number suppression
# table_3$count[which(table_3$count <=5)] = "[redacted]"

#table_3

total_long_covid = sum(table_3$count[7:10])

# 13. validate long covid count
table_3$count[13] = total_long_covid


write.csv(table_3,"output/table_3.csv",row.names=F)

rmarkdown::render("analysis/compiled_table3_results.Rmd",
                  output_file="table_3",output_dir="output")

