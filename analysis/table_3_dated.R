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
#                                      11. checking: COVID is not na but long COVID is na
#                                      12. validate long covid count
#                                      13. checking: COVID na but long COVID is not na
#                                      14. checking: COVID na and long COVID na
#                                      15.long COVID !na and COVID !na
#                                      16. number of observations
#                                      17. validate number of observations
# Output:  table_3.csv, table_3.html

library(readr); library(dplyr)

start.time = Sys.time()

## Read in data and identify factor variables and numerical variables------------
input <- read_rds("output/input_stage1.rds")

## construct table_3
table_3 <- as.data.frame(matrix(nrow=17, ncol=2))

names(table_3) <- c("sequence","count")

##table_3$sequence_no <- 1:17
table_3$sequence <- c("1. COVID-VAX-Long COVID",
                      "2. COVID-Long COVID -VAX",
                      "3. VAX-COVID-Long COVID",
                      "4. COVID-VAX",
                      "5. VAX-COVID",
                      "6. COVID != na & VAX == na & long COVID == na",
                      "7. long COVID-COVID",
                      "8. same day COVID and long COVID",
                      "9. COVID-long COVID",
                      "10. COVID == na but long COVID != na",
                      "11. long COVID != na",
                      "12. validate long covid count (sum sequence count 7:10)", 
                      "13. COVID != na & long COVID == na",
                      "14. COVID na and long COVID na",
                      "15. long COVID !na and COVID !na",
                      "16. number of observations",
                      "17. validate number of observations (sum sequence count: 10, 13-15)")

## sequence 1: COVID-VAX-Long COVID
table_3$count[1] <- length(which(input$out_covid_date < input$vax_covid_date1 & 
                        input$out_covid_date  < input$out_first_long_covid_date &
                        input$vax_covid_date1 < input$out_first_long_covid_date &
                        !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & !is.na(input$out_first_long_covid_date)))

## sequence 2: COVID-Long COVID -VAX
table_3$count[2] <-length(which(input$out_covid_date < input$vax_covid_date1 & 
                       input$out_covid_date  < input$out_first_long_covid_date &
                       input$vax_covid_date1 > input$out_first_long_covid_date &
                       !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & !is.na(input$out_first_long_covid_date)))

## sequence 3: VAX-COVID-Long COVID
table_3$count[3] <- length(which(input$out_covid_date > input$vax_covid_date1 & 
                       input$out_covid_date  < input$out_first_long_covid_date &
                       input$vax_covid_date1 < input$out_first_long_covid_date &
                       !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & !is.na(input$out_first_long_covid_date)))

## sequence 4: COVID-VAX without long COVID
table_3$count[4] <- length(which(input$out_covid_date < input$vax_covid_date1 & 
                       !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))


## sequence 5: VAX-COVID without long COVID
table_3$count[5] <-length(which(input$out_covid_date > input$vax_covid_date1 & 
                        !is.na(input$out_covid_date) & !is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))

## sequence 6: COVID, no VAX and no long COVID
table_3$count[6] <-length(which(!is.na(input$out_covid_date) & is.na(input$vax_covid_date1) & is.na(input$out_first_long_covid_date)))

## 7. checking: long COVID - COVID
table_3$count[7] <-length(which(input$out_covid_date  > input$out_first_long_covid_date &
                                  !is.na(input$out_covid_date) & !is.na(input$out_first_long_covid_date)))

## 8. checking: same day COVID and long COVID
table_3$count[8] <-length(which(input$out_covid_date  == input$out_first_long_covid_date &
                                  !is.na(input$out_covid_date) & !is.na(input$out_first_long_covid_date)))

## 9. checking: COVID-long COVID
table_3$count[9] <-length(which(input$out_covid_date  < input$out_first_long_covid_date &
                                   !is.na(input$out_covid_date) & !is.na(input$out_first_long_covid_date)))


## 10. checking: COVID na but long COVID is !na

table_3$count[10] <-length(which(is.na(input$out_covid_date) &
                                 !is.na(input$out_first_long_covid_date)))

## 11. checking: long COVID
table_3$count[11] <-length(which(!is.na(input$out_first_long_covid_date)))

#table_3

total_long_covid = sum(table_3$count[7:10])

## 12. validate long covid count
table_3$count[12] = total_long_covid

## 13. checking: long COVID na but COVID !na
table_3$count[13] <-length(which(!is.na(input$out_covid_date) &
                                   is.na(input$out_first_long_covid_date)))

## 14. checking: long COVID na and COVID is na
table_3$count[14] <-length(which(is.na(input$out_covid_date) &
                                   is.na(input$out_first_long_covid_date)))


## 15. checking: long COVID !na and COVID !na
table_3$count[15] <-length(which(!is.na(input$out_covid_date) &
                                   !is.na(input$out_first_long_covid_date)))

## 16. output: number of observations
table_3$count[16] <-nrow(input)

## 17. validate: number of observations
table_3$count[17] <- table_3$count[10] + sum(table_3$count[13:15])

## small number suppression

index = which(table_3$count<=5)

## to avoid backward calculation for disclosure
for(i in index){
  if(i %in% c(7,8,9,10)){
    table_3$count[12] = table_3$count[12] - table_3$count[i]
  }
  if(i %in% c(10,13,14,15)){
    table_3$count[17] = table_3$count[17] - table_3$count[i]
  }
}

table_3$count[which(table_3$count <=5)] = "[redacted]"

write.csv(table_3,"output/table_3.csv",row.names=F)

csv_file ="table_3"
rmarkdown::render("analysis/compiled_table3_results.Rmd", output_file="table_3",output_dir="output")

end.time = Sys.time()

run.time = end.time - start.time

run.time
