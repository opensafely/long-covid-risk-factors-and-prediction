# Purpose: to combine all table 2 into one table
library(readr); library(dplyr)
fs::dir_create(here::here("output", "review", "descriptives"))

print("Starting to combine table 2 files")

# function for small number suppression
source("analysis/functions/redactor2.R")

table_2_all.csv <-read.csv("output/review/descriptives/table_2_all.csv")
table_2_all_vax_c.csv <-read.csv("output/review/descriptives/table_2_all_vax_c.csv")
table_2_infected.csv <-read.csv("output/review/descriptives/table_2_infected.csv")
table_2_vaccinated.csv <-read.csv("output/review/descriptives/table_2_vaccinated.csv")

table_2_all.csv$analysis <- "all"
table_2_all_vax_c.csv$analysis <- "all_vax_c"
table_2_infected.csv$analysis <- "infected"
table_2_vaccinated.csv$analysis <- "vaccinated"

print("Table 2 files read in successfully!")

## make single data set for output

table_2_com <- rbind(table_2_all.csv, table_2_all_vax_c.csv, table_2_vaccinated.csv, table_2_infected.csv)

table_2_wide <- reshape(table_2_com, idvar = c("outcome","subgrp","subgrp_level"), timevar = "analysis", direction = "wide")

print("table_2_wide created successfully!")

temp <- table_2_wide %>% select(contains("event_count"))
table_2_wide$diff_all_vax = abs(table_2_wide$event_count.all-table_2_wide$event_count.all_vax_c)
table_2_wide$diff_all_vaccinated = abs(table_2_wide$event_count.all-table_2_wide$event_count.vaccinated)
table_2_wide$diff_all_infected = abs(table_2_wide$event_count.all-table_2_wide$event_count.infected)

variables <- unique(table_2_wide$subgrp)

for(i in variables){
  print(i)
  index = which(table_2_wide$variable == i)#RK - I don't think $variable exists in table 2 wide? Should it be subgrp?
  
  index2 <- which(!is.na(table_2_wide$diff_all_vax[index]))
  table_2_wide$diff_all_vax[index2] = redactor2(table_2_wide$diff_all_vax[index2])
  
  index3 <- which(!is.na(table_2_wide$diff_all_infected[index]))
  table_2_wide$diff_all_infected[index3] = redactor2(table_2_wide$diff_all_infected[index3])
  
  index4 <- which(!is.na(table_2_wide$diff_all_vaccinated[index]))
  table_2_wide$diff_all_vaccinated[index4] = redactor2(table_2_wide$diff_all_infected[index4])#RK redactor function has infected rather than vaccinated
}

#RK - in the diff columns there are still some counts of 0,1,2 etc- what are these? Do these differences matter?
# Does anything else need to be redacted?

index <- which(is.na(table_2_wide$diff_all_vax))
# although person-years do not need to be redacted, I would still do so for now
table_2_wide[index,4:26] = "[redacted]"   
index <- which(is.na(table_2_wide$diff_all_vaccinated))
table_2_wide[index,4:26] = "[redacted]"

index<-which(is.na(table_2_wide$diff_all_infected))
table_2_wide[index,4:26] = "[redacted]"

# make table 2 a single file
write.csv(table_2_wide, file="output/review/descriptives/table_2_combined.csv", row.names = F)

# output help file for table 2
CSV_file = "output/review/descriptives/table_2_combined.csv"
rmarkdown::render("analysis/compilation/compiled_table.Rmd",
                  output_file="table_2_combined",output_dir="output/review/descriptives")

print("Table 2 saved successfully!")