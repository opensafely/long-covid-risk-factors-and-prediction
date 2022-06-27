# Purpose: to combine all table 1 into one table
library(readr); library(dplyr)
fs::dir_create(here::here("output", "review", "descriptives"))

print("Starting to combine table 1 files")

# function for small number suppression
source("analysis/functions/redactor2.R")

table_1_all.csv <-read.csv("output/review/descriptives/table_1_all.csv")
table_1_infected.csv <-read.csv("output/review/descriptives/table_1_infected.csv")
table_1_vaccinated.csv <-read.csv("output/review/descriptives/table_1_vaccinated.csv")

table_1_all.csv$analysis <- "all"
table_1_infected.csv$analysis <- "infected"
table_1_vaccinated.csv$analysis <- "vaccinated"

print("Table 1 files read in successfully!")

## make single data set for output

table_1_com <- rbind(table_1_all.csv, table_1_vaccinated.csv, table_1_infected.csv)

table_1_wide <- reshape(table_1_com, idvar = c("variable","subgroup_level"), timevar = "analysis", direction = "wide")

table_1_wide$diff_all_vax = abs(table_1_wide$number.all-table_1_wide$number.vaccinated)
table_1_wide$diff_all_infected = abs(table_1_wide$number.all-table_1_wide$number.infected)

table_1_wide <- table_1_wide %>% filter(subgroup_level!="FALSE")

print("table_1_wide created successfully!")

index <- which(!is.na(table_1_wide$diff_all_vax))
print("Part 1")
print(index)
table_1_wide$diff_all_vax[index] = redactor2(table_1_wide$diff_all_vax[index])

index <- which(!is.na(table_1_wide$diff_all_infected))
print("Part 2")
print(index)
table_1_wide$diff_all_infected[index] = redactor2(table_1_wide$diff_all_infected[index])

# make table 1 a single file
write.csv(table_1_wide, file="output/review/descriptives/table_1_combined.csv", row.names = F)

# output help file for table 1
CSV_file = "output/review/descriptives/table_1_combined.csv"
rmarkdown::render("analysis/compilation/compiled_table.Rmd",
                  output_file="table_1_combined",output_dir="output/review/descriptives")

print("Table 1 saved successfully!")
