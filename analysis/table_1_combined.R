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

index <- which(is.na(table_1_com$subgroup_level))
table_1_com$subgroup_level[index] <- table_1_com$variable[index]

table_1_wide <- reshape(table_1_com, idvar = c("variable","subgroup_level"), timevar = "analysis", direction = "wide")

# according to the definition
index <- which(table_1_wide$variable == "sub_cat_covid_phenotype" &
                 table_1_wide$subgroup_level == "no_infection")
table_1_wide$number.infected[index] = 0 # change from NA to 0 because no infection have been excluded for infected population
index <- which(table_1_wide$variable == "sub_cat_covid_history" & 
                 (table_1_wide$subgroup_level == "Missing" |
                    table_1_wide$subgroup_level == "FALSE"))
table_1_wide$number.infected[index] = 0 # change from NA to 0 because prior covid history have been excluded for infected population
table_1_wide$percent.infected[index] = 0 # as above
# That is fine. I think you should also correct percent.infected which remain 100% while the associated number has been changed to 0.
# Response: percent.infected[index] is now also set to 0

table_1_wide$diff_all_vax = abs(table_1_wide$number.all-table_1_wide$number.vaccinated)
table_1_wide$diff_all_infected = abs(table_1_wide$number.all-table_1_wide$number.infected)

# It doesn't look like abs() is needed here, on lines 40-41. 
# Maybe you added abs() in case the number.all is smaller then either number.vax or number.inf?
# Response: abs() is needed because only positive values are allowed in the redactor2 function

# table_1_wide <- table_1_wide %>% filter(subgroup_level!="FALSE") %>%
#   filter(variable != "cov_cat_covid_phenotype")

table_1_wide <- table_1_wide %>%
  filter(variable != "cov_cat_covid_phenotype")

print("table_1_wide created successfully!")

variables <- unique(table_1_wide$variable)

for(i in variables){
  print(i)
  index = which(table_1_wide$variable == i)
  
  index2 <- which(!is.na(table_1_wide$diff_all_vax[index]))
  table_1_wide$diff_all_vax[index[index2]] = redactor2(table_1_wide$diff_all_vax[index[index2]])

  index3 <- which(!is.na(table_1_wide$diff_all_infected[index]))
  table_1_wide$diff_all_infected[index[index3]] = redactor2(table_1_wide$diff_all_infected[index[index3]])
}

index <- which(is.na(table_1_wide$diff_all_vax))
table_1_wide[index,3:17] = "[redacted]"

index <- which(is.na(table_1_wide$diff_all_infected))
table_1_wide[index,3:17] = "[redacted]"

# Because infection is NA for sub_cat_covid_history = TRUE, it gets redacted for the whole row.
# I'm not sure this should be redacted for sub_cat_covid_history = TRUE. 
# Expect for the infection cohort, other numbers seemed ok for all and vax cohorts for that category.
# Maybe this infection NA for sub_cat_covid_history = TRUE needs to be taken into account when applying redaction.

##Response: patients with sub_cat_covid_history = TRUE have now all be excluded from the analysis
##          and so this category has nown been removed

# make table 1 a single file
write.csv(table_1_wide, file="output/review/descriptives/table_1_combined.csv", row.names = F)

# output help file for table 1
CSV_file = "output/review/descriptives/table_1_combined.csv"
rmarkdown::render("analysis/compilation/compiled_table.Rmd",
                  output_file="table_1_combined",output_dir="output/review/descriptives")

print("Table 1 saved successfully!")
