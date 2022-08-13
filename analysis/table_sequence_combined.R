# Purpose: to combine all table_sequence into one table
library(readr); library(dplyr)
fs::dir_create(here::here("output", "review", "descriptives"))

print("Starting to combine table_sequence files")

# function for small number suppression
source("analysis/functions/redactor2.R")

table_seq_all <-read.csv("output/review/descriptives/table_sequence_all.csv")
table_seq_infected <-read.csv("output/review/descriptives/table_sequence_infected.csv")
table_seq_vaccinated <-read.csv("output/review/descriptives/table_sequence_vaccinated.csv")
#View(table_seq_all)

table_seq_all$analysis <- "all"
table_seq_infected$analysis <- "infected"
table_seq_vaccinated$analysis <- "vaccinated"

print("Tables for sequence count read in successfully!")

## make single data set for output

table_seq_com <- rbind(table_seq_all, table_seq_vaccinated, table_seq_infected)

table_seq_wide <- reshape(table_seq_com, idvar = "sequence",
                          timevar = "analysis", direction = "wide")

print("table_seq_wide created successfully!")
table_seq_wide <- table_seq_wide %>% select(sequence, contains("n."), seq_label.all) %>%
  rename(seq_label = seq_label.all) %>%
  mutate(numeric.all = as.numeric(gsub(",", "", n.all))) %>%
  mutate(numeric.vaccinated = as.numeric(gsub(",", "", n.vaccinated))) %>%
  mutate(numeric.infected = as.numeric(gsub(",", "", n.infected)))


print("table_seq_wide re-organised successfully!")

table_seq_wide$diff_all_vaccinated = abs(table_seq_wide$numeric.all-table_seq_wide$numeric.vaccinated)
table_seq_wide$diff_all_infected = abs(table_seq_wide$numeric.all-table_seq_wide$numeric.infected)

## Apply redaction
index <- which(!is.na(table_seq_wide$diff_all_vaccinated))
table_seq_wide$diff_all_vaccinated[index] = redactor2(table_seq_wide$diff_all_vaccinated[index])

index <- which(!is.na(table_seq_wide$diff_all_infected))
table_seq_wide$diff_all_infected[index] = redactor2(table_seq_wide$diff_all_infected[index])

# If difference is redacted as NA but none of the originals are NA, redact the original
index1 <- which(is.na(table_seq_wide$diff_all_vaccinated)&(!is.na(table_seq_wide$numeric.all) & 
                                                            !is.na(table_seq_wide$numeric.vaccinated)))

index2 <- which(is.na(table_seq_wide$diff_all_infected)&(!is.na(table_seq_wide$numeric.all) &
                                                            !is.na(table_seq_wide$numeric.infected)))
index1 <- c(1,2,3) # for testing
index2 <- c(3,4,5) # for testing
index <- c(unique(c(index1, index2)))
table_seq_wide$n.all[index] = table_seq_wide$n.vaccinated[index]= table_seq_wide$n.infected[index] = "[redacted]"

table_seq_wide <- table_seq_wide %>% select(!contains("numeric")) 
print("table_seq_wide redacted successfully!")

write.csv(table_seq_wide, file="output/review/descriptives/table_sequence_combined.csv", row.names=F)