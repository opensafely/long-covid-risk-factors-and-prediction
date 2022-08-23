# Programmed by Elsie Horne

library(dplyr); library(tidyverse); library(readr)

fs::dir_create(here::here("output", "review", "descriptives"))

source("analysis/functions/redactor2.R")

args <- commandArgs(trailingOnly=TRUE)

# Specify cohort 
if(length(args)==0){
  #cohort <- "all"          # all eligible population
  #cohort <- "vaccinated"    # vaccinated population
  cohort <- "infected"      # infected population
}else{
  cohort <- args[[1]]
}
# Read in data
input <- read_rds(paste0("output/input_stage1_", cohort, ".rds"))

# remove covariates
covariate_names <- names(input)[grepl("cov_", names(input))]
variables_to_keep <-names(input)[!names(input)%in%(covariate_names)]
input <- input[,variables_to_keep]

# Q: no need to censor by index date and follow-up end as this just describe the sequence?

# transform data to long format and calculate sequence of events
input_long_1 <- input %>% 
  as_tibble() %>%
 dplyr::select(patient_id, out_covid_date, vax_covid_date2, out_first_long_covid_date) %>%
 pivot_longer(
    cols = -patient_id
    ) %>%
  arrange(patient_id, name) %>%
  group_by(patient_id) %>%
  # when calculating rank, NAs stay NA, and ties take the min
  mutate(sequence = rank(value, na.last = "keep", ties.method = "min")) %>%
  ungroup() %>%
  dplyr::select(-value) %>%
  mutate(across(name, 
                ~case_when(.x %in% "out_covid_date" ~ "covid",
                           .x %in% "vax_covid_date2" ~ "vax",
                           .x %in% "out_first_long_covid_date" ~ "long",
                           TRUE ~ NA_character_))) 

# transform to wide with sequence number for each event and 
# number of observations with each of the given sequences
input_wide_1 <- input_long_1 %>%
  pivot_wider(
    names_from = name,
    values_from = sequence
  ) %>%
  group_by(covid, vax, long) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  # order, starting with most common sequence
  mutate(order = row_number()) 

# clean names
table_sequence <- input_wide_1 %>%
  pivot_longer(
    cols = c(covid, vax, long)
  ) %>%
  group_by(order, value) %>%
  mutate(count = n()) %>%
  mutate(newname = if_else(
    is.na(value),
    # prefix "!" for missing events
    str_c(str_c("!", name), collapse = " & "),
    # "==" for events on same date
    str_c(name, collapse = "==")
  )) %>%
  ungroup() %>%
  dplyr::select(-name) %>%
  distinct() %>%
  # arrange by order
  arrange(order, value) %>%
  mutate(na_flag = is.na(value)) %>%
  group_by(order, na_flag) %>%
  mutate(
    count2 = n(),
    newname2 = if_else(
      count2 > 1,
      # collapse separated by "<" in order
      str_c("(", str_c(newname, collapse = " < "), ")"),
      newname
    ) 
  )  %>%
  ungroup() %>%
  distinct(n, order, newname2, na_flag) %>%
  arrange(order, desc(na_flag)) %>%
  group_by(order) %>%
  mutate(newname3 = str_c(newname2, collapse = " & ")) %>%
  ungroup() %>%
  distinct(newname3, n) %>%
  dplyr::select(sequence = newname3, n) %>%
  # redact smallest values until total redacted >5 using redactor2 function
  mutate(across(n, redactor2)) %>%
  mutate(across(n, ~if_else(is.na(.x), "[redacted]", scales::comma(.x, accuracy = 1))))
    
# highlight the sequences you're interested in
table_sequence<- table_sequence %>%
  mutate(
    seq_label = case_when(
      sequence == "(covid < vax < long)" ~ 1,
      sequence == "(covid < long < vax)" ~ 2,
      sequence == "(vax < covid < long)" ~ 3,
      sequence == "!long & (covid < vax)" ~ 4,
      sequence == "!long & (vax < covid)" ~ 5,
      sequence == "!vax & !long & covid" ~ 6,
      TRUE ~ NA_real_
    )
  )

# output sequence count table
write.csv(table_sequence,
          file=paste0("output/review/descriptives/table_sequence_", cohort,".csv"),
          row.names=F)

