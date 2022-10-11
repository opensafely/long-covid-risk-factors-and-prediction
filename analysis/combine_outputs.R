
# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine estimates and performance measures across the analyses for easier review
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('here')
library('glue')

fs::dir_create(here("output", "review", "model", "combined"))

file_prefix <- c("HR", "AIC", "PM")
all_files <- lapply(
  file_prefix,
  function(x)
    list.files(path = here::here("output", "review", "model"), 
               pattern = glue("{x}_.+.csv"),
               all.files = FALSE,
               full.names = FALSE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE)
)
names(all_files) <- file_prefix

# print all_files to log
print(all_files)

# read all files and add filename as column   
combine_outputs <- function(prefix) {
  bind_rows(
    lapply(all_files[[prefix]], 
           function(x)
             readr::read_csv(
               here("output", "review", "model", x),
               progress = FALSE
             ) %>%
             mutate(file = str_remove_all(x, glue("^{prefix}\\_|\\.csv")))
    )
  )
}

for ( p in file_prefix) {
  cat(glue("{p}:"), "\\n")
  cat("combine outputs\\n")
  out <- combine_outputs(p) 
  cat("save combined outputs\\n")
  readr::write_csv(
    out,
    here("output", "review", "model", "combined", glue("{p}_combined.csv"))
  )
}
