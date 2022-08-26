library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
#library(dplyr)

###########################
# Load information to use #
###########################

## defaults ----
defaults_list <- list(
  version = "3.0",
  expectations= list(population_size=20000L)
)

analysis <- c("all", "all_vax_c", "vaccinated", "all_vax_td", "infected") 
cohort <- c("all", "vaccinated", "infected")
cohort2 <- c("all", "all_vax_c" , "vaccinated", "infected")
analysis_to_run <- c("all", "all_vax_c", "all_vax_td", "infected") 

# create action functions ----

############################
## generic action function #
############################
action <- function(
  name,
  run,
  dummy_data_file=NULL,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL
){
  outputs <- list(
    highly_sensitive = highly_sensitive,
    moderately_sensitive = moderately_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL
  
  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  action[sapply(action, is.null)] <- NULL
  
  action_list <- list(name = action)
  names(action_list) <- name
  
  action_list
}

## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}

## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}

apply_stage0_data_cleaning <- function(cohort){
  splice(
    comment(glue("Stage 0 Data Cleaning - {cohort}")),
    action(
      name = glue("stage0_data_cleaning_{cohort}"),
      run = "r:latest analysis/stage0_data_cleaning.R",
      arguments = c(cohort),
      needs = list(glue("generate_study_population_{cohort}")),
      highly_sensitive = list(
        cohort = glue("output/input_stage0_{cohort}.rds")
      ),
      moderately_sensitive = list(
        variable_check_table = glue("output/not_for_review/descriptives/table_0_{cohort}.csv"),
        histogram_numerical_variable = glue("output/not_for_review/descriptives/histogram_*_{cohort}.svg"),
        table_gp = glue("output/not_for_review/descriptives/table_gp_*_{cohort}*")
      )
    )
  )
}

apply_table1 <- function(cohort){
  splice(
    comment(glue("Table 1. Patient Characteristics - {cohort}")),
    action(
      name = glue("table_1_{cohort}"),
      run = "r:latest analysis/table_1.R",
      arguments = c(cohort),
      needs = list(glue("stage1_define_eligible_population_{cohort}")),
      moderately_sensitive = list(
        descriptive_table = glue("output/review/descriptives/table_1_{cohort}.csv")
      )
    )
  )
}

apply_table2 <- function(cohort){
  splice(
    comment(glue("Table 2. Event count - {cohort}")),
    action(
      name = glue("table_2_{cohort}"),
      run = "r:latest analysis/table_2.R",
      arguments = c(cohort),
      needs = list(if(cohort == "all_vax_c"){
        glue("stage1_define_eligible_population_all")}else{
          glue("stage1_define_eligible_population_{cohort}")
        }
      ),
      moderately_sensitive = list(
        event_count_table = glue("output/review/descriptives/table_2_{cohort}*")
      )
    )
  )
}
apply_table_sequence <- function(cohort){
  splice(
    comment(glue("Table. Sequence count - {cohort}")),
    action(
      name = glue("table_sequence_{cohort}"),
      run = "r:latest analysis/table_sequence.R",
      arguments = c(cohort),
      needs = list(
          glue("stage1_define_eligible_population_{cohort}")
      ),
      moderately_sensitive = list(
        sequence_count_table = glue("output/review/descriptives/table_sequence_{cohort}*")
      )
    )
  )
}
apply_stage1_eligibility <- function(cohort){
  splice(
    comment(glue("Stage 1 define eligible population - {cohort}")),
    action(
      name = glue("stage1_define_eligible_population_{cohort}"),
      run = "r:latest analysis/stage1_define_eligible_population.R",
      arguments = c(cohort),
      needs = list(glue("stage0_data_cleaning_{cohort}")),
      highly_sensitive = list(
        cohort = glue("output/input_stage1_{cohort}.rds")
      ),
      moderately_sensitive = list(
        table_flow_chart = glue("output/not_for_review/descriptives/flow_chart_{cohort}*"),
        table_gp_interactions = glue("output/not_for_review/descriptives/table_quantile_gp*_{cohort}*")
      )
    )
  )
}
apply_development_cox_model <- function(analysis){
  splice(
    comment(glue("Development Cox Model - {analysis}")),
    action(
      name = list(glue("development_cox_model_{analysis}")),
      run = "r:latest analysis/stage3_model_development.R",
      arguments = c(analysis),
      needs = list(if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
        glue("stage1_define_eligible_population_all")}else{
        glue("stage1_define_eligible_population_{analysis}")
        }
        ),
      moderately_sensitive = list(
        #ph_test_CSV = glue("output/review/model/PH_test_*_{analysis}.csv"),
        supporting_document = glue("output/review/model/analysis_data_summary_{analysis}*"),
        hazard_ratios = glue("output/review/model/HR_*_{analysis}*"),
        selected_variables = glue("output/not_for_review/model/selected_variables_{analysis}.csv"),
        model_selection = glue("output/not_for_review/model/model_selection_{analysis}.csv"),
        AIC = glue("output/review/model/AIC_{analysis}.csv")
      )
    )
  )
}
apply_development_cox_model_age_sex <- function(analysis){
  splice(
    comment(glue("Development Cox Model - {analysis}")),
    action(
      name = list(glue("development_model_age_sex_{analysis}")),
      run = "r:latest analysis/stage3_4_model_dev_eval_age_sex.R",
      arguments = c(analysis),
      needs = list(if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
        glue("stage1_define_eligible_population_all")}else{
          glue("stage1_define_eligible_population_{analysis}")
        }
      ),
      moderately_sensitive = list(
        # fit_cox_model = glue("output/not_for_review/model/fit_cox_model_age_sex_{analysis}.rds"),
        hazard_ratios = glue("output/review/model/HR_age_sex_*_{analysis}*"),
        performance_measure = glue("output/review/model/PM_age_sex_*_{analysis}*"),
        survival_plot = glue("output/not_for_review/model/survival_plot_*_age_sex_*_{analysis}.svg")
      )
    )
  )
}
apply_development_cox_model_age_sex_adjusted <- function(analysis){
  splice(
    comment(glue("Development Cox Model - {analysis}")),
    action(
      name = list(glue("development_model_age_sex_adjusted_{analysis}")),
      run = "r:latest analysis/stage3_4_model_dev_eval_age_sex_adjusted.R",
      arguments = c(analysis),
      needs = list(if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
        glue("stage1_define_eligible_population_all")}else{
          glue("stage1_define_eligible_population_{analysis}")
        }
      ),
      moderately_sensitive = list(
        hazard_ratios = glue("output/review/model/HR_age_sex_adjusted_{analysis}*"),
        performance_measure = glue("output/review/model/PM_age_sex_adjusted_{analysis}*")
      )
    )
  )
}

apply_cox_model_subset_variables <- function(analysis){
  splice(
    comment(glue("Development Cox Model using a subset of variables - {analysis}")),
    action(
      name = list(glue("model_subset_variables_{analysis}")),
      run = "r:latest analysis/stage3_model_subset_variables.R",
      arguments = c(analysis),
      #needs =list("identify_subset_variables"),
      needs = list(glue("identify_subset_variables"),if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
        glue("stage1_define_eligible_population_all")}else{
          glue("stage1_define_eligible_population_{analysis}")
        }
      ),
      moderately_sensitive = list(
        hazard_ratios = glue("output/review/model/HR_subset*_{analysis}*"),
        performance_measure = glue("output/review/model/PM_subset*_{analysis}*")
      )
    )
  )
}
apply_figure_loghr_age <- function(analysis){
  splice(
    comment(glue("Figure. Log hazard ratio against age - {analysis}")),
    action(
      name = glue("figure_loghr_age_{analysis}"),
      run = "r:latest analysis/figure_log_hr_vs_age.R",
      arguments = c(analysis),
      needs = list(glue("development_cox_model_{analysis}"),if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
        glue("stage1_define_eligible_population_all")}else{
          glue("stage1_define_eligible_population_{analysis}")
        }
      ),
      moderately_sensitive = list(
        figure_loghr_age = glue("output/review/model/figure_loghr_age_{analysis}*")
      )
    )
  )
}
apply_evaluation_cox_model <- function(analysis){
  splice(
    action(
      name = glue("evaluation_cox_model_{analysis}"),
      run = "r:latest analysis/stage4_model_evaluation.R",
      arguments = c(analysis),
      needs = list(if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
        glue("stage1_define_eligible_population_all")}else{
          glue("stage1_define_eligible_population_{analysis}")
        }),
      moderately_sensitive = list(
        performance_measure_CSV = glue("output/review/model/PM_*_{analysis}.csv"),
        survival_plot = glue("output/not_for_review/model/survival_plot_*_{analysis}.svg")
        # calibration_plot = glue("output/review/model/calibration_plot_{analysis}.svg"),
        # risk_histogram = glue("output/review/model/risk_histogram_{analysis}.svg"),
        # risk_histogram_bin_count = glue("output/review/descriptives/risk_hist_bin_count_{analysis}*")
      )
    )
  )
}
# apply_validation_cox_model_iecv <- function(analysis){
#   splice(
#     comment(glue("Validation Cox Model - {analysis}")),
#     action(
#       name = glue("validation_cox_model_{analysis}"),
#       run = "r:latest analysis/stage5_model_validation_iecv_revised2.R",
#       arguments = c(analysis),
#       needs = list(if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
#         glue("stage1_define_eligible_population_all")}else{
#           glue("stage1_define_eligible_population_{analysis}")
#         }),
#       moderately_sensitive = list(
#         val_performance_measure_CSV = glue("output/review/model/iecv_performance_measures_{analysis}.csv"),
#         val_cal_plot = glue("output/review/model/iecv_calibration_plot_*_{analysis}.svg") 
#       )
#     )
#   )
# }
##########################################################
## Define and combine all actions into a list of actions #
##########################################################
actions_list <- splice(
  
  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create_project_actions.R",
          "Edit and run create_project_actions.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),
  comment(""),
  comment("Part 1. Generate Study Population"),
  comment(""),
  comment("Generate dummy data for all eligible population"),
  action(
    name = "generate_study_population_all",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_all --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_all.feather")
    )
  ),
  comment("Generate dummy data for study population - vaccinated"),
  action(
    name = "generate_study_population_vaccinated",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_vaccinated --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_vaccinated.feather")
    )
  ),
  comment("Generate dummy data for study population - infected"),
  action(
    name = "generate_study_population_infected",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_infected --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_infected.feather")
    )
  ),
  comment("Part 2. Create Analysis Data Sets"),
  comment("Data Cleaning"),
  splice(
    unlist(lapply(cohort, function(x) apply_stage0_data_cleaning(cohort = x)), recursive = FALSE)
  ),
  comment("Define eligible population"),
  splice(
    unlist(lapply(cohort, function(x) apply_stage1_eligibility(cohort = x)), recursive = FALSE)
  ),
   # Figures and tables
  comment("Part 3. Figures and tables"),
  # Table 1 Patient Characteristics
  splice(
    unlist(lapply(cohort, function(x) apply_table1(cohort = x)), recursive = FALSE)
  ),
  
  comment("Table_1_combined - all table 1 combined"),
  action(
    name = "table_1_combined",
    run = "r:latest analysis/table_1_combined.R",
    needs = list("table_1_all", "table_1_vaccinated", "table_1_infected"),
    moderately_sensitive = list(
      table = glue("output/review/descriptives/table_1_combined*")
    )
  ),
  # Table 2 Event count and incidence rate
  comment("Table 2 event count"),
  splice(
    unlist(lapply(cohort2, function(x) apply_table2(cohort = x)), recursive = FALSE)
  ),
  comment("Table_2_combined - all table 2 combined"),
  action(
    name = "table_2_combined",
    run = "r:latest analysis/table_2_combined.R",
    needs = list("table_2_all", "table_2_all_vax_c", "table_2_vaccinated", "table_2_infected"),
    moderately_sensitive = list(
      table = glue("output/review/descriptives/table_2_combined*")
    )
  ),
  comment("Table - sequence count"),
  splice(
    unlist(lapply(cohort, function(x) apply_table_sequence(cohort = x)), recursive = FALSE)
  ),
  comment("Table sequence combined - all tables for sequence count combined"),
  action(
    name = "table_sequence_combined",
    run = "r:latest analysis/table_sequence_combined.R",
    needs = list("table_sequence_all", "table_sequence_vaccinated", "table_sequence_infected"),
    moderately_sensitive = list(
      table = glue("output/review/descriptives/table_sequence_combined*")
    )
  ),
  comment("Figure - long covid count"),
  action(
    name = "figure_long_covid_count_all",
    run = "r:latest analysis/figure_long_covid_count.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      long_covid_count_all = glue("output/review/descriptives/long_covid_count_*_all*") # all regions
    )
  ),
  comment("Figure_hist - Histogram of days from covid to long covid"),
  action(
    name = "figure_hist_all",
    run = "r:latest analysis/figure_hist.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      days_covid_to_long= glue("output/review/descriptives/days_c_to_long*"),
      table_bin_count= glue("output/review/descriptives/supporting_doc_hist_*")
    )
  ),
  comment("Figure - Kaplan Meier plot"),
  action(
    name = "figure_km_all",
    run = "r:latest analysis/figure_kaplan_meier.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      plot_km = glue("output/review/descriptives/figure_kaplan_meier_*"),
      data_km = glue("output/review/descriptives/supporting_doc_km_*")
    )
  ),
  comment("Table - frequencies of snomed code for long covid diagnosis"),
  action(
    name = "table_snomed_codes_all",
    run = "r:latest analysis/table_snomed_code.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      snomed_code = glue("output/not_for_review/descriptives/snomed_code*")
    )
  ),
  # comment("figure - long covid count by region"),
  # action(
  #   name = "figure_long_covid_region_all",
  #   run = "r:latest analysis/figure_long_covid_count_region.R",
  #   needs = list("stage1_define_eligible_population_all"),
  #   moderately_sensitive = list(
  #     long_covid_count_region = glue("output/not_for_review/descriptives/long_covid_count_*_region*")
  #   )
  # ),
  comment("Summarise survival data"),
  action(
    name = "summarise_survival_data_all",
    run = "r:latest analysis/stage2_summarise_survival_data.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      summary_survival_data = glue("output/review/descriptives/summarise_survival_data*")
    )
  ),
  comment("Part 4. Modelling"),
  comment("Development Cox model"),
  comment("post-viral fatigue"),
  action(
    name = "development_cox_model_fatigue",
    run = "r:latest analysis/stage3_fatigue_survival_analysis.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      summary_survival_data = glue("output/review/model/analysis_data_summary_fatigue*"),
      cox_model_output = glue("output/review/model/HR_fatigue*")
    )
  ),
  # long COVID code
  splice(
    unlist(lapply(analysis, function(x) apply_development_cox_model(analysis = x)), recursive = FALSE)
  ),
  # Age sex Cox model
  splice(
    unlist(lapply(analysis, function(x) apply_development_cox_model_age_sex(analysis = x)), recursive = FALSE)
  ),
  # Age sex adjsuted Cox model
  splice(
    unlist(lapply(analysis, function(x) apply_development_cox_model_age_sex_adjusted(analysis = x)), recursive = FALSE)
  ),
  comment("Subset variables"),
  action(
    name = "identify_subset_variables",
    run = "r:latest analysis/stage3_identify_subset_variables.R",
    needs = list("development_cox_model_all", "development_cox_model_all_vax_c",
                 "development_cox_model_all_vax_td", "development_cox_model_vaccinated",
                 "development_cox_model_infected"),
    moderately_sensitive = list(
      subset_variables = glue("output/review/model/selected_vars.csv")
    )
  ),
  splice(
    unlist(lapply(analysis, function(x) apply_cox_model_subset_variables(analysis = x)), recursive = FALSE)
  ),
  comment("Figure - log hazard ratio against continuous age"),
  splice(
    unlist(lapply(analysis, function(x) apply_figure_loghr_age(analysis = x)), recursive = FALSE)
  ),
  comment("Evaluation Cox model"),
  splice(
    unlist(lapply(analysis, function(x) apply_evaluation_cox_model(analysis = x)), recursive = FALSE)
  )
  # comment("Validation Cox model"),
  # splice(
  #   unlist(lapply(analysis, function(x) apply_validation_cox_model_iecv(analysis = x)), recursive = FALSE)
  # )
)
  ## combine everything ----
  project_list <- splice(
    defaults_list,
    list(actions = actions_list)
  )

#####################################################################################
## convert list to yaml, reformat comments and white space, and output a .yaml file #
#####################################################################################
as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")