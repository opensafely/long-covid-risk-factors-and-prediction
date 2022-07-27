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
  expectations= list(population_size=15000L)
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
        variable_check_table_CSV = glue("output/not_for_review/descriptives/table_0_{cohort}.csv"),
        variable_check_table_HTML = glue("output/not_for_review/descriptives/table_0_{cohort}.html"),
        histogram_numerical_variable = glue("output/not_for_review/descriptives/histogram_*_{cohort}.svg")
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
        descriptive_table_CSV = glue("output/review/descriptives/table_1_{cohort}.csv"),
        descriptive_table_HTML = glue("output/review/descriptives/table_1_{cohort}.html")
      )
    )
  )
}

apply_table2 <- function(cohort){
  splice(
    comment(glue("Table 2. Event count and incidence rate - {cohort}")),
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
        incidence_rate_table_CSV = glue("output/review/descriptives/table_2_{cohort}.csv"),
        incidence_rate_talbe_HTML = glue("output/review/descriptives/table_2_{cohort}.html")
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
        flow_chart_csv = glue("output/flow_chart_{cohort}.csv"),
        flow_chart_html = glue("output/flow_chart_{cohort}.html")
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
        supporting_document_CSV = glue("output/review/model/analysis_data_summary_{analysis}.csv"),
        supporting_document_html = glue("output/review/model/analysis_data_summary_{analysis}.html"),
        hazard_ratios_CSV = glue("output/review/model/hazard_ratio_estimates_*_{analysis}.csv"),
        hazard_ratios_HTML = glue("output/review/model/hazard_ratio_estimates_*_{analysis}.html"),
        selected_variables = glue("output/not_for_review/model/selected_variables_{analysis}.csv"),
        model_selection = glue("output/not_for_review/model/model_selection_{analysis}.csv")
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
        #ph_test_CSV = glue("output/review/model/PH_test_*_{analysis}.csv"),
        #supporting_document_CSV = glue("output/review/model/analysis_data_summary_{analysis}.csv"),
        #supporting_document_html = glue("output/review/model/analysis_data_summary_{analysis}.html"),
        fit_cox_model = glue("output/not_for_review/model/fit_cox_model_age_sex_{analysis}.rds"),
        hazard_ratios_CSV = glue("output/review/model/hazard_ratio_estimates_age_sex_*_{analysis}.csv"),
        hazard_ratios_HTML = glue("output/review/model/hazard_ratio_estimates_age_sex_*_{analysis}.html"),
        performance_measure_CSV = glue("output/review/model/performance_measures_age_sex_*_{analysis}.csv"),
        performance_measure_HTML = glue("output/review/model/performance_measures_age_sex_*_{analysis}.html"),
        survival_plot = glue("output/review/model/survival_plot_*_age_sex_*_{analysis}.svg")
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
        hazard_ratios_CSV = glue("output/review/model/HR_estimates_age_sex_adjusted_{analysis}.csv"),
        hazard_ratios_HTML = glue("output/review/model/HR_estimates_age_sex_adjusted_{analysis}.html"),
        performance_measure_CSV = glue("output/review/model/performance_measures_age_sex_adjusted_{analysis}.csv"),
        performance_measure_HTML = glue("output/review/model/performance_measures_age_sex_adjusted_{analysis}.html")
      )
    )
  )
}

# apply_development_cox_model_subset_variables <- function(analysis){
#   splice(
#     comment(glue("Development Cox Model using a subset of variables - {analysis}")),
#     action(
#       name = list(glue("development_model_subset_variables_{analysis}")),
#       run = "r:latest analysis/stage3_4_model_dev_eval_subset_variables.R",
#       arguments = c(analysis),
#       # needs =list("development_cox_model_all", "development_cox_model_all_vax_c",
#       #             "development_cox_model_all_vax_td",
#       #             "development_cox_model_vaccinated", "development_cox_model_infected"),
#       needs =list("stage1_define_eligible_population_all",
#                   "stage1_define_eligible_population_vaccinated",
#                   "stage1_define_eligible_population_infected"),
#       moderately_sensitive = list(
#         hazard_ratios_CSV = glue("output/review/model/HR_estimates_selected_vars_{analysis}.csv"),
#         hazard_ratios_HTML = glue("output/review/model/HR_estimates_selected_vars_{analysis}.html"),
#         performance_measure_CSV = glue("output/review/model/performance_measures_selected_vars_{analysis}.csv"),
#         performance_measure_HTML = glue("output/review/model/performance_measures_selected_vars_{analysis}.html")
#       )
#     )
#   )
# }

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
        performance_measure_CSV = glue("output/review/model/performance_measures_*_{analysis}.csv"),
        performance_measure_HTML = glue("output/review/model/performance_measures_*_{analysis}.html"),
        survival_plot = glue("output/review/model/survival_plot_*_{analysis}.svg")
      )
    )
  )
}

# apply_evaluation_cox_model_age_sex <- function(analysis){
#   splice(
#     action(
#       name = glue("evaluation_cox_model_age_sex_{analysis}"),
#       run = "r:latest analysis/stage4_model_eval_age_sex.R",
#       arguments = c(analysis),
#       needs = list(glue("development_model_age_sex_{analysis}")),
#       moderately_sensitive = list(
#         performance_measure_CSV = glue("output/review/model/performance_measures_age_sex_*_{analysis}.csv"),
#         performance_measure_HTML = glue("output/review/model/performance_measures_age_sex_*_{analysis}.html"),
#         survival_plot = glue("output/review/model/survival_plot_age_sex_*_{analysis}.svg")
#       )
#     )
#   )
# }

apply_validation_cox_model_iecv <- function(analysis){
  splice(
    comment(glue("Validation Cox Model - {analysis}")),
    action(
      name = glue("validation_cox_model_{analysis}"),
      run = "r:latest analysis/stage5_model_validation_iecv_revised.R",
      arguments = c(analysis),
      needs = list(if(analysis == "all" | analysis == "all_vax_c" | analysis == "all_vax_td"){
        glue("stage1_define_eligible_population_all")}else{
          glue("stage1_define_eligible_population_{analysis}")
        }),
      moderately_sensitive = list(
        val_performance_measure_CSV = glue("output/review/model/val_performance_measures_{analysis}.csv"),
        val_performance_measure_html = glue("output/review/model/val_performance_measures_{analysis}.html"),
        val_cal_plot = glue("output/review/model/val_cal_plot_*_{analysis}.svg"),
        val_re_cal_plot = glue("output/review/model/val_re_cal_plot_*_{analysis}.svg")
      )
    )
  )
}
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
  comment("Define eligible population"),
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
  comment("table_3 - sequence count"),
  action(
    name = "table_3_all",
    run = "r:latest analysis/table_3.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      sequence_count_table_CSV = glue("output/review/descriptives/table_3.csv"),
      sequence_count_table_HTML = glue("output/review/descriptives/table_3.html")
    )
  ),
  comment("Figure_1 - long covid count"),
  action(
    name = "figure_1_all",
    run = "r:latest analysis/figure_1.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      figure_long_covid_count_all = glue("output/figure_1_*.svg"),
      table_csv_long_covid_count_all = glue("output/review/descriptives/long_covid_count_*_all.csv"),
      table_html_long_covid_count_all = glue("output/review/descriptives/long_covid_count_*_all.html")
    )
  ),
  comment("Figure_hist - Histogram of days from covid to long covid"),
  action(
    name = "figure_hist_all",
    run = "r:latest analysis/figure_hist.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      figure_days_c_to_lc = glue("output/review/descriptives/figure_hist.svg"),
      table_csv_summary= glue("output/review/descriptives/summary_days_c_to_long.csv"),
      table_bin_count= glue("output/review/descriptives/hist_*")
    )
  ),
  # comment("Figure - hazard ratio plot"),
  # action(
  #   name = "figure_hazard_ratio",
  #   run = "r:latest analysis/figure_hazard_ratio_plot.R",
  #   needs = glue("development_cox_model_{analysis_to_run}"),
  #   moderately_sensitive = list(
  #     figure_hazard_ratio_plot = glue("output/review/model/figure_hr_*.svg")
  #   )
  # ),
  comment("Figure - cumulative probability plot"),
  action(
    name = "figure_cum_prob_km_all",
    run = "r:latest analysis/figure_cum_prob_km.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      cum_prob_plot = glue("output/review/descriptives/figure_cum_*.svg")
    )
  ),
  comment("Suppl_table_1 - frequencies of snomed code for long covid diagnosis"),
  action(
    name = "suppl_table_1_all",
    run = "r:latest analysis/suppl_table_1.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      pie_chart_long_covid_code = glue("output/review/descriptives/suppl_figure_pie.svg"),
      table_csv_long_covid_code = glue("output/review/descriptives/suppl_table_1.csv"),
      table_html_long_covid_code = glue("output/review/descriptives/suppl_table_1.html")
    )
  ),
  comment("Suppl_figure_1 - long covid count by region"),
  action(
    name = "suppl_figure_1_all",
    run = "r:latest analysis/suppl_figure_1.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      figure_long_covid_count_region = glue("output/review/descriptives/suppl_figure_1_*.svg"),
      table_csv_long_covid_count_region = glue("output/review/descriptives/long_covid_count_*.csv"),
      table_html_long_covid_region = glue("output/review/descriptives/long_covid_count_*.html")
    )
  ),
  comment("Summarise survival data"),
  action(
    name = "summarise_survival_data_all",
    run = "r:latest analysis/stage2_summarise_survival_data.R",
    needs = list("stage1_define_eligible_population_all"),
    moderately_sensitive = list(
      summary_survival_data_CSV = glue("output/review/descriptives/summarise_survival_data.csv"),
      summary_survival_data_HTML = glue("output/review/descriptives/summarise_survival_data.html")
    )
  ),
  comment("Part 4. Modelling"),
  comment("Development Cox model"),
  
  splice(
    unlist(lapply(analysis, function(x) apply_development_cox_model(analysis = x)), recursive = FALSE)
  ),

  
  splice(
    unlist(lapply(analysis, function(x) apply_development_cox_model_age_sex(analysis = x)), recursive = FALSE)
  ),
  
  splice(
    unlist(lapply(analysis, function(x) apply_development_cox_model_age_sex_adjusted(analysis = x)), recursive = FALSE)
  ),
  
  # splice(
  #   unlist(lapply(analysis, function(x) apply_development_cox_model_subset_variables(analysis = x)), recursive = FALSE)
  # ),
  # 
  comment("Evaluation Cox model"),
  splice(
    unlist(lapply(analysis, function(x) apply_evaluation_cox_model(analysis = x)), recursive = FALSE)
  ),
  # splice(
  #   unlist(lapply(analysis, function(x) apply_evaluation_cox_model_age_sex(analysis = x)), recursive = FALSE)
  # ),
  comment("Validation Cox model"),
  splice(
    unlist(lapply(analysis, function(x) apply_validation_cox_model_iecv(analysis = x)), recursive = FALSE)
  )
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