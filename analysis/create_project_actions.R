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

analysis <- analysis_development <- c("all", "vax_c", "vaccinated", "all_vax_td", "infected") 
cohort <- c("all", "vaccinated", "infected")

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

apply_development_cox_model <- function(analysis_development){
  splice(
    comment(glue("Development Cox Model - {analysis_development}")),
    action(
      name = glue("development_cox_model_{analysis_development}"),
      run = "r:latest analysis/stage3_model_development.R",
      arguments = c(analysis_development),
      needs = list("stage1_define_eligible_population"),
      moderately_sensitive = list(
        ph_test_CSV = glue("output/review/model/PH_test_*_{analysis_development}.csv"),
        hazard_ratios_CSV = glue("output/review/model/hazard_ratio_estimates_*_{analysis_development}.csv"),
        hazard_ratios_HTML = glue("output/review/model/hazard_ratio_estimates_*_{analysis_development}.html"),
        model_selection = glue("output/not_for_review/model/model_selection_{analysis_development}.csv")
      )
    )
  )
}

apply_evaluation_cox_model <- function(analysis){
  splice(
    comment(glue("Evaluation Cox Model - {analysis}")),
    action(
      name = glue("evaluation_cox_model_{analysis}"),
      run = "r:latest analysis/stage4_model_evaluation.R",
      arguments = c(analysis),
      needs = list("stage1_define_eligible_population"),
      moderately_sensitive = list(
        performance_measure_CSV = glue("output/review/model/performance_measures_*_{analysis}.csv"),
        performance_measure_HTML = glue("output/review/model/performance_measures_*_{analysis}.html"),
        survival_plot = glue("output/review/model/survival_plot_*_{analysis}.svg")
      )
    )
  )
}

apply_validation_cox_model_iecv <- function(analysis){
  splice(
    comment(glue("Validation Cox Model - {analysis}")),
    action(
      name = glue("validation_cox_model_{analysis}"),
      run = "r:latest analysis/stage5_model_validation_iecv.R",
      arguments = c(analysis),
      needs = list("stage1_define_eligible_population"),
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
  comment("Generate dummy data for study_definition for all eligible population"),
  action(
    name = "generate_study_population_all",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_all --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_all.feather")
    )
  ),
  comment("Generate dummy data for study_definition - vaccinated"),
  action(
    name = "generate_study_population_vaccinated",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_vaccinated --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_vaccinated.feather")
    )
  ),
  comment("Generate dummy data for study_definition - infected"),
  action(
    name = "generate_study_population_infected",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_infected --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input_infected.feather")
    )
  ),
  comment("Stage 0 - Data cleaning"),
  action(
    name = "stage0_data_cleaning",
    run = "r:latest analysis/stage0_data_cleaning.R all_cohorts",
    needs = list("generate_study_population_all", "generate_study_population_vaccinated", "generate_study_population_infected"),
    moderately_sensitive = list(
      variable_check_table_CSV = glue("output/not_for_review/descriptives/table_0_*.csv"),
      variable_check_table_HTML = glue("output/not_for_review/descriptives/table_0_*.html"),
      histogram_numerical_variable = glue("output/not_for_review/descriptives/histogram_*")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_stage0_*.rds")
    )
  ),
  comment("Stage 1 - Define eligible population"),
  action(
    name = "stage1_define_eligible_population",
    run = "r:latest analysis/stage1_define_eligible_population.R all_cohorts",
    needs = list("stage0_data_cleaning"),
    moderately_sensitive = list(
      flow_chart_csv = glue("output/flow_chart_*.csv"),
      flow_chart_html = glue("output/flow_chart_*.html")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_stage1_*.rds")
    )
  ),
  comment("table_1 - Patient characteristics"),  
  action(
    name = "table_1",
    run = "r:latest analysis/table_1.R all_cohorts",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      descriptive_table_CSV = glue("output/review/descriptives/table_1_*.csv"),
      descriptive_table_HTML = glue("output/review/descriptives/table_1_*.html")
    )
  ),
  comment("table_2 - event count and incidence rate"),  
  action(
    name = "table_2",
    run = "r:latest analysis/table_2.R all_cohorts",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      incidence_rate_table_CSV = glue("output/review/descriptives/table_2_*.csv"),
      incidence_rate_talbe_HTML = glue("output/review/descriptives/table_2_*.html")
    ),
  ),
  comment("table_3 - sequence count"),
  action(
    name = "table_3",
    run = "r:latest analysis/table_3.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      sequence_count_table_CSV = glue("output/review/descriptives/table_3.csv"),
      sequence_count_table_HTML = glue("output/review/descriptives/table_3.html")
    )
  ),
  comment("Figure_1 - long covid count"),
  action(
    name = "figure_1",
    run = "r:latest analysis/figure_1.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      figure_long_covid_count_all = glue("output/figure_1_*.svg"),
      table_csv_long_covid_count_all = glue("output/review/descriptives/long_covid_count_*_all.csv"),
      table_html_long_covid_count_all = glue("output/review/descriptives/long_covid_count_*_all.html")
    )
  ),
  comment("Figure_2 - days from covid to long covid"),
  action(
    name = "figure_2",
    run = "r:latest analysis/figure_2.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      figure_days_c_to_lc = glue("output/review/descriptives/figure_hist.svg"),
      table_csv_summary= glue("output/review/descriptives/summary_days_c_to_long.csv")
    )
  ),
  comment("Figure - hazard ratio plot"),
  action(
    name = "figure_hazard_ratio",
    run = "r:latest analysis/figure_hazard_ratio_plot.R",
    needs = glue("development_cox_model_{analysis}"),
    moderately_sensitive = list(
      figure_hazard_ratio_plot = glue("output/review/model/figure_hr_*.svg")
    )
  ),
  comment("Figure - cumulative probability plot"),
  action(
    name = "figure_cum_prob_km",
    run = "r:latest analysis/figure_cum_prob_km.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      cum_prob_plot = glue("output/review/descriptives/figure_cum_*.svg")
    )
  ),
  comment("Suppl_table_1 - frequencies of snomed code for long covid diagnosis"),
  action(
    name = "suppl_table_1",
    run = "r:latest analysis/suppl_table_1.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      pie_chart_long_covid_code = glue("output/review/descriptives/suppl_figure_pie.svg"),
      table_csv_long_covid_code = glue("output/review/descriptives/suppl_table_1.csv"),
      table_html_long_covid_code = glue("output/review/descriptives/suppl_table_1.html")
    )
  ),
  comment("Suppl_figure_1 - long covid count by region"),
  action(
    name = "suppl_figure_1",
    run = "r:latest analysis/suppl_figure_1.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      figure_long_covid_count_region = glue("output/review/descriptives/suppl_figure_1_*.svg"),
      table_csv_long_covid_count_region = glue("output/review/descriptives/long_covid_count_*.csv"),
      table_html_long_covid_region = glue("output/review/descriptives/long_covid_count_*.html")
    )
  ),
  comment("Summarise survival data"),
  action(
    name = "summarise_survival_data",
    run = "r:latest analysis/stage2_summarise_survival_data.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      summary_survival_data_CSV = glue("output/review/descriptives/summarise_survival_data.csv"),
      summary_survival_data_HTML = glue("output/review/descriptives/summarise_survival_data.html")
    )
  ),
  comment("Development Cox model"),
  splice(
    # over outcomes
    unlist(lapply(analysis_development, function(x) apply_development_cox_model(analysis_development = x)), recursive = FALSE)
  ),
  comment("Evaluation Cox model"),
  splice(
    # over outcomes
    unlist(lapply(analysis, function(x) apply_evaluation_cox_model(analysis = x)), recursive = FALSE)
  ),
  splice(
    # over outcomes
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