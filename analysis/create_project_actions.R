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
  expectations= list(population_size=10000L)
)

#analyses <- c("all", "vax_c", "vaccinated")

analysis <- c("all", "vax_c")

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
apply_development_cox_model <- function(analysis){
  splice(
    comment(glue("Development Cox Model - {analysis}")),
    action(
      name = glue("development_cox_model_{analysis}"),
      run = "r:latest analysis/stage3_model_development.R",
      arguments = c(analysis),
      needs = list("stage1_define_eligible_population"),
      moderately_sensitive = list(
        ph_test_CSV = glue("output/PH_test_*_{analysis}.csv"),
        hazard_ratios_CSV = glue("output/hazard_ratio_estimates_*_{analysis}.csv"),
        hazard_ratios_HTML = glue("output/hazard_ratio_estimates_*_{analysis}.html")
        #calibration = glue("output/calibration_development_*_{analysis}.svg")
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
        performance_measure_CSV = glue("output/performance_measures_*_{analysis}.csv"),
        performance_measure_HTML = glue("output/performance_measures_*_{analysis}.html"),
        survival_plot = glue("output/survival_plot_*_{analysis}.svg")
      )
    )
  )
}

apply_validation_cox_model <- function(analysis){
  splice(
    comment(glue("Validation Cox Model - {analysis}")),
    action(
      name = glue("validation_cox_model_{analysis}"),
      run = "r:latest analysis/stage5_model_validation.R",
      arguments = c(analysis),
      needs = list("stage1_define_eligible_population"),
      moderately_sensitive = list(
        val_performance_measure_CSV = glue("output/val_performance_measures_{analysis}.csv"),
        val_cal_plot = glue("output/val_cal_plot_*_{analysis}.svg"),
        val_re_cal_plot = glue("output/val_re_cal_plot_*_{analysis}.svg")
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
  
  #comment("Generate dummy data for study_definition - electively_unvaccinated"),
  action(
    name = "generate_study_population",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition --output-format feather",
    highly_sensitive = list(
      cohort = glue("output/input.feather")
    )
  ),
  #comment("Stage 0 - Data cleaning"),
  action(
    name = "stage0_data_cleaning",
    run = "r:latest analysis/stage0_data_cleaning.R",
    needs = list("generate_study_population"),
    moderately_sensitive = list(
      variable_check_table_CSV = glue("output/table_0.csv"),
      variable_check_table_HTML = glue("output/table_0.html")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_stage0.rds")
    )
  ),
  #comment("Stage 1 - Define eligible population"),
  action(
    name = "stage1_define_eligible_population",
    run = "r:latest analysis/stage1_define_eligible_population.R",
    needs = list("stage0_data_cleaning"),
    moderately_sensitive = list(
      flow_chart_csv = glue("output/flow_chart.csv"),
      flow_chart_html = glue("output/flow_chart.html")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_stage1_*.rds")
    )
  ),
  #comment("table_1 - Patient characteristics"),  
  action(
    name = "table_1",
    run = "r:latest analysis/table_1.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      descriptive_table_CSV = glue("output/table_1.csv"),
      descriptive_table_HTML = glue("output/table_1.html")
    )
  ),
  #comment("table_2 - event count and incidence rate"),  
  action(
    name = "table_2",
    run = "r:latest analysis/table_2.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      incidence_rate_table_CSV = glue("output/table_2.csv"),
      incidence_rate_talbe_HTML = glue("output/table_2.html")
    ),
  ),
  #comment("table_3 - sequence count"),
  action(
    name = "table_3",
    run = "r:latest analysis/table_3.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      sequence_count_table_CSV = glue("output/table_3.csv"),
      sequence_count_table_HTML = glue("output/table_3.html")
    )
  ),
  #comment("Figure_1 - long covid count"),
  action(
    name = "figure_1",
    run = "r:latest analysis/figure_1.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      figure_long_covid_count_all = glue("output/figure_1_*.svg"),
      table_csv_long_covid_count_all = glue("output/long_covid_count_*_all.csv"),
      table_html_long_covid_count_all = glue("output/long_covid_count_*_all.html")
    )
  ),
  #comment("Figure_2 - days from covid to long covid"),
  action(
    name = "figure_2",
    run = "r:latest analysis/figure_2.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      figure_days_c_to_lc = glue("output/figure_hist.svg"),
      table_csv_summary= glue("output/summary_days_c_to_long.csv")
    )
  ),
  #comment("Suppl_table_1 - frequencies of snomed code for long covid diagnosis"),
  action(
    name = "suppl_table_1",
    run = "r:latest analysis/suppl_table_1.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      pie_chart_long_covid_code = glue("output/suppl_figure_pie.svg"),
      table_csv_long_covid_code = glue("output/suppl_table_1.csv"),
      table_html_long_covid_code = glue("output/suppl_table_1.html")
    )
  ),
  #comment("Suppl_figure_1 - long covid count by region"),
  action(
    name = "suppl_figure_1",
    run = "r:latest analysis/suppl_figure_1.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      figure_long_covid_count_region = glue("output/suppl_figure_1_*.svg"),
      table_csv_long_covid_count_region = glue("output/long_covid_count_*.csv"),
      table_html_long_covid_region = glue("output/long_covid_count_*.html")
    )
  ),
  #comment("Summarise survival data"),
  action(
    name = "summarise_survival_data",
    run = "r:latest analysis/stage2_summarise_survival_data.R",
    needs = list("stage1_define_eligible_population"),
    moderately_sensitive = list(
      summary_survival_data_CSV = glue("output/summarise_survival_data.csv"),
      summary_survival_data_HTML = glue("output/summarise_survival_data.html")
    )
  ),
  #comment("Development Cox model"),
  splice(
    # over outcomes
    unlist(lapply(analysis, function(x) apply_development_cox_model(analysis = x)), recursive = FALSE)
  ),
  # #comment("Evaluation Cox model"),
  # action(
  #   name = "evaluation_cox_model",
  #   run = "r:latest analysis/stage4_model_evaluation.R",
  #   needs = list("stage1_define_eligible_population"),
  #   moderately_sensitive = list(
  #     performance_measure_CSV = glue("output/performance_measures_*.csv"),
  #     performance_measure_HTML = glue("output/performance_measures_*.html"),
  #     survival_plot = glue("output/survival_plot_*.svg")
  #   )
  # ),
  
  #comment("Evaluation Cox model"),
  splice(
    # over outcomes
    unlist(lapply(analysis, function(x) apply_evaluation_cox_model(analysis = x)), recursive = FALSE)
  ),
  # #comment("Validation Cox model"),
  # action(
  #   name = "validation_cox_model",
  #   run = "r:latest analysis/stage5_model_validation.R",
  #   needs = list("stage1_define_eligible_population"),
  #   moderately_sensitive = list(
  #     val_performance_measure_CSV = glue("output/val_performance_measures.csv"),
  #     val_cal_plot = glue("output/val_cal_plot_*.svg"),
  #     val_re_cal_plot = glue("output/val_re_cal_plot_*.svg")
  #   )
  # )
  #comment("Validation Cox model"),
  splice(
    # over outcomes
    unlist(lapply(analysis, function(x) apply_validation_cox_model(analysis = x)), recursive = FALSE)
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