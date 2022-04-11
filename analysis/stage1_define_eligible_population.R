# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: define eligible population
# Output:  input_stage1.rds

library(readr); library(dplyr); library(htmlTable)
input <- read_rds("output/input_stage0.rds")

#View(input[,vars_dates])

# Step 4. Define eligible population--------------------------------------------
steps <- c("starting point","dead before index date", "missing sex", "missing age", "age <18y", "age>105y", "ethnicity")
# starting point
flow_chart_n <- nrow(input)

# Dead: removed if dead before index date
input <- input%>%filter(death_date > index_date | is.na(death_date))
flow_chart_n <- c(flow_chart_n, nrow(input))

# Sex: remove if missing
input <- input%>%filter(!is.na(cov_cat_sex))
table(input$cov_cat_sex)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Age: remove if missing
input <- input%>%filter(!is.na(cov_num_age))
#table(input$cov_cat_age)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Adult: remove if age < 18
input <- input%>%filter(cov_num_age>=18)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Adult: remove if age > 105 years
input <- input%>%filter(cov_num_age <=105)
flow_chart_n <- c(flow_chart_n, nrow(input))

# Ethnicity: remove if missing
input <- input%>%filter(!is.na(cov_cat_ethnicity))
flow_chart_n <- c(flow_chart_n, nrow(input))

# previous long covid
#cohort_start = as.Date("2020-12-01", format="%Y-%m-%d")
#index <- which(input$out_first_long_covid_date < cohort_start)

#table(input$cov_cat_previous_covid)
#input <- input%>%filter(cov_cat_previous_covid == " No COVID code")
#flow_chart_n <- c(flow_chart_n, nrow(input))

flow_chart<-cbind(steps, flow_chart_n)

write.csv(flow_chart, file="output/flow_chart.csv")

rmarkdown::render("analysis/compiled_flow_chart_results.Rmd",
                  output_file=paste0("flow_chart_", population),output_dir="output")
#htmlTable(flow_chart, file="output/flow_chart.html")


# # For categorical variables, replace "na" with "Missing" as a category
# cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
# input_factor_vars <- input[,cov_factor_names]
# 
# # why this doesn't pick up na in smoking status???
# for(i in 1:length(cov_factor_names)){
#   index = which(is.na(input_factor_vars[,i]))
#   if(length(index)>0){
#     input_factor_vars[index,i]="Missing"
#   }
# }

# # vaccination status is a dynamic variable
# input[,cov_factor_names] <- input_factor_vars 
# input$vax_doses <- NULL
# index = which(!is.na(input$vax_covid_date1)& input$vax_covid_date1  < input$index_date)
# input$vax_does[index] = 1

saveRDS(input, file = "output/input_stage1.rds")

