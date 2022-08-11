function_df_summary <- function(input, analysis){
  df_summary <- data.frame(variable = character(),
                           number  = numeric(),
                           percent = numeric(),
                           mean    = numeric(),
                           sd      = numeric(), 
                           IQR   = numeric(),
                           stringsAsFactors = FALSE)
  
  cov_factor_names <- names(input)[grepl("cov_cat", names(input))]
  cov_num_names <- names(input)[grepl("cov_num", names(input))]
  # factor variables: number and percentage---------------------------------------
  input_factor_vars <- input[, cov_factor_names]
  for(i in 1:length(cov_factor_names)){
    levels = names(table(input_factor_vars[,i]))
    start = nrow(df_summary)+1
    df_summary[start,1] = cov_factor_names[i]
    start = nrow(df_summary)+1
    end = nrow(df_summary)+length(levels)
    df_summary[start:end,1] <- c(levels)            # variable name
    df_summary[start:end,2] <- c(table(input_factor_vars[,i]))  # number
    df_summary[start:end,3] <- 100*round(c(table(input_factor_vars[,i]))/nrow(input_factor_vars),4)  # percentage
    print(levels)
  }
  # numerical variables: number and percentage of observations, mean and standard deviations
  input_num_vars <- input[,cov_num_names]
  if(length(cov_num_names) == 1){
    index = nrow(df_summary)+1
    df_summary[index,1] <- cov_num_names
    df_summary[index,2] <- length(which(!is.na(unlist(input_num_vars)))) # number of observations
    df_summary[index,4] <- round(mean(unlist(input_num_vars)),2) # mean
    df_summary[index,5] <- round(sd(unlist(input_num_vars)),2) # sd
    df_summary[index,6] <- round(IQR(unlist(input_num_vars)),2)  # IQR
  }
  if(length(cov_num_names)>1){
    for(i in 1:length(cov_num_names)){
      index = nrow(df_summary)+1
      df_summary[index,1] <- cov_num_names[i]
      df_summary[index,2] <- length(which(!is.na(unlist(input_num_vars[,i])))) # number of observations
      df_summary[index,4] <- round(mean(unlist(input_num_vars[,i])),2) # mean
      df_summary[index,5] <- round(sd(unlist(input_num_vars[,i])),2) # sd
      df_summary[index,6] <- round(IQR(unlist(input_num_vars[,i])),2)  # IQR
    }
  }
  
  # Output as help file: number of observations from the sampled population in each covariate level
  write.csv(df_summary, file=paste0("output/review/model/analysis_data_summary_", analysis,".csv"), row.names = F)
  rmarkdown::render("analysis/compilation/compiled_analysis_data_summary.Rmd",
                    output_file=paste0("analysis_data_summary_", analysis),output_dir="output/review/model")
}
