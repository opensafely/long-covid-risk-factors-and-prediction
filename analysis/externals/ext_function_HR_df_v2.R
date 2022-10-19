function_combine_hr <- function(df_list_full, df_list_full_categorical, csv_index, cohort){
  # HR from full models with age splines
  df_hr <- data.frame(df_list_full[csv_index])
  # HR from full models with age categorical
  df_hr_cat_age <- data.frame(df_list_full_categorical[csv_index])
  df_hr_cat_age <- df_hr_cat_age %>% filter(grepl("age", term))
  df_hr_cat_age <- df_hr_cat_age %>% mutate(term = sub("40_59", "40 to 59", term)) %>%
    mutate(term = sub("60_79", "60 to 79", term)) %>%
    mutate(term = sub("80_105", "80 to 105", term))
  df_hr_combined <- rbind(df_hr, df_hr_cat_age)
  df_hr_combined <- df_hr_combined %>% filter(!grepl("num_age", term)) # remove estimated coefficients for age splines
  df_hr_combined$cohort = cohort
  return(df_hr_combined)
}

function_combine_as_hr <- function(df_list_age_sex_adjusted, df_list_age_sex, df_list_age_sex_categorical, csv_index, cohort){
  # To obtain age sex adjusted hazard ratios for all other covariates apart from age and sex
  df_hr_as_adj <- as.data.frame(df_list_age_sex_adjusted[csv_index])
  if(cohort != "Vaccination time-dependent"){
    df_hr_as_adj <- df_hr_as_adj %>% filter(term != "cov_cat_sex=F") %>%
      filter(term != "cov_num_age") %>% filter(term != "cov_num_age'")
  }else{
    df_hr_as_adj <- df_hr_as_adj %>% filter(term != "cov_cat_sex=F") %>%
      filter(term != "cov_num_age") %>% filter(term != "cov_num_age'") %>%
      filter(term != "cov_cat_ie.status=1")
  }
  
  df_hr_as_adj <- df_hr_as_adj %>% select(!c(model, predictor))
  
  # To obtain HR for sex using age-sex model with age spline
  df_hr_as_spl <- as.data.frame(df_list_age_sex[csv_index])
  
  df_hr_as_spl <- df_hr_as_spl %>% filter(term != "cov_num_age") %>% 
    filter(term != "cov_num_age'")
  
  # To obtain HR for categorical age using age-sex model with age categorical
  df_hr_as_cat <- as.data.frame(df_list_age_sex_categorical[csv_index])
  df_hr_as_cat <- df_hr_as_cat %>% filter(term != "cov_cat_sex=F")
  
  # combine hr from age-sex adjusted models
  df_hr_as <- rbind(df_hr_as_adj, df_hr_as_spl, df_hr_as_cat)
  df_hr_as$cohort = cohort
  df_hr_as <- df_hr_as %>% mutate(term = sub("40_59", "40 to 59", term)) %>%
    mutate(term = sub("60_79", "60 to 79", term)) %>%
    mutate(term = sub("80_105", "80 to 105", term))
  return(df_hr_as)
}

function_combine_as_hr_v2 <- function(df_list_age_sex_adjusted, df_list_age_sex, df_list_age_sex_categorical, csv_index, cohort){
  # To obtain age sex adjusted hazard ratios for all other covariates apart from age and sex
  df_hr_as_adj <- as.data.frame(df_list_age_sex_adjusted[csv_index])
  if(cohort != "Vaccination time-dependent"){
    df_hr_as_adj <- df_hr_as_adj %>% filter(term != "cov_cat_sex=F") %>%
      filter(term != "cov_num_age") %>% filter(term != "cov_num_age'")
  }else{
    df_hr_as_adj <- df_hr_as_adj %>% filter(term != "cov_cat_sex=F") %>%
      filter(term != "cov_num_age") %>% filter(term != "cov_num_age'") %>%
      filter(term != "cov_cat_ie.status=1")
  }
  
  # df_hr_as_adj <- df_hr_as_adj %>% select(!c(model, predictor))
  
  # To obtain HR for sex using age-sex model with age spline
  df_hr_as_spl <- as.data.frame(df_list_age_sex[csv_index])
  
  df_hr_as_spl <- df_hr_as_spl %>% filter(term != "cov_num_age") %>% 
    filter(term != "cov_num_age'")
  
  # To obtain HR for categorical age using age-sex model with age categorical
  df_hr_as_cat <- as.data.frame(df_list_age_sex_categorical[csv_index])
  df_hr_as_cat <- df_hr_as_cat %>% filter(term != "cov_cat_sex=F")
  
  # combine hr from age-sex adjusted models
  df_hr_as <- rbind(df_hr_as_adj, df_hr_as_spl, df_hr_as_cat)
  df_hr_as$cohort = cohort
  df_hr_as <- df_hr_as %>% mutate(term = sub("40_59", "40 to 59", term)) %>%
    mutate(term = sub("60_79", "60 to 79", term)) %>%
    mutate(term = sub("80_105", "80 to 105", term))
  return(df_hr_as)
}


function_HR_df_v2 <- function(df_hr, csv_hr_order, cohort, common_dir){
  hr <- df_hr
  hr$variable = NULL
  hr <- hr%>% mutate(variable = ifelse(grepl("age",term), "Demographics",
                                       ifelse(grepl("sex",term), "Demographics",
                                              ifelse(grepl("bmi", term), "Demographics",
                                                     ifelse(grepl("ethnicity", term), "Demographics",
                                                            ifelse(grepl("region", term), "Demographics", 
                                                                   ifelse(grepl("imd", term), "Demographics",
                                                                          ifelse(grepl("gp", term), "GP-Patient Interaction", "Disease History"))))))))
  cohort = hr$cohort[1]
  hr <- hr %>% 
    dplyr::select(c("term","hazard_ratio", contains("conf"), contains("se"), "variable")) %>%
    # keep all characters before =
    mutate(pred_name = sub("=.*", "", term))  %>%
    mutate(pred_name = sub("cov_cat_", "", pred_name)) %>%
    mutate(pred_name = sub("sub_cat_", "", pred_name)) %>%
    mutate(pred_name = sub("cov_num_", "", pred_name)) %>%
    # keep all characters after =
    mutate(pred_level = sub(".*=", "", term)) %>%
    mutate(term = sub("cov_cat_", "", term)) %>%
    mutate(term = sub("sub_cat_", "", term)) %>%
    mutate(term = sub("cov_num_", "", term)) %>%
    mutate(term = gsub("_", " ", term)) %>%
    mutate(term = gsub("=TRUE", "", term)) 
  hr <- hr %>% rename(conf.low=robust.conf.low) %>%
    rename(conf.high = robust.conf.high)
  hr <- hr[order(hr$variable, hr$term),]
  hr <- hr %>% mutate('HR (95% CI)' = paste0(format(round(hazard_ratio,2), nsmall = 2), 
                                             " (", format(round(conf.low,2),nsmall=2), ", ", 
                                             format(round(conf.high,2),nsmall=2), ")"))
  
  hr <- hr[order(hr$term),]
  
  pred_name <- unique(hr$pred_name)
  
  pred_count <- rep(NA, length(pred_name))
  index = 1
  for(i in pred_name){
    pred_count[index] <- length(which(hr$pred_name == i))
    index = index + 1
  }
  
  df <- data.frame(pred_name, pred_count)
  df <- df %>% dplyr::filter(pred_count>1) %>%dplyr::select(pred_name)
  a <- data.frame(matrix(nrow = nrow(hr)+nrow(df), ncol=ncol(hr)))
  a[1:nrow(hr),] <- hr
  names(a) <- names(hr)
  for(i in 1:nrow(df)){
    a$term[nrow(hr)+i] <- a$pred_name[nrow(hr)+i] <- df$pred_name[i]
  }
  
  hr <- a
  hr <- hr %>% mutate(term = gsub("_", " ", term))
  
  hr$term <- str_to_sentence(hr$term)
  hr$term <-sub("Bmi", "BMI", hr$term)
  hr$term <-sub("Imd", "IMD", hr$term)
  hr$term <-sub("Sle", "SLE", hr$term)
  
  
  temp <- hr$term[order(hr$term, decreasing = T)]
  
  # hr_fixed <- hr %>% dplyr::select(c(term, variable, pred_name, pred_level, variable))
  # hr_fixed <- hr_fixed[order(hr_fixed$variable, hr_fixed$term),]
  # write.csv(hr_fixed, file=paste0(common_dir,"hr_fixed2.csv"),row.names=F)
  
  ## read in a pre-specified document
  #df <- read.csv(file=paste0(common_dir, "hr_fixed.csv"))
  df <- read.csv(file=paste0(common_dir, csv_hr_order))
  #View(df)
  
  # remove Ie.status if the HR are not from Cox model with vaccination status as a time-dependent variable
  if(cohort != "Vaccination time-dependent"){
    df <- df%>%filter(!grepl("Ie", df$term, fixed = TRUE))
  }
  
  #df$term <-sub("Ie", "ie", df$term)
  # remove covid if the cohort is not infected cohort
  if(cohort != "Post-COVID"){
    df <- df%>%filter(!grepl("Covid", term, fixed = TRUE)) %>%
      filter(!grepl("COVID", term, fixed = TRUE))
  }
  
  a <- merge(df,hr,by="term", all=T)
  
  a <- a[order(a$row.num),]
  
  hr <- a
  
  temp <- hr$term[reorder(hr$term, desc(hr$row.num))]
  
  #hr$row.num = seq(1:nrow(hr))
  hr$term <- sub("Ie.status", "Vaccination status", hr$term)
  hr$term <- sub("Vaccination status=1", "       Vaccinated", hr$term)
  hr$term <- sub("Smoking status=missing", "       Missing (smoking status)", hr$term)
  hr$term <- sub("Age'", "Age spline: the 2nd basis function", hr$term)
  hr$term <- sub("Gp consultation=", "      ", hr$term)
  hr$term <- sub("Sex=f", "       Female", hr$term)
  hr$term <-gsub(".*=","       ",hr$term)
  #hr$term <-gsub("="," ",hr$term)
  hr$term <- str_to_sentence(hr$term)
  hr$term <- sub("Bmi=", "      ", hr$term)
  hr$term <-sub("Bmi", "BMI", hr$term)
  hr$term <-sub("Imd", "IMD", hr$term)
  hr$term <-sub("Sle", "SLE", hr$term)
  #hr$term <-sub("Ra sle psoriasis", "Rheumatoid arthritis, SLE or Psoriasis", hr$term)
  hr$term <- sub("gp", "GP", hr$term)
  hr$term <- sub("Gp", "GP", hr$term)
  hr$term <- sub("Haem cancer", "Haematological cancer", hr$term)
  hr <- hr %>% filter(!is.na(term)) %>%
    filter(!is.na(variable.x))
  hr <- hr %>% rename(variable = variable.x) %>%
    rename(pred_name = pred_name.x) %>%
    rename(pred_level = pred_level.x) %>%
    dplyr::select(c("term", "row.num","variable", "pred_name", "pred_level", "subgroup",
                    "hazard_ratio", "conf.low", "conf.high", "robust.se", "HR (95% CI)"))
  
  # indent each disease history
  index <- which(hr$variable=="Disease History")
  index2 <- which(hr$term[index]=="History of diseases")
  index <- index[-index2]
  hr$term[index] <- paste0("       ", hr$term[index])
  return(hr)
}