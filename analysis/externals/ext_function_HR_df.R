function_HR_df <- function(df_list,csv_index){
  hr <- data.frame(df_list[csv_index])
  hr$variable = NULL
  hr <- hr%>% mutate(variable = ifelse(grepl("age",term), "Demographics",
                                       ifelse(grepl("sex",term), "Demographics",
                                              ifelse(grepl("bmi", term), "Demographics",
                                                     ifelse(grepl("ethnicity", term), "Demographics",
                                                            ifelse(grepl("region", term), "Demographics", 
                                                                   ifelse(grepl("imd", term), "Demographics",
                                                                          ifelse(grepl("gp", term), "GP Consultation Rate", "Disease History"))))))))
  
  hr <- hr %>% 
    dplyr::select(c("term","hazard_ratio", contains("conf"), contains("se"), "variable")) %>%
    # keep all characters before =
    mutate(pred_name = sub("=.*", "", term))  %>%
    mutate(pred_name = sub("cov_cat_", "", pred_name)) %>%
    mutate(pred_name = sub("cov_num_", "", pred_name)) %>%
    # keep all characters after =
    mutate(pred_level = sub(".*=", "", term)) %>%
    mutate(term = sub("cov_cat_", "", term)) %>%
    mutate(term = sub("cov_num_", "", term)) %>%
    mutate(term = gsub("_", " ", term)) %>%
    mutate(term = gsub("=TRUE", "", term)) %>%
    rename(conf.low=robust.conf.low) %>%
    rename(conf.high = robust.conf.high)
  hr <- hr[order(hr$variable, hr$term),]
  hr <- hr %>% mutate('HR (95% CI)' = paste0(format(round(hazard_ratio,3), nsmall = 3), 
                                             " (", format(round(conf.low,3),nsmall=3), ", ", 
                                             format(round(conf.high,3),nsmall=3), ")"))
  
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
  
  #hr_fixed <- hr %>% dplyr::select(c(term, variable, pred_name, pred_level, variable))
  #hr_fixed <- hr_fixed[order(hr_fixed$variable, hr_fixed$term),]
  #write.csv(hr_fixed, file=paste0(common_dir,"hr_fixed.csv"),row.names=F)
  
  ## read in a pre-specified document
  df <- read.csv(file=paste0(common_dir, "hr_fixed.csv"))
  #View(df)
  
  # remove Ie.status if the HR are not from Cox model with vaccination status as a time-dependent variable
  if(!csv_index%in%td_analysis){
    df <- df%>%filter(!grepl("Ie", df$term, fixed = TRUE))
  }
  if(!csv_index%in%infected_cohort){
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
  return(hr)
}
