# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: Produce hazard ratio plot for predictors
# Output:  

library(readr); library(dplyr); library(ggplot2)

fs::dir_create(here::here("output", "review", "model"))

figure_hr_generation <-function(analysis)
{
  selection <- read_csv(file=paste0("output/not_for_review/model/model_selection_",analysis,".csv"))
  which_model <- selection$which_model
  hr <- read_csv(file=paste0("output/review/model/hazard_ratio_estimates_", which_model, "_", analysis, ".csv"))
  hr <- hr %>% dplyr::select(c("term","hazard_ratio", "conf.low", "conf.high")) %>%
    # keep all characters before =
    mutate(pred_name = sub("=.*", "", term))  %>%
    mutate(pred_name = sub("cov_cat_", "", pred_name)) %>%
    mutate(pred_name = sub("cov_num_", "", pred_name)) %>%
    # keep all characters after =
    mutate(pred_level = sub(".*=", "", hr$term)) %>%
    mutate(term = sub("cov_cat_", "", term)) %>%
    mutate(term = sub("cov_num_", "", term)) %>%
    mutate(term = sub("_", " ", term)) 
  
  hr_plot <- ggplot(hr, aes(x = hazard_ratio, y = term)) +        # ggplot2 plot with confidence intervals
    geom_point(shape=15, size = 3, color = "darkblue") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), size=0.5) +
    xlab(label='\nHazard Ratio and 95% Confidence Interval')+
    #
    ylab(label='Predictorss\n') +
    geom_vline(xintercept = 1, linetype = 2, color = "#b16100", size=1)
  hr_plot
  ggsave(file=paste0("output/review/model/figure_hr_",which_model, "_", analysis, ".svg"), 
         plot=hr_plot, width=12, height=10)
}

for(analysis in c("all", "vax_c", "vaccinated", "all_vax_td", "infected")){
  try(figure_hr_generation(analysis))
}
