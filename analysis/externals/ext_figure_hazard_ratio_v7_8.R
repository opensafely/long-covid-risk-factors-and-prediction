# Purpose: Forest plot for hazard ratios with numerical numbers
# Programmed by Yinghui Wei
# 2022-08-24
# https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
library(grid)
library(dplyr)
library(forestploter)
source("analysis/externals/ext_figure_hazard_ratio_v2.R")

#################################################################################
## Part 1. Data frame setup                                                    ##
#################################################################################
df <- merge(hr_vax_c, hr_vaccinated, by=c("term","row.num"), all=T)

df <- arrange(df,row.num)

df <- df %>% select(c(term,row.num,variable.x, subgroup.x, hazard_ratio.x, conf.low.x, conf.high.x,
                      "HR (95% CI).x", hazard_ratio.y, conf.low.y, conf.high.y, "HR (95% CI).y")) %>%
  rename(variable = variable.x) %>% rename(subgroup = subgroup.x) %>%
  rename("HR (95% CI)" = "HR (95% CI).x") %>%
  rename("HR (95% CI) " = "HR (95% CI).y")%>%
  rename(Characteristic = term) 


df[is.na(df$`HR (95% CI)`),"HR (95% CI)"] <- ""
df[is.na(df$`HR (95% CI) `),"HR (95% CI) "] <- ""

# Add two blank column for CI
df$`Pre-Vaccination` <- paste(rep(" ", 25), collapse = " ")
df$`Post-Vaccination` <- paste(rep(" ", 25), collapse = " ")
#df$` ` <- paste(rep(" ", nrow(df)), collapse = " ") # add empty space
df$` ` <- paste(rep(" ", 5), collapse = " ")

# Set-up theme
tm2 <- forest_theme(base_size = 10,
                    refline_lty = "solid",
                    ci_pch = c(15,18),
                    xlog = T,
                    ci_col = c("#377eb8", "#4daf4a"),
                    footnote_col = "blue",
                    legend_name = "Cox Model", legend_position = "bottom",
                    legend_value = c("Fully adjusted   ", "Age and sex adjusted"),
                    vertline_lty = c("dashed", "dotted"),
                    vertline_col = c("#d6604d", "#bababa"))


#################################################################################
## Part 2. Version 8                                                         ##
#################################################################################
# Version 8: Demographics one graph; non-demographics another graph
#            with HR and 95%CI for age-sex adjsuted HR and fully adjusted HR on the plot

# Set-up theme
tm2 <- forest_theme(base_size = 10,
                    refline_lty = "solid",
                    ci_pch = c(15,18),
                    xlog = T,
                    ci_col = c("#377eb8", "#4daf4a"),
                    footnote_col = "blue",
                    legend_name = "Cox Model", legend_position = "bottom",
                    legend_value = c("Fully adjusted   ", "Age and sex adjusted"),
                    vertline_lty = c("dashed", "dotted"),
                    vertline_col = c("#d6604d", "#bababa"))


# df <- df %>% filter(variable=="Demographics")
df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

## created for the purpose of plot exercise - to be replaced by the real estimates
df$age_sex_aHR <- df$`HR (95% CI)`
df$age_sex_aHR2 <- df$`HR (95% CI) `


df <- df %>% rename("Fully aHR (95% CI)" = "HR (95% CI)") %>%
  rename("Fully aHR (95% CI) " = "HR (95% CI) ") %>%
  rename("Age and sex aHR (95%)" = age_sex_aHR) %>%
  rename("Age and sex aHR (95%) " = age_sex_aHR2) 

v7_plot <- function(df,var_grp){
  # show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x, and 14 Space for CI, 12 = HR.y
  # 16 age-sex adjusted HR; 17 age-sex adjusted HR
  df <- df %>% filter(variable==var_grp)
  p <- forest(df[,c(1, 13, 8, 16, 15,  14,  12, 17)],  
              est = list(df$hazard_ratio.x,
                         df$hazard_ratio.y),
              lower = list(df$conf.low.x,
                           df$conf.low.y), 
              upper = list(df$conf.high.x,
                           df$conf.high.y),
              ci_column = c(2, 6),
              ref_line = 1,
              vert_line = c(0.5, 2),
              xlim = c(0, 6),
              ticks_at = c(0,1, seq(2,6,by=2)),
              nudge_y = 0.2,
              theme = tm2)
  plot(p)
  cohort = "pre_vax_and_vax"
  ggsave(file=paste0("v7_plot_HR_",cohort, "_", var_grp,".svg"), path = paste0(output_dir, "figures"),
         plot=p, width=30, height=20)
}

v7_plot(df,var_grp="demographics")
v7_plot(df,var_grp="non_demographics")


#################################################################################
## Part 2. Version 8                                                         ##
#################################################################################
# Version 8: Demographics one graph; non-demographics another graph
#            with HR and 95%CI for age-sex adjsuted HR and fully adjusted HR on the plot

v8_plot <- function(df,var_grp){
  # show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x, and 14 Space for CI, 12 = HR.y
  # 16 age-sex adjusted HR; 17 age-sex adjusted HR
  df <- df %>% filter(variable==var_grp)
  p <- forest(df[,c(1, 13, 8, 16, 15,  14,  12, 17)],  
              est = list(df$hazard_ratio.x,
                         df$hazard_ratio.x,
                         df$hazard_ratio.y,
                         df$hazard_ratio.y),
              lower = list(df$conf.low.x,
                           df$conf.low.x,
                           df$conf.low.y,
                           df$conf.low.y), 
              upper = list(df$conf.high.x,
                           df$conf.high.x,
                           df$conf.high.y,
                           df$conf.high.y),
              ci_column = c(2, 6),
              ref_line = 1,
              vert_line = c(0.5, 2),
              xlim = c(0, 6),
              ticks_at = c(0,1, seq(2,6,by=2)),
              nudge_y = 0.2,
              theme = tm2)
  plot(p)
  cohort = "pre_vax_and_vax"
  ggsave(file=paste0("v8_plot_HR_",cohort, "_", var_grp,".svg"), path = paste0(output_dir, "figures"),
         plot=p, width=30, height=20)
}

v8_plot(df,var_grp="demographics")
v8_plot(df,var_grp="non_demographics")