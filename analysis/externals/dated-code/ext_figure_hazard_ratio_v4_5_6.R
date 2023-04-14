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

#################################################################################
## Part 2. Version 4                                                           ##
#################################################################################

# Version 4: multiple cohorts on one graph, with numerical values displayed for hazard ratios
# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_lty = "solid",
                   ci_pch = 15,
                   ci_col = "#377eb8",
                   footnote_col = "blue",
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"))

# show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x,15=empty space, and 14 Space for CI, 12 = HR.y
p <- forest(df[,c(1, 13, 8, 15, 14, 12)],  
            est = list(df$hazard_ratio.x,
                       df$hazard_ratio.y),
            lower = list(df$conf.low.x,
                         df$conf.low.y), 
            upper = list(df$conf.high.x,
                         df$conf.high.y),
            xlim = c(0, 6),
            ticks_at = c(0.25,1, seq(2,6,by=2)),
            ci_column = c(2, 5),
            ref_line = 1,
            vert_line = c(0.5, 2),
            nudge_y = 0.2,
            theme = tm)

plot(p)

cohort = "pre_vax_and_vax"
ggsave(file=paste0("v4_plot_HR_",cohort, ".svg"), path = paste0(output_dir, "figures"),
       plot=p, width=30, height=20)

#################################################################################
## Part 3. Version 5                                                            ##
#################################################################################
# Version 5 without numerical values
# show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x, and 14 Space for CI, 12 = HR.y
p <- forest(df[,c(1, 13, 15, 14)],  
            est = list(df$hazard_ratio.x,
                       df$hazard_ratio.y),
            lower = list(df$conf.low.x,
                         df$conf.low.y), 
            upper = list(df$conf.high.x,
                         df$conf.high.y),
            ci_column = c(2, 4),
            ref_line = 1,
            vert_line = c(0.5, 2),
            xlim = c(0, 6),
            ticks_at = c(0.25,1, seq(2,6,by=2)),
            nudge_y = 0.2,
            theme = tm)

plot(p)

cohort = "pre_vax_and_vax"
ggsave(file=paste0("v5_plot_HR_",cohort, ".svg"), path = paste0(output_dir, "figures"),
       plot=p, width=30, height=20)

#################################################################################
## Part 4. Version 6                                                           ##
#################################################################################

# Verson 6: Demographics one graph; non-demographics another graph
df_reserve <- df

# df <- df %>% filter(variable=="Demographics")
df <- df %>% mutate(variable = ifelse(variable == "Demographics", "demographics", "non_demographics"))

v6_plot <- function(df,var_grp){
# show in order: 1 Characteristic, 13 Space for CI, 8 = HR.x, and 14 Space for CI, 12 = HR.y
  df <- df %>% filter(variable==var_grp)
  p <- forest(df[,c(1, 13, 15, 14)],  
            est = list(df$hazard_ratio.x,
                       df$hazard_ratio.y),
            lower = list(df$conf.low.x,
                         df$conf.low.y), 
            upper = list(df$conf.high.x,
                         df$conf.high.y),
            ci_column = c(2, 4),
            ref_line = 1,
            vert_line = c(0.5, 2),
            xlim = c(0, 6),
            ticks_at = c(0.25,1, seq(2,6,by=2)),
            nudge_y = 0.2,
            theme = tm)
  plot(p)
  cohort = "pre_vax_and_vax"
  ggsave(file=paste0("v6_plot_HR_",cohort, "_", var_grp,".svg"), path = paste0(output_dir, "figures"),
         plot=p, width=30, height=20)
}

v6_plot(df,var_grp="demographics")
v6_plot(df,var_grp="non_demographics")
