# Purpose: Forest plot for hazard ratios with numerical numbers
# Programmed by Yinghui Wei
# 2022-08-23
# https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
library(grid)
library(forestploter)
source("analysis/externals/ext_figure_hazard_ratio_v2.R")


function_HR_plot_num <- function(df, cohort, outputdir){
  # Add blank column for the forest plot to display CI.
  # Adjust the column width with space. 
  df$` ` <- paste(rep(" ", nrow(df)), collapse = " ")
 # df$term[which(df$term == "GP consultation")] = "GP Consultation Rate"
  df <- df %>% rename(Characteristic = term) 
  df[is.na(df$`HR (95% CI)`),11] <- ""
  p <- forest(df[,c(1,13,11)],
              est = df$hazard_ratio,
              lower = df$conf.low, 
              upper = df$conf.high,
              sizes = df$robust.se,
              ci_column = 2,
              ref_line = 1,
              arrow_lab = c("Lower risk", "Higher risk"),
              #xlim = c(0, ceiling(max(df$conf.high,na.rm =T))),
              #ticks_at = c(0,0.5,1, seq(2,ceiling(max(df$conf.high,na.rm =T)),by=2)),
              xlim = c(0, 6),
              ticks_at = c(0,0.5,1, seq(2,6,by=2)),
              footnote = "")
  p
  ggsave(file=paste0("v3_plot_HR_",cohort, ".svg"), path = paste0(output_dir, "figures"),
         plot=p, width=12, height=20)
}

# Pre-vaccination cohort - all characteristics
df <- hr_vax_c
cohort = "vax_c"
function_HR_plot_num(df, cohort, outputdir)

# Pre-vaccination cohort - demographics variables
df <-hr_vax_c_demographics <- hr_vax_c %>% filter(variable == "Demographics")
cohort = "vax_c_demographics_only"
function_HR_plot_num(df, cohort, outputdir)

# Post-vaccination cohort - all characteristics
df <- hr_vaccinated
cohort = "vaccinated"
function_HR_plot_num(df, cohort, outputdir)
