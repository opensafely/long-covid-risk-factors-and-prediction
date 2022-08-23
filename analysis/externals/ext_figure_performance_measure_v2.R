library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)
common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"
results_dir = paste0(common_dir, "2022-08-22/")
output_dir <- paste0(common_dir, "2022-08-22/")

file_list=list.files(path = results_dir, pattern = "PM_*")
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(results_dir, file_list[i], sep=''))
  )
}

df_list <- list(PM_full_all_vax_c.csv, 
                PM_full_vaccinated.csv) 

pm <- data.frame(c_stat = numeric(),
                 c_stat_lower = numeric(),
                 c_stat_upper = numeric(),
                 cali_slope = numeric(),
                 cohort = character())


function_extract_c <- function(df_list, csv_index){
  df <- data.frame(df_list[csv_index])
  c_stat <- df$value[1]
  c_stat_lower <- df$value[2]
  c_stat_upper <- df$value[3]
  calibration_slope <- df$value[4]
  return(c(c_stat, c_stat_lower, c_stat_upper, calibration_slope))
}

for(csv_index in 1:2){
  pm[csv_index,1:4] <- function_extract_c(df_list,csv_index)
  pm$cohort[csv_index] = gsub("PM_","",noquote(file_list[csv_index]))
  pm$cohort[csv_index] = gsub(".csv","",pm$cohort[csv_index])
}
pm$cohort[1]="Pre-Vaccination"
pm$cohort[2]="Post-Vaccination"
pm <- pm%>%rename(Cohort = cohort)

df <-pm
df$` ` <- paste(rep("            ", nrow(df)), collapse = " ")
df$`C-Statistic (95% CI)` <- paste0(format(round(df$c_stat,3),nsmall=3), " (", 
                                    format(round(df$c_stat_lower,3),nsmall=3),", ", 
                                    format(round(df$c_stat_upper,3),nsmall=3), ")")
df <- df %>% rename('Calibration Slope' = "cali_slope")
tm <- forest_theme(base_size = 10,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 18,
                   ci_col = "#762a83",
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")
p <- forest(df[,c(5,4,6,7)],
            est = df$c_stat,
            lower = df$c_stat_lower, 
            upper = df$c_stat_upper,
            ci_column = 3,
            ref_line = 20,
            xlim = c(0.5, 1),
            ticks_at = seq(0.5,1,by=0.25),
            footnote = "",
            theme = tm)
p
ggsave(file=paste0("v2_plot_pm", ".svg"), path = paste0(output_dir, "figures"),
       plot=p, width=12, height=20)
