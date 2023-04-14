library(readr); library(dplyr); library(tidyverse); library(ggplot2); library(data.table)
library(stringr); library(grid); library(forestploter)
common_dir = "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/"
results_dir = paste0(common_dir, "2022-11-02/")
output_dir <- paste0(common_dir, "2022-11-02/")

#-------------------------------------------------------------------------------------------------------------------
#- read in data
PM <- read.csv(paste0(results_dir, "PM_combined.csv"))
#- separate out data - full model with age categorical (comments: should find out with age as spline)
PM_full_all.csv        <- PM[PM$file =="full_model_categorical_all",]           # primary
PM_full_all_vax_c.csv  <- PM[PM$file =="full_model_categorical_all_vax_c",]     # pre-vax
PM_full_vaccinated.csv <- PM[PM$file =="full_model_categorical_vaccinated",]    # post-vax                                                # post-vax
PM_full_infected.csv   <- PM[PM$file =="full_model_categorical_infected",]      # post-covid
PM_full_all_vax_td.csv <- PM[PM$file =="full_model_categorical_all_vax_td",]    # vaccination time-dependent

#- separate out data - age sex model with age spline
PM_age_sex_model_all.csv        <- PM[PM$file =="age_sex_model_all",]                # primary
PM_age_sex_model_all_vax_c.csv  <- PM[PM$file =="age_sex_model_all_vax_c",]    # pre-vax
PM_age_sex_model_vaccinated.csv <- PM[PM$file =="age_sex_model_vaccinated",]  # post-vax                                                # post-vax
PM_age_sex_model_infected.csv   <- PM[PM$file =="age_sex_model_infected",]      # post-covid
PM_age_sex_model_all_vax_td.csv <- PM[PM$file =="age_sex_model_all_vax_td",]      # vaccination time-dependent

df_list <- list(PM_full_all.csv,          # primary
                PM_full_all_vax_c.csv,    # pre-vaccination
                PM_full_vaccinated.csv,   # vaccinated
                PM_full_infected.csv,     # post-covid
                PM_full_all_vax_td.csv,   # primary - vax time dependent
                PM_age_sex_model_all.csv, # primary, age-sex adjusted
                PM_age_sex_model_all_vax_c.csv,  # pre-vaccination, age-sex adjusted
                PM_age_sex_model_vaccinated.csv, # post-vaccination, age-sex adjusted
                PM_age_sex_model_infected.csv,   # post-covid, age-sex adjusted
                PM_age_sex_model_all_vax_td.csv  # primary, vaccination time-dependent
                ) 

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

for(csv_index in 1:10){
  pm[csv_index,1:4] <- function_extract_c(df_list,csv_index)
  pm$cohort[csv_index] = gsub("PM_","",noquote(file_list[csv_index]))
  pm$cohort[csv_index] = gsub(".csv","",pm$cohort[csv_index])
}
# pm$cohort[1]="Primary"
# pm$cohort[2]="Pre-Vaccination"
# pm$cohort[3]="Post-Vaccination"
# pm$cohort[4]="Post-COVID"
# pm$cohort[5]="Vaccination time-dependent"

pm$cohort = rep(c("Primary", "Pre-Vaccination", "Post-Vaccination", "Post-COVID", "Vaccination time-dependent") ,2)
pm <- pm%>%rename(Cohort = cohort)
pm$model = c(rep("full", 5), rep("as", 5))
pm <- reshape(pm, idvar = "Cohort", timevar = "model", direction = "wide")
df <-pm
df$` ` <- paste(rep("            ", nrow(df)), collapse = " ")
dec_pl = 3
df$`C-Statistic from \n the full model` <- paste0(format(round(df$c_stat.full,dec_pl),nsmall=dec_pl), " (", 
                                     format(round(df$c_stat_lower.full,dec_pl),nsmall=dec_pl),", ", 
                                     format(round(df$c_stat_upper.full,dec_pl),nsmall=dec_pl), ")")
df$`C-Statistic from \n age and sex model` <- paste0(format(round(df$c_stat.as,dec_pl),nsmall=dec_pl), " (", 
                                    format(round(df$c_stat_lower.as,dec_pl),nsmall=dec_pl),", ", 
                                    format(round(df$c_stat_upper.as,dec_pl),nsmall=dec_pl), ")")
#df <- df %>% rename('Calibration Slope' = "cali_slope")
#pm$model = c(rep("Full model", 5), rep("Age and sex model", 5))

tm2 <- forest_theme(base_size = 10,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#377eb8", "#4daf4a"),
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   footnote_col = "blue",
                   legend_name = "Model   ",
                   legend_position = "bottom",
                   legend_value = c("  Full model   ", "  Age and sex model                        "),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"))

# tm <- forest_theme(base_size = 10,
#                    # Confidence interval point shape, line type/color/width
#                    ci_pch = 18,
#                    ci_col = "#762a83",
#                    ci_lty = 1,
#                    ci_lwd = 1.5,
#                    ci_Theight = 0.2, # Set an T end at the end of CI 
#                    # Reference line width/type/color
#                    refline_lwd = 1,
#                    refline_lty = "dashed",
#                    refline_col = "grey20",
#                    # Vertical line width/type/color
#                    vertline_lwd = 1,
#                    vertline_lty = "dashed",
#                    vertline_col = "grey20",
#                    # Change summary color for filling and borders
#                    summary_fill = "#4575b4",
#                    summary_col = "#4575b4",
#                    # Footnote font size/face/color
#                    footnote_cex = 0.6,
#                    footnote_fontface = "italic",
#                    footnote_col = "blue")

# 1- cohort; 10 - space; 11 - fully adjusted C-statistic; 12 - Age-sex adjusted C-statistic

p <- forest(df[,c(1, 10, 11, 12)],
            est = list(df$c_stat.full,
                       df$c_stat.as),
            lower = list(df$c_stat_lower.full,
                         df$c_stat_lower.as), 
            upper = list(df$c_stat_upper.full,
                         df$c_stat_upper.as),
            ci_column = 2,
            ref_line = 20,
            xlim = c(0.5, 0.8),
            ticks_at = seq(0.5,0.8,by=0.1),
            # xlim = c(0.5, 1),
            # ticks_at = seq(0.5,1,by=0.25),
            theme = tm2)
p

ggsave(file=paste0("v4_plot_pm", ".svg"), path = paste0(output_dir, "figures"),
       plot=p, width=12, height=20)
