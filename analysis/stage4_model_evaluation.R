# Purpose: Long COVID risk factors and prediction models
# Content: Cox model: model evaluation
# Output:  One CSV file, One HTML file, for apparent performance measures
#          SVG files: survival plots and calibration plot

library(readr); library(dplyr); library(rms); library(MASS)
library(rms); library(fastDummies); library(pseudo)
# library(survcomp) ## not yet available in opensafely

################################################################################
# Part 1: load fitted model                                                    #
################################################################################
# load data, with defined weight, and import formula for survival analysis 
source("analysis/stage3_model_selection.R")

print("Starting stage_4_model_evaluation.R")

fs::dir_create(here::here("output", "review", "model"))

if(which_model == "selected"){
  # loading the selected model as fit_cox_model_vs from backward elimination is not a standard Cox model object
  fit_cox_model <-rms::cph(formula= as.formula(surv_formula),
                           data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
}
#the full model is already loaded in stage3_model_input_set_up, so no need to refit

print("Part 1. Finished loading fitted cox model!")

################################################################################
# Part 2: Output apparent model evaluation - C and calibration slope           #
################################################################################
## fit_cox_model is the full model
# source file for model evaluation
source("analysis/functions/function_model_evaluation.R")
subset_vars =""
#which_model = "full" # reset the model to full, evaluate full model
print(fit_cox_model)
print(fit_cox_model_selected)
function_model_evaluation(input,fit_cox_model, which_model, analysis, subset_vars, graphics_output = TRUE, save_output = TRUE)
print("Finished model valuation")
# 
# ################################################################################
# # Part 3: Calibration plot: adapted from iecv script                            #
# ################################################################################
# if(analysis == "all_vax_td"){
#   input$patient_id <- seq.int(nrow(input))  # reset patient id as they are not unique in this case
# }
# #
# splines_for_age = grepl("rms::rcs", surv_formula)
# # names of covariates and factor levels
# covariates <- names(fit_cox_model$coefficients)
# 
# #pred_level = sub(".*=", "", covariates)  # keep all characters after =
# pred_level = sapply(strsplit(covariates, '='), `[`, 2)
# pred_names = sub("=.*", "", covariates)  # keep all characters before =
# pred_name_level <- paste0(pred_names, "_", pred_level)
# pred_name_level = gsub("_NA", "", pred_name_level)
# 
# predictors <- data.frame(pred_name_level, fit_cox_model$coefficients)
# covariates <- unique(pred_names)
# factor_covars <- covariates[grepl("cat", covariates)]
# #input <- input %>% dplyr::select(-all_of(covariates))
# input_select <- dummy_cols(input, select_columns = factor_covars)
# start = ncol(input)+1
# #names(input_select)[start:ncol(input_select)]
# print("extract covariate name if splines are selected for age!")
# if(splines_for_age == TRUE){
#   x1 = input_select$cov_num_age
#   r1 = rcs(x1,parms=knot_placement)
#   a <- data.matrix(r1)
#   a1 <- a[,1]
#   a2 <- a[,2]
#   a <- data.frame(a1,a2)
#   names(a) <- c("rcs1","rcs2")
#   input_select$cov_num_age_rcs2 = c(a[,2])
#   rm(a)
#   names(input_select)[grep("cov_num_age_rcs2", names(input_select))] = "cov_num_age'"
#   print("Covariate names for age (splines) have been extracted successfully!")
# }
# input_select <- input_select %>% dplyr::select(c(patient_id, all_of(pred_name_level)))
# 
# for(j in pred_name_level){
#   print(j)
#   row.index = which(pred_name_level ==j)
#   input_select[,j] = input_select[,j]*predictors$fit_cox_model.coefficients[row.index]
# }
# cov_cols <- names(input_select)[grep("cov", names(input_select))]
# 
# # Here the linear predictor is calculated for the testing data, using the linear combination of covariates
# if(length(cov_cols)>1){
#   input_select <- input_select %>% mutate(lin_pred = rowSums(.[ , cov_cols])) %>%
#     dplyr::select(c(patient_id,lin_pred))
# }
# if(length(cov_cols)==1){
#   input_select <- input_select %>% mutate(lin_pred = input_select[,cov_cols]) %>%
#     dplyr::select(c(patient_id,lin_pred))
# }
# 
# ## left join: keep all observations in input_select
# input <- merge(x = input_select, y = input, by = "patient_id", all.x = TRUE)
# # Cox model for testing data
# test_cox_model <- cph(Surv(lcovid_surv,lcovid_cens)~lin_pred,data = input, method="breslow")
# 
# # Calculate the C-statistic for the discrimination of the model in the validation dataset
# # Harrell's C-statistic for C_k: data from region k
# c_stat = round(concordance(test_cox_model)$concordance,3)
# c_stat_lower = round(concordance(test_cox_model)$concordance - 1.96*sqrt((concordance(test_cox_model))$var),3)
# c_stat_upper = round(concordance(test_cox_model)$concordance + 1.96*sqrt((concordance(test_cox_model))$var),3)
# c_stat_var = round((concordance(test_cox_model))$var,6)
# 
# print("Calculation for the C-statistic is completed!")
# 
# # Calibration slope
# cal_slope = round(test_cox_model$coef,3)
# # Calibration plot for the validation data
# # Calculate predicted survival probability at 1 year
# time_point = 365
# y1_cox <- summary(survfit(fit_cox_model),time=time_point)$surv
# y1_cox
# 
# pred_surv_prob = y1_cox^exp(input_select$lin_pred)
# 
# pred_risk = 1 - pred_surv_prob
# 
# val_ests <- val.surv(est.surv = pred_surv_prob,
#                      S = Surv(input$lcovid_surv,input$lcovid_cens),
#                      u=time_point,fun=function(p)log(-log(p)),pred = sort(runif(100, 0, 1)))
# print("val_ests is now specified!")
# 
# ###Calibration plot with pseudos
# #Calculate pseudo values using pseudo package
# pseudovalues<- pseudosurv(input$lcovid_surv, input$lcovid_cens, tmax = 365)
# pseudos<- data.frame(pseudo = pseudovalues$pseudo, risk = pred_risk)
# pseudos_sorted<- arrange(pseudos, risk) #Sort by risk
# 
# #Smooth pseudo values using weighted local regression (LOESS)
# loess_pseudo<- predict(loess(pseudo ~ risk, data = pseudos_sorted,
#                              degree = 1, #Fit polynomial of degree 1 (linear) between groups
#                              span = 0.3 #Proportion of closest points to use in fit (span*number of obs)
# ),
# se = T)
# 
# #PLOT
# #Setting up
# axislim<- 1
# spike_bounds <- c(-0.05, 0)
# bin_breaks <- seq(0, axislim, length.out = 100 + 1)
# freqs <- table(cut(pseudos_sorted$risk, breaks = bin_breaks))
# bins <- bin_breaks[-1]
# freqs_valid <- freqs[freqs > 0]
# freqs_rescaled <- spike_bounds[1] + (spike_bounds[2] - spike_bounds[1]) *
#   (freqs_valid - min(freqs_valid)) / (max(freqs_valid) - min(freqs_valid))
# 
# # save the calibration plot
# svg(paste0("output/review/model/calibration_plot_", analysis, ".svg"))
# #Start with a blank plot
# plot(x = pseudos_sorted$risk, y = pseudos_sorted$pseudovalue,
#      xlim = c(0, axislim), ylim = c(spike_bounds[1], axislim), #Lower y axis must leave space for histogram
#      axes = F, xaxs="i",
#      xlab = "Predicted risks", ylab = "Observed outcome proprtion",
#      frame.plot = FALSE, type = "n")
# axis(side = 1, at = seq(0, axislim, by=0.1))
# axis(side = 2, at = seq(0, axislim, by=0.1))
# #Add confidence intervals
# polygon(x = c(pseudos_sorted$risk, rev(pseudos_sorted$risk)),
#         y = c(pmax(loess_pseudo$fit - qt(p = 0.975, df = loess_pseudo$df) * loess_pseudo$se.fit, 0), #Stop confidence interval dipping below 0
#               pmax(rev(loess_pseudo$fit + qt(p = 0.975, df = loess_pseudo$df) * loess_pseudo$se.fit),1)),
#         col = "lightgrey")
# #Add diagonal line for reference of perfect calibration
# abline(a=0, b=1, col="red", lty=2)
# #Add line of calibration
# lines(x = pseudos_sorted$risk, y = loess_pseudo$fit,
#       col = "black", lwd = 2)
# #Add histogram of predicted risk underneath x axis
# segments(x0 = bins[freqs > 0], x1 = bins[freqs > 0],
#          y0 = spike_bounds[1], y1 = freqs_rescaled)
# 
# dev.off()
# print("Calibration plot is created successfully!")
# 
# svg(paste0("output/review/model/histogram_risks", analysis, ".svg"))
# 
# ################################################################################
# # Part 4: histogram of risks: adapted from iecv script                          #
# ################################################################################
# 
# 
# risk <- c(pseudos_sorted$risk, 1-loess_pseudo$fit)
# name_risk <- c(rep("Rredicted risk",length(pseudos_sorted$risk)),
#                rep("Observed risk",length(loess_pseudo$fit)))
# df_risk <- data.frame(risk, name_risk)
# # color <- c(rep("grey",length(pseudos_sorted$risk)),
# #            rep("black",length(loess_pseudo$fit)))
# #
# # df_risk$color <- factor(df_risk$color)
# df_risk$name_risk <- factor(df_risk$name_risk)
# # Map smoke to fill, make the bars NOT stacked, and make them semitransparent
# 
# # YW: have to check if loess_pseudo$fit should be 1-loess_pseudo$fit
# figure_hist <- ggplot(df_risk, aes(x=risk, fill=name_risk)) +
#   geom_histogram(position="identity", alpha=0.4, bins = 30) +
#   labs(title="",x="Risk of long COVID", y = "Count")
# figure_hist
# bin_count <- ggplot_build(figure_hist)$data[[1]]$count
# bin_count <- data.frame(seq(1:length(bin_count)), bin_count)
# names(bin_count) <- c("bin", "count")
# ggsave(file=paste0("output/review/model/risk_histogram_", analysis, ".svg"),
#        plot=figure_hist, width=16, height=8)
# write.csv(bin_count, file=paste0("output/review/descriptives/risk_hist_bin_count_", analysis,".csv"), row.names = F)
# outfile=paste0("risk_hist_bin_count_", analysis)
# rmarkdown::render("analysis/compilation/compiled_hist_bin_count.Rmd",
#                   output_file=outfile,output_dir="output/review/descriptives")
