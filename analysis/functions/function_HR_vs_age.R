function_figure_hr_vs_age <- function(input, fit_cox_model_splines, analysis){
  dd <- datadist(input)
  dd$limits$cov_num_age[2] <- 55
  options(datadist = "dd")
  pdata <- Predict(fit_cox_model_splines, cov_num_age, ref.zero = TRUE, fun = exp)
  p <- ggplot(data = pdata)  +
    geom_hline(aes(yintercept = 1), linetype = 3) +
    xlab("\nAge in years\n") + ylab(label = "\nHazard ratio (95% CI) compared to 55 years\n") 
  
  p_hr_vs_age <- p + coord_trans(y = "log10") + 
    scale_x_continuous(breaks = seq(20, 100, by = 20),limits=c(18,100)) +
    theme(axis.text = element_text(size =20),
          axis.title = element_text(size =20),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  
  ggsave(file=paste0("output/review/model/plot_HR_vs_age_",analysis, ".svg"),
         plot=p_hr_vs_age, width=15, height=10)
}
