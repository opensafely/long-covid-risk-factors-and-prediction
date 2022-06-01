# Purpose: Long COVID risk factors and prediction models
# Author:  Yinghui Wei
# Content: cumulative probability of long COVID by age and  sex
# Output:  Two KM plots by age and sex
library(readr); library(dplyr);library(survival);library(survminer)
fs::dir_create(here::here("output", "review", "descriptives"))
cohort = "all"
input <- read_rds(paste0("output/input_stage1_", cohort,".rds"))
input <- input %>% select(lcovid_surv, lcovid_cens, contains("sex"), contains("cov_cat_age"))

levels(input$cov_cat_age_group) <- c("18-39", "40-59", "60-79", "80+")
table(input$cov_cat_age_group)
table(input$cov_cat_sex)
levels(input$cov_cat_sex) <- c("Female", "Male")
KM <- survfit(Surv(lcovid_surv, lcovid_cens) ~ cov_cat_age_group, type="kaplan-meier", 
              data = input,
              conf.type="log")
N <- length(unique(input$cov_cat_age_group))

svg("output/review/descriptives/figure_cum_prob_age_sex.svg", width = 9, height = 5,)

ggsurvplot_facet(KM, fun=function(x) { 1- x }, data=input, facet.by = "cov_cat_sex",
                palette = "jco", 
                legend.labs = NULL,
                pval = FALSE, short.panel.labs = TRUE, 
                panel.labs.background = list(color = "gray", fill = "gray"), 
                panel.labs.font = list(size=13),
                legend.title="Age Group") +
  labs(title="",x="\nDays since 1 December 2020", y = "\nCumulative probability of long COVID\n") +
  theme(legend.position="bottom", legend.title=element_text(size=13), 
        legend.text=element_text(size=13),
        axis.text = element_text(size = 13))
dev.off()

# figure_km <- function(time, cens, covariate, fig.title){
#   KM <- survfit(Surv(time, cens) ~ covariate, type="kaplan-meier", 
#                 conf.type="log")
#   N <- length(unique(covariate))
#   plot(KM, fun=function(x) { 1- x }, xlab="Days since 1 December 2020", main = fig.title,
#        ylab="\nCumulative probability of long COVID", 
#        cex.axis = 1.5, cex.lab = 1.5, cex.main=1.5,
#        lty=1:N,lwd=2, col=1:N)
#   legend(
#     "topleft",
#     legend=unique(covariate),
#     lty=1:N, col=1:N,lwd=2,cex = 1.5, 
#     horiz=FALSE,
#     bty='n')
# }

# input_male <- input %>% filter(cov_cat_sex == "M")
# input_female <- input %>% filter(cov_cat_sex == "F")
# svg("output/review/descriptives/km_age_sex.svg", width = 15, height = 9,)
# par(mfrow=c(1,2))
# # Cumulative probability for long COVID - KM plot by age and sex
# figure_km(input_female$lcovid_surv, input_female$lcovid_cens, input_female$cov_cat_age_group, fig.title="Female")
# figure_km(input_male$lcovid_surv, input_male$lcovid_cens, input_male$cov_cat_age_group, fig.title="Male")
# dev.off()


