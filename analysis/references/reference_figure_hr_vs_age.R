library(survival)
library(splines)
library(ggplot2)
# https://stackoverflow.com/questions/43792902/hazard-ratio-plot-with-confidence-waist-in-ggplot2
# colon cancer death dataset
ccd <- na.omit(subset(colon, etype == 2))

library(rms)
dd <- datadist(ccd)
dd$limits$age[2] <- 50
options(datadist = "dd")

cph <- cph(Surv(time, status) ~ rx + sex + rcs(age, c(20, 50, 70)), data = ccd, x = TRUE, y = TRUE)

pdata <- Predict(cph, age, ref.zero = TRUE, fun = exp)

ggplot(data = pdata) +
  
  geom_hline(aes(yintercept = 1), linetype = 3) +
  
  labs(x = "Age at baseline, years",
       y = "Hazard Ratio (95% CI) vs. 50 years",
       title = "Mortality hazard ratio as a function of age",
       subtitle = "Natural spline: knots at 20, 50, and 70 years")
