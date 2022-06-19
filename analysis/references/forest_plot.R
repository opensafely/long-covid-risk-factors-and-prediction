#https://nightingalehealth.github.io/ggforestplot/articles/ggforestplot.html

# example 1

#devtools::install_github("NightingaleHealth/ggforestplot")
# Load and attach the package
library(ggforestplot)

# Load and attach other useful packages
# install.packages("tidyverse")
library(tidyverse)

# Filter only associations to BMI for the first 30 biomarkers of the example
# dataset
df <-
  ggforestplot::df_linear_associations %>%
  filter(
    trait == "BMI",
    dplyr::row_number() <= 30
  )

# Draw a forestplot of cross-sectional, linear associations
ggforestplot::forestplot(
  df = df,
  name = name,
  estimate = beta,
  se = se
)


# Example 2
install.packages("magrittr")
library(magrittr)

# Linear associations
# Get subset of example data frame
df_linear <-
  df_linear_associations %>%
  dplyr::arrange(name) %>%
  dplyr::filter(dplyr::row_number() <= 30)

# Forestplot
forestplot(
  df = df_linear,
  estimate = beta,
  logodds = FALSE,
  colour = trait,
  xlab = "1-SD increment in cardiometabolic trait
  per 1-SD increment in biomarker concentration"
)

