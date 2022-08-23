# Purpose: Forest plot for hazard ratios with numerical numbers
# Programmed by Yinghui Wei
# 2022-08-23
# https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
library(grid)
library(forestploter)

df <-hr_vax_c_demographics <- hr_vax_c %>% filter(variable == "Demographics")
# Add blank column for the forest plot to display CI.
# Adjust the column width with space. 
df$` ` <- paste(rep(" ", nrow(df)), collapse = " ")
df <- df %>% rename(Characteristic = term)
df[is.na(df$`HR (95% CI)`),11] <- ""
# hr_vax_c_demographics <- hr_vax_c_demographics %>% mutate(empty_column = NA)
svglite::svglite(file = paste0(output_dir, "figures/", "forest_plot_hr_vax_c_demo.svg"), 
                 width = 10,
                 height = 15)
p <- forest(df[,c(1,13,11)],
            est = df$hazard_ratio,
            lower = df$conf.low, 
            upper = df$conf.high,
            sizes = df$robust.se,
            ci_column = 2,
            ref_line = 1,
            arrow_lab = c("Lower risk", "Higher risk"),
            xlim = c(0, ceiling(max(df$conf.high,na.rm =T))),
            ticks_at = c(0,0.5,1, seq(2,ceiling(max(df$conf.high,na.rm =T)),by=2)),
            footnote = "Demographics variables")

# Print plot
p
dev.off()
