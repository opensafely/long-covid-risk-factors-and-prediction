# Purpose: to create a map for long covid cases by region as of 31 March 2022

library(tidyverse)
library(ggplot2)
library(sf)
library(data.table)
library(dtplyr)

data_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/tables/"
data_file <- paste0(data_dir, "long_covid_cases.csv")
output_dir <- "C:/Users/yingh_/University of Bristol/grp-EHR - Documents/Projects/long-covid-risk-factors/OS-outputs/2022-06-06/figures/"
data <- read.csv(data_file)

## Load shapefiles
nuts_shp<-st_read("package_map/NUTS_Level_1_(January_2018)_Boundaries.shp")
saveRDS(nuts_shp,here::here("output", "nuts_shp.rds"))
nuts_shp<-readRDS(here::here("output", "nuts_shp.rds"))

long_covid_map<-nuts_shp %>%
  filter(nuts118nm!="Wales" & nuts118nm!="Northern Ireland" & nuts118nm!="Scotland") %>%
  left_join(data,by="nuts118cd") %>%
  ggplot(aes(geometry = geometry,fill=long_covid_percent)) +
  geom_sf(lwd = .8, colour='black') +
  geom_sf_label(aes(label = paste0(round(long_covid_cases/1000000,1),"M")),
                label.size = 0.1,
                label.r = unit(0.5, "lines"),
                fun.geometry = st_centroid,
                show.legend = F) +
  scale_fill_gradient2(limits=c(0,100), breaks = c(0,sort(round(data$long_covid_percent,0)),100),midpoint = 50, high = "navyblue",
                       mid = "indianred", low = "ivory1",na.value = "white") +
  theme(legend.position = c(0.2,0.5),legend.text.align = 1,
        panel.background=element_rect(fill="white")) + 
  ggtitle("Long COVID cases by region") +
  guides(fill=guide_legend(title="Long COVID\npercent (%)")) + 
  xlab("") + ylab("")

long_covid_map

ggsave(file=paste0("long_covid_map", ".svg"), path = output_dir,
       plot=long_covid_map, width=10, height=12)
