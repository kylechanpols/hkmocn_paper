library(sf)
library(readr)
library(rgdal)
library(tidyverse)
library(tmap)
setwd("F://gdelt")

protest <- read_csv("cnhkmo_protest_data_REG_YEAR.csv")


china <- readOGR( 
  dsn= "F://gdelt//CN province shapes", 
  layer="gadm36_CHN_1",
  verbose=FALSE
)

hk<- readOGR( 
  dsn= "F://gdelt//HK shape", 
  layer="gadm36_HKG_0",
  verbose=FALSE
)

mo <- readOGR( 
  dsn= "F://gdelt//MO Shape", 
  layer="gadm36_MAC_0",
  verbose=FALSE
)



protest_agg <- protest %>% dplyr::filter(year >= 1997) %>% group_by(region) %>% summarise(n_protest = sum(n_protest))

colnames(protest_agg)[1] <- "NAME_1"

china@data <- left_join(china@data, protest_agg, by="NAME_1")
china$id <- as.numeric(rownames(china@data))

hk@data$protest <- protest_agg %>% filter(NAME_1 == "Hong Kong") %>% select(n_protest) %>% as.numeric
mo@data$protest <- protest_agg %>% filter(NAME_1 == "Macau") %>% select(n_protest) %>% as.numeric

# How to plot the small SARs?
#1) Rbind the sPDFs

brk <- seq(0,1500,length.out = 5)

legend.map <- tm_shape(china)+
  tm_fill(col ="n_protest", style="fixed", palette="Oranges", breaks = brk)+
  tm_layout(legend.only= TRUE)

CN_map <- tm_shape(china)+
  tm_borders()+
  tm_fill(col="n_protest", style="fixed", palette="Oranges", breaks = brk)+
  tm_layout(legend.show = FALSE, title="China")

HK_map <- tm_shape(hk)+
  tm_borders()+
  tm_fill(col="protest", style="fixed", palette="Oranges", breaks = brk)+
  tm_layout(legend.show = FALSE, title="Hong Kong")

MO_map <-  tm_shape(mo)+
  tm_borders()+
  tm_fill(col="protest", style="fixed", palette="Oranges", breaks = brk)+
  tm_layout(legend.show = FALSE, title="Macau")

tmap_arrange(CN_map, HK_map, MO_map, legend.map)
