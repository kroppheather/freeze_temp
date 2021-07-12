library(dplyr)
library(lubridate)

#use archived data on ADC

DIR <- "E:/Google Drive/research/projects/freeze_temp/resource_map_doi_10_18739_A2736M31X/resource_map_doi_10_18739_A2736M31X/data"
#read in data
soil <- read.csv(paste0(DIR,"/soil_temp.csv"))
air <- read.csv(paste0(DIR,"/air_temp.csv"))
siteID <- read.csv(paste0(DIR,"/siteinfo.csv"))
vegeClass <- read.csv(paste0(DIR,"/vege_class.csv"))
vegeID <- read.csv(paste0(DIR,"/vegeID.csv"))


