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

#check that site 44 is not included
soil <- soil[soil$site_id != 44,]
air <- air[air$site_id != 44,]
#note looks like I actually removed it before
#adding to ADC

#subset for winter period



#start with using water year. May need to see if freezing starts in sept
#start with soil

#get water year
soil$leapID <- leap_year(soil$year_st)
soil$wyear <- ifelse( soil$leapID == TRUE & soil$doy_st >= 275, soil$year_st+1,
                      ifelse(soil$leapID == FALSE & soil$doy_st >= 274,soil$year_st+1,soil$year_st))