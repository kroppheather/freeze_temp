library(dplyr)
library(lubridate)
library(ggplot2)


#use archived data on ADC

DIR <- "E:/Google Drive/research/projects/freeze_temp/resource_map_doi_10_18739_A2736M31X/resource_map_doi_10_18739_A2736M31X/data"
#read in data
soil <- read.csv(paste0(DIR,"/soil_temp.csv"), na.strings=c("NA","NaN"))
air <- read.csv(paste0(DIR,"/air_temp.csv"))
siteID <- read.csv(paste0(DIR,"/siteinfo.csv"))
vegeClass <- read.csv(paste0(DIR,"/vege_class.csv"))
vegeID <- read.csv(paste0(DIR,"/vegeID.csv"))

#focus on shallow soils
#at or above 20 cm
soil <- soil[soil$st_depth <= 20,]

#subset for winter period

#start with using water year. May need to see if freezing starts in sept
#start with soil

#extend the water year
#starting on oct 1 means there are some below freezing temps
#so need to start earlier tha traditional water year. Moving to
#sept 1
soil$leapID <- leap_year(soil$year_st)
soil$wyear <- ifelse( soil$leapID == TRUE & soil$doy_st >= 245, 
                      soil$year_st+1,
                      ifelse(soil$leapID == FALSE & soil$doy_st >= 244,
                             soil$year_st+1,
                             soil$year_st))

air$leapID <- leap_year(air$year_ai)
air$wyear <- ifelse( air$leapID == TRUE & air$doy_ai >= 245, 
                      air$year_ai+1,
                      ifelse(air$leapID == FALSE & air$doy_ai >= 244,
                             air$year_ai+1,
                             air$year_ai))
#identify days in study period
#to have the same length of period even during leap year
#end summer period on doy 152 (June 1 on non-leap year and May 31 leap year)
soil$studyPeriod <- ifelse( soil$leapID == TRUE & soil$doy_st >= 245, 
                            1,
                            ifelse(soil$leapID == FALSE & soil$doy_st >= 244,
                                   1,
                            ifelse(soil$doy_st < 152,
                                   1,0)))

#subset so focusing on study period
#and omit an NA
soil <- na.omit(soil[soil$studyPeriod == 1,])
#focus on 1-10 cm in soil for now
soil <- soil[soil$st_depth >= 1 & soil$st_depth <= 10,]


#get total observations by depth, water year, and site
soilCount <- aggregate(soil$soil_t, by=list(
                                       wyear= soil$wyear,
                                       st_depth= soil$st_depth,
                                       site_id= soil$site_id),
                       FUN="length")
#get soil count with full period
#only allow three missing
soilCountA <- soilCount[soilCount$x >= 271,]
#summarize number of sitexdepth with full observations
sitesCount <- aggregate(soilCountA$x, by=list(
                                        st_depth = soilCountA$st_depth,
                                        site_id= soilCountA$site_id),
                        FUN="length")
#see how many sites at least have five years of data
sitesLong <- sitesCount[sitesCount$x >= 5,]
colnames(sitesLong)[3] <- "YearCount"
#match vegetation info
sitesLists1 <- left_join(sitesLong, vegeClass, by="site_id")
#get only sites with criteria of longer obs period back into the list
sitesList <- inner_join(soilCountA, sitesLists1,by=c("site_id","st_depth"))

#subset soil to only contain longer sites
soilW <- inner_join(soil,sitesList, by=c("site_id","st_depth","wyear"))

#make a winter period day count
soilW$dayID <- ifelse( soilW$leapID == TRUE & soilW$doy_st >= 245, 
                       soilW$doy_st-244,
                      ifelse(soilW$leapID == FALSE & soilW$doy_st >= 244,
                             soilW$doy_st-243,
                             soilW$doy_st+122))
#put each sitexdepth into a list for easier reference for plots and calculations
soilWList <- list()
yearsWList <- list()
#decade colors
#1990, 2000, 2010
colsD <- c("#19647e25","#ffc85725","#4b3f7225")

for(i in 1:nrow(sitesLong)){
  soilWList[[i]] <- soilW[soilW$site_id == sitesLong$site_id[i]&soilW$st_depth == sitesLong$st_depth[i],]
  yearsWList[[i]] <- unique(data.frame(wyear=soilWList[[i]]$wyear))
  yearsWList[[i]]$decadeCol <-  ifelse(floor(yearsWList[[i]]/10) == 199,colsD[1],
                                       ifelse(floor(yearsWList[[i]]/10) == 200, colsD[2],
                                              ifelse(floor(yearsWList[[i]]/10) == 201, colsD[3],"#31393c")))
  
  
  }



#start by visualizing all data
plot(c(0),c(0),type="n", col=rgb(0.5,0.5,0.5,0.5),
     xlab="Day",ylim=c(-40,15),xlim=c(0,280),xaxt="n",
     ylab="temperature (c)")
axis(1, c(1,31,62,92,123,154,182,213,244,275),c("Sept","Oct","Nov","Dec","Jan","Feb","Mar","April","May","June"))

for(i in 1:nrow(sitesLong)){
  for(k in 1:length(yearsWList[[i]]$wyear)){
    points(soilWList[[i]]$dayID[soilWList[[i]]$wyear == yearsWList[[i]]$wyear[k]], 
     soilWList[[i]]$soil_t[soilWList[[i]]$wyear == yearsWList[[i]]$wyear[k]], 
     type="l", col=yearsWList[[i]]$decadeCol[k])
  }
}
legend("bottomleft", c("1990s","2000s","2010s"), col=colsD,lwd=2, bty="n")



plot(c(0),c(0),type="n", col=rgb(0.5,0.5,0.5,0.5),
     xlab="Day",ylim=c(-40,15),xlim=c(0,280),xaxt="n",
     ylab="temperature (c)")
axis(1, c(1,31,62,92,123,154,182,213,244,275),c("Sept","Oct","Nov","Dec","Jan","Feb","Mar","April","May","June"))
for(k in 1:length(yearsWList[[1]]$wyear)){
  points(soilWList[[1]]$dayID[soilWList[[1]]$wyear == yearsWList[[1]]$wyear[k]], 
         soilWList[[1]]$soil_t[soilWList[[1]]$wyear == yearsWList[[1]]$wyear[k]], 
         type="l", col=yearsWList[[1]]$decadeCol[k])
}
#look at full data
fullObs <- data.frame(dayID = rep(seq(1, 274), times=length(seq(1991,2017))),
                      wyear = rep(seq(1991,2017), each=length(seq(1, 274))))

fullSoil <- full_join(soilW, fullObs, by=c("dayID","wyear"))

ggplot(data=fullSoil[fullSoil$wyear == 2000,],
       aes(x=site_id, y=dayID,fill=soil_t) )+
  geom_tile()+
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white")
ggplot(data=fullSoil[fullSoil$wyear == 2016,],
       aes(x=site_id, y=dayID,fill=soil_t) )+
  geom_tile()+
  scale_fill_gradient2(name = "Temperature (c)",
                       low = "#2166ac",
                       mid = "#d8daeb",
                       high = "#b2182b",
                       na.value = "white")
#calculate freeze

#freeze thaw cycles
