library(sp)
library(raster)
library(rgdal)

#Loading in the stop/frisk dataset, and the census block shapefile:
stops=get(load("/Users/nyu/Desktop/Stop_Frisk/sqf.Rdata"))
setwd("/Users/nyu/Desktop/tabblock2010_36_pophu")
nyc_blocks<- shapefile('tabblock2010_36_pophu.shp')
nyc_blocks$area= area(nyc_blocks)

#Loading the following libraries after finding the area (there are issues with the area command possibly being called from a library different from library(raster):

library(dplyr)
library(rgeos)
library(spatstat)

#Filtering stops by year and removing NAs:
sp2006 <- filter(stops, stops$year==2006)
sp2006 <- filter(sp2006, !is.na(lat))

sp2007 <- filter(stops, stops$year==2007)
sp2007 <- filter(sp2007, !is.na(lat))

sp2008 <- filter(stops, stops$year==2008)
sp2008 <- filter(sp2008, !is.na(lat))

sp2009 <- filter(stops, stops$year==2009)
sp2009 <- filter(sp2009, !is.na(lat))

sp2010 <- filter(stops, stops$year==2010)
sp2010 <- filter(sp2010, !is.na(lat))

sp2011 <- filter(stops, stops$year==2011)
sp2011 <- filter(sp2011, !is.na(lat))

sp2012 <- filter(stops, stops$year==2012)
sp2012 <- filter(sp2012, !is.na(lat))



#Extracting coordinates and making a spdf:

xy2006 <- data.frame(sp2006$lon, sp2006$lat)
spdf2006 <- SpatialPointsDataFrame(xy2006, data=sp2006,  proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

xy2007 <- data.frame(sp2007$lon, sp2007$lat)
spdf2007 <- SpatialPointsDataFrame(xy2007, data=sp2007,  proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

xy2008 <- data.frame(sp2008$lon, sp2008$lat)
spdf2008 <- SpatialPointsDataFrame(xy2008, data=sp2008,  proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

xy2009 <- data.frame(sp2009$lon, sp2009$lat)
spdf2009 <- SpatialPointsDataFrame(xy2009, data=sp2009,  proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

xy2010 <- data.frame(sp2010$lon, sp2010$lat)
spdf2010 <- SpatialPointsDataFrame(xy2010, data=sp2010,  proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

xy2011 <- data.frame(sp2011$lon, sp2011$lat)
spdf2011 <- SpatialPointsDataFrame(xy2011, data=sp2011,  proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

xy2012 <- data.frame(sp2012$lon, sp2012$lat)
spdf2012 <- SpatialPointsDataFrame(xy2012, data=sp2012,  proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))


#Overlaying the spatial point data from the years 2006-2013 onto the census block polygons from the block shapefiles in nyc:
ID2006 <- over(spdf2006 , nyc_blocks[,"BLOCKID10"] )
spdf2006$BLOCKID10=ID2006$BLOCKID10

ID2007 <- over(spdf2007 , nyc_blocks[,"BLOCKID10"] )
spdf2007$BLOCKID10=ID2007$BLOCKID10

ID2008 <- over(spdf2008 , nyc_blocks[,"BLOCKID10"] )
spdf2008$BLOCKID10=ID2008$BLOCKID10

ID2009 <- over(spdf2009 , nyc_blocks[,"BLOCKID10"] )
spdf2009$BLOCKID10=ID2009$BLOCKID10

ID2010 <- over(spdf2010 , nyc_blocks[,"BLOCKID10"] )
spdf2010$BLOCKID10=ID2010$BLOCKID10

ID2011 <- over(spdf2011 , nyc_blocks[,"BLOCKID10"] )
spdf2011$BLOCKID10=ID2011$BLOCKID10

ID2012 <- over(spdf2012 , nyc_blocks[,"BLOCKID10"] )
spdf2012$BLOCKID10=ID2012$BLOCKID10



install.packages("plyr")
library(plyr)

#Merging the two datasets, (2006-2013):
spdf2006@data<-join(spdf2006@data, nyc_blocks@data, by= "BLOCKID10")
spdf2007@data<-join(spdf2007@data, nyc_blocks@data, by= "BLOCKID10")
spdf2008@data<-join(spdf2008@data, nyc_blocks@data, by= "BLOCKID10")
spdf2009@data<-join(spdf2009@data, nyc_blocks@data, by= "BLOCKID10")
spdf2010@data<-join(spdf2010@data, nyc_blocks@data, by= "BLOCKID10")
spdf2011@data<-join(spdf2011@data, nyc_blocks@data, by= "BLOCKID10")
spdf2012@data<-join(spdf2012@data, nyc_blocks@data, by= "BLOCKID10")


detach("package:plyr", unload=TRUE) 

#Aggregated Relative Risk, 2006:
local_intensity2006 <- spdf2006@data %>% group_by(BLOCKID10, spdf2006$area) %>% summarise(n=n()) %>% mutate(area=`spdf2006$area`/1000000)%>% mutate(n/area)
population_density2006= spdf2006@data %>% group_by(BLOCKID10,spdf2006$area,spdf2006$POP10) %>% summarise(n=n()) %>% mutate(areakm2=`spdf2006$area`/1000000) %>% mutate(pop_density=`spdf2006$POP10`/areakm2) 
relative_risk2006= ((local_intensity2006$`n/area`)/(population_density2006$pop_density))
relative_risk2006=relative_risk2006[relative_risk2006!=Inf]

#Block-wise average relative risk for 2006 (0.2882582 incidents per sq km):
mean(relative_risk2006)


#Aggregated Relative Risk, 2007:
local_intensity2007 <- spdf2007@data %>% group_by(BLOCKID10, spdf2007$area) %>% summarise(n=n()) %>% mutate(area=`spdf2007$area`/1000000)%>% mutate(n/area)
population_density2007= spdf2007@data %>% group_by(BLOCKID10,spdf2007$area,spdf2007$POP10) %>% summarise(n=n()) %>% mutate(areakm2=`spdf2007$area`/1000000) %>% mutate(pop_density=`spdf2007$POP10`/areakm2) 
relative_risk2007= ((local_intensity2007$`n/area`)/(population_density2007$pop_density))
relative_risk2007=relative_risk2007[relative_risk2007!=Inf]

#Block-wise average relative risk for 2007 (0.2582691 incidents per sq km):
mean(relative_risk2007)


#Aggregated Relative Risk, 2008:
local_intensity2008 <- spdf2008@data %>% group_by(BLOCKID10, spdf2008$area) %>% summarise(n=n()) %>% mutate(area=`spdf2008$area`/1000000)%>% mutate(n/area)
population_density2008= spdf2008@data %>% group_by(BLOCKID10,spdf2008$area,spdf2008$POP10) %>% summarise(n=n()) %>% mutate(areakm2=`spdf2008$area`/1000000) %>% mutate(pop_density=`spdf2008$POP10`/areakm2) 
relative_risk2008= ((local_intensity2008$`n/area`)/(population_density2008$pop_density))
relative_risk2008=relative_risk2008[relative_risk2008!=Inf]

#Block-wise average relative risk for 2008 (0.2905614 incidents per sq km):
mean(relative_risk2008)


#Aggregated Relative Risk, 2009:
local_intensity2009 <- spdf2009@data %>% group_by(BLOCKID10, spdf2009$area) %>% summarise(n=n()) %>% mutate(area=`spdf2009$area`/1000000)%>% mutate(n/area)
population_density2009= spdf2009@data %>% group_by(BLOCKID10,spdf2009$area,spdf2009$POP10) %>% summarise(n=n()) %>% mutate(areakm2=`spdf2009$area`/1000000) %>% mutate(pop_density=`spdf2009$POP10`/areakm2) 
relative_risk2009= ((local_intensity2009$`n/area`)/(population_density2009$pop_density))
relative_risk2009=relative_risk2009[relative_risk2009!=Inf]

#Block-wise average relative risk for 2009 (0.2859769 incidents per sq km):
mean(relative_risk2009)


#Aggregated Relative Risk, 2010:
local_intensity2010 <- spdf2010@data %>% group_by(BLOCKID10, spdf2010$area) %>% summarise(n=n()) %>% mutate(area=`spdf2010$area`/1000000)%>% mutate(n/area)
population_density2010= spdf2010@data %>% group_by(BLOCKID10,spdf2010$area,spdf2010$POP10) %>% summarise(n=n()) %>% mutate(areakm2=`spdf2010$area`/1000000) %>% mutate(pop_density=`spdf2010$POP10`/areakm2) 
relative_risk2010= ((local_intensity2010$`n/area`)/(population_density2010$pop_density))
relative_risk2010=relative_risk2010[relative_risk2010!=Inf]

#Block-wise average relative risk for 2010 (0.302948 incidents per sq km):
mean(relative_risk2010)


#Aggregated Relative Risk, 2011:
local_intensity2011 <- spdf2011@data %>% group_by(BLOCKID10, spdf2011$area) %>% summarise(n=n()) %>% mutate(area=`spdf2011$area`/1000000)%>% mutate(n/area)
population_density2011= spdf2011@data %>% group_by(BLOCKID10,spdf2011$area,spdf2011$POP10) %>% summarise(n=n()) %>% mutate(areakm2=`spdf2011$area`/1000000) %>% mutate(pop_density=`spdf2011$POP10`/areakm2) 
relative_risk2011= ((local_intensity2011$`n/area`)/(population_density2011$pop_density))
relative_risk2011=relative_risk2011[relative_risk2011!=Inf]

#Block-wise average relative risk for 2011 (0.3452312 incidents per sq km):
mean(relative_risk2011)


#Aggregated Relative Risk, 2012:
local_intensity2012 <- spdf2012@data %>% group_by(BLOCKID10, spdf2012$area) %>% summarise(n=n()) %>% mutate(area=`spdf2012$area`/1000000)%>% mutate(n/area)
population_density2012= spdf2012@data %>% group_by(BLOCKID10,spdf2012$area,spdf2012$POP10) %>% summarise(n=n()) %>% mutate(areakm2=`spdf2012$area`/1000000) %>% mutate(pop_density=`spdf2012$POP10`/areakm2) 
relative_risk2012= (local_intensity2012$`n/area`)/(population_density2012$pop_density)
relative_risk2012=relative_risk2012[relative_risk2012!=Inf]
head(local_intensity2006)

#Block-wise average relative risk for 2012 (0.2883465 incidents per sq km):
mean(relative_risk2012)


#Plotting the average relative risk over the years:
year=c("2006", "2007", "2008", "2009", "2010", "2011", "2012")
relative_risk_allyears= c(mean(relative_risk2006), mean(relative_risk2007), mean(relative_risk2008),mean(relative_risk2009),mean(relative_risk2010),mean(relative_risk2011),mean(relative_risk2012))
plot(relative_risk_allyears, type="l", xlab="Year", , xaxt="n")
axis(1, at=1:7, labels=year) 

#Plotting just the local intensities, unadjusted for population density:
local_intensities_allyears<-c(mean(data.matrix(local_intensity2006)), mean(data.matrix(local_intensity2007)), mean(data.matrix(local_intensity2008)),mean(data.matrix(local_intensity2009)),mean(data.matrix(local_intensity2010)),mean(data.matrix(local_intensity2011)),mean(data.matrix(local_intensity2012)))
plot(local_intensities_allyears, type="l", xlab="Year", , xaxt="n")
axis(1, at=1:7, labels=year) 

#Density plots of relative risk:
relrisk2010_2<-subset(relative_risk2010,relative_risk2010<.4)
relrisk2011_2<-subset(relative_risk2011,relative_risk2011<.4)
relrisk2012_2<-subset(relative_risk2012,relative_risk2012<.4)

plot(density(relrisk2012_2),main="Density of Relative Risk by Year")
lines(density(relrisk2011_2),col=2)
lines(density(relrisk2010_2),col=3)
names <- c(2010,2011,2012)
legend(.3, 15, lty=1, col=c(3,2,1), legend=names)





