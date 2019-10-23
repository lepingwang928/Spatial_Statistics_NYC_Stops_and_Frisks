library(spatstat)
library(dplyr)
library(rgdal)
library(maptools)
library(smacpod)

#K Functions to test CSR for 2010, 2011 and 2012 on NYC tracts: 

#Reading in the complete merged datasets :
setwd("/Users/nyu/Desktop/")
sp2010<- read.table("OverallStopsPop2010.csv", header=TRUE, sep=",")
sp2011<- read.table("OverallStopsPop2011.csv", header=TRUE, sep=",")
sp2012<- read.table("OverallStopsPop2012.csv", header=TRUE, sep=",")


#Filtering out missing levels for tract ids:
sp2010<- sp2010 %>% filter(!is.na(GEOID10))
sp2011<- sp2011 %>% filter(!is.na(GEOID10))
sp2012<- sp2012 %>% filter(!is.na(GEOID10))


#Setting up the rectangular window for the three years:
mn2010 <- apply(sp2010[, c("lon.x", "lat.x")], 2, min)
mx2010 <- apply(sp2010[, c("lon.x", "lat.x")], 2, max)

mn2011 <- apply(sp2011[, c("lon.x", "lat.x")], 2, min)
mx2011 <- apply(sp2011[, c("lon.x", "lat.x")], 2, max)

mn2012 <- apply(sp2012[, c("lon.x", "lat.x")], 2, min)
mx2012 <- apply(sp2012[, c("lon.x", "lat.x")], 2, max)





#Running the K function tests:
p2010 <- ppp(sp2010$lon.x, sp2010$lat.x, window=owin(c(mn2010[1], mx2010[1]), c(mn2010[2], mx2010[2])))
E.square2010 <- envelope(p2010, Kest, nsim = 20)
plot(E.square2010)

p2011 <- ppp(sp2011$lon.x, sp2011$lat.x, window=owin(c(mn2011[1], mx2011[1]), c(mn2011[2], mx2011[2])))
E.square2011 <- envelope(p2011, Kest, nsim = 20)
plot(E.square2011)

p2012 <- ppp(sp2012$lon.x, sp2012$lat.x, window=owin(c(mn2012[1], mx2012[1]), c(mn2012[2], mx2012[2])))
E.square2012 <- envelope(p2012, Kest, nsim = 20)
plot(E.square2012)
