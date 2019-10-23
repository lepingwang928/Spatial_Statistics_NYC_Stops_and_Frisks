#### The everything code ####
## You'll need the following files to run this:
##   - "NYC_CT_2010_Pop" (dbf/cpg/prj/sbn/sbx/shp/shx/xml): census tracts shapefile
##   - "NYC PUMAs" (dbf/prj/shp/shx): census PUMAs shapefile
##   - "CenPop2010_Mean_TR36.txt": census tracts data with population and coordinates
##   - "Stops Population2010/11/12.csv": stop-and-frisk data with census tracts and population mapped to each stop
##   - "NYPD_Complaint_Data_Historic.csv": NYPD complaints data for 2018 (to mid-October 2018)
##   - "2010_Census_Tract_to_2010_PUMA.txt": key mapping census tract to PUMA
## Optional: "tracts_allyears.csv", this data frame gets created in this code but the loop takes a long time to run

#### Data import, processing ####
## Change this filepath to your working directory
filepath = "C:/Users/Mac Tan/Desktop/Mac's Stuff/Notes/NYU/18F/APSTA 2015/Project/NYC_Stop_Frisk"
## Shapefiles
# Census tracts
NYC_tracts <- readOGR(dsn = filepath, layer = "NYC_CT_2010_Pop")
# Public Use Microdata Areas (PUMAs)
NYC_PUMAs <- readOGR(dsn = filepath, layer = "NYC PUMAs")

## Census tracts
centroids <- read.table("CenPop2010_Mean_TR36.txt", sep = ",", header = TRUE, colClasses = "character") %>%
  mutate(TRACTID = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
  mutate_at(vars(c("LATITUDE", "LONGITUDE", "POPULATION")), as.numeric) %>%
  dplyr::select(TRACTID, POPULATION, LATITUDE, LONGITUDE)


## Stops mapped to census tracts
stops_pop_2010 <- read.csv("Stops Population2010.csv", stringsAsFactors = FALSE) %>%
  mutate(GEOID10 = as.character(GEOID10))
stops_pop_2011 <- read.csv("Stops Population2011.csv", stringsAsFactors = FALSE) %>%
  mutate(GEOID10 = as.character(GEOID10))
stops_pop_2012 <- read.csv("Stops Population2012.csv", stringsAsFactors = FALSE) %>%
  mutate(GEOID10 = as.character(GEOID10))

## Collapse stops by census tract (nearest centroid)
tracts_2010 <- stops_pop_2010 %>%
  group_by(GEOID10) %>%
  summarise(stops2010 = n(),
            pop2010 = mean(D001)) %>%
  ungroup()

tracts_2011 <- stops_pop_2011 %>%
  group_by(GEOID10) %>%
  summarise(stops2011 = n(),
            pop2011 = mean(D001)) %>%
  ungroup()

tracts_2012 <- stops_pop_2012 %>%
  group_by(GEOID10) %>%
  summarise(stops2012 = n(),
            pop2012 = mean(D001)) %>%
  ungroup()

## Obtain tract IDs
NYC_tractids <- NYC_tracts@data %>%
  dplyr::select(GEOID10) %>%
  mutate(GEOID10 = as.character(GEOID10))

## Crime rates (2018 complaints data)
complaints <- read.csv("NYPD_Complaint_Data_Historic.csv", stringsAsFactors = TRUE) %>%
  dplyr::select(Latitude, Longitude) %>%
  na.omit()

## Map complaint to nearest tract centroid (this takes a while to run, so you can skip ahead by reading in the
## "tracts_allyears.csv" file attached)
complaints$nearest_idx <- NA
for(i in 1:nrow(complaints)) {
  distances <- sqrt((complaints$Latitude[i] - centroids$LATITUDE)^2 + 
                      (complaints$Longitude[i] - centroids$LONGITUDE)^2)
  idx <- which.min(distances)
  complaints$nearest_idx[i] <- idx
}
complaints$nearest_tract <- centroids$TRACTID[complaints$nearest_idx]

complaints <- complaints %>% # collapse crimes by census tract
  group_by(nearest_tract) %>%
  summarise(crimes = n())

complaints <- merge(NYC_tractids, complaints, by.x = "GEOID10", 
                    by.y = "nearest_tract", all.x = TRUE)

## Crime rate by tract
crime_rate <- merge(complaints, centroids, by.x = "GEOID10", by.y = "TRACTID",
                    all.x = TRUE, all.y = FALSE) %>%
  mutate(crime_rate = 1000*crimes/(POPULATION+0.5)) %>%
  select(GEOID10, crimes, population = POPULATION, crime_rate, lat = LATITUDE, lng = LONGITUDE)

## Merge population onto crime rate by tract
tracts_allyears <- merge(crime_rate, tracts_2010, by = "GEOID10", all.x = TRUE) %>%
  merge(tracts_2011, by = "GEOID10", all.x = TRUE) %>%
  merge(tracts_2012, by = "GEOID10", all.x = TRUE)

## NA's should be zeroes--artifact of how merge works
for(i in 2:ncol(tracts_allyears)) {
  tracts_allyears[is.na(tracts_allyears[,i]),i] <- 0
}

## Ordering the data in the same way as the shapefile
shapefile_tracts_in_order <- as.character(NYC_tracts@data$GEOID10)
tracts_allyears <- tracts_allyears %>%
  arrange(GEOID10)
tracts_allyears <- tracts_allyears[order(shapefile_tracts_in_order),]
tracts_allyears$stoprate2010 <- tracts_allyears$stops2010/(tracts_allyears$population+.5)
tracts_allyears$stoprate2011 <- tracts_allyears$stops2011/(tracts_allyears$population+.5)
tracts_allyears$stoprate2012 <- tracts_allyears$stops2012/(tracts_allyears$population+.5)

## Census tract to PUMA key
PUMA_tract_key <- read.table("2010_Census_Tract_to_2010_PUMA.txt", header = TRUE, sep = ",",
                             stringsAsFactors = FALSE, colClasses = "character") %>%
  mutate(GEOID10 = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
  dplyr::select(GEOID10, PUMA5CE)

## Various objects for analyses
NYC_PUMAs.utm <- spTransform(NYC_PUMAs, CRS("+init=epsg:3724 +units=km"))
NYC_PUMAs.nb <- poly2nb(NYC_PUMAs.utm, queen = FALSE)
NYC_PUMAs.lw <- nb2listw(NYC_PUMAs.nb)

## Manipulating pathological neighbors for tracts
NYC_tracts.nb <- poly2nb(NYC_tracts, queen = FALSE)
NYC_tracts.lw <- nb2listw(NYC_tracts.nb, zero.policy = TRUE)

# Fudge neighbor list for tract 36005038500 in row 894 because it has no neighbors
NYC.nb2 <- NYC_tracts.nb
NYC.nb2[[894]] <- 896L
NYC.nb2[[896]] <- sort(c(NYC.nb2[[896]], 894L))
NYC.lw2 <- nb2listw(NYC.nb2, zero.policy = TRUE)

## Collapse census tracts by PUMA and order appropriately
tracts_allyears_pumas <- tracts_allyears %>%
  merge(PUMA_tract_key, by = "GEOID10", all.x = TRUE, all.y = FALSE)

pumas_allyears <- tracts_allyears_pumas %>%
  group_by(PUMA5CE) %>%
  summarise(crimes = sum(crimes),
            population = sum(population) + 1,
            crime_rate = crimes / population,
            lat = mean(lat),
            lng = mean(lng),
            stops2010 = sum(stops2010),
            pop2010 = sum(pop2010),
            stops2011 = sum(stops2011),
            pop2011 = sum(pop2011),
            stops2012 = sum(stops2012),
            pop2012 = sum(pop2012))

puma_order <- NYC_PUMAs.utm@data$puma %>% as.character %>% as.numeric %>% rank
pumas_allyears <- pumas_allyears[puma_order,]

#### Regressions and residuals ####
## On crime (2018)
crime_reg2010 <- glm(stops2010~crime_rate, offset = log(population),
                     data = pumas_allyears, family = poisson)
crime_reg2011 <- glm(stops2011~crime_rate, offset = log(population),
                     data = pumas_allyears, family = poisson)
crime_reg2012 <- glm(stops2012~crime_rate, offset = log(population),
                     data = pumas_allyears, family = poisson)
summary(crime_reg2010)
summary(crime_reg2011)
summary(crime_reg2012)

## On time-lagged stop rate
pumas_lagreg2011 <- glm(stops2011~I(log(stops2010/population)), offset = log(population), data = pumas_allyears,
                        family = poisson)
pumas_lagreg2012 <- glm(stops2011~I(log(stops2011/population)), offset = log(population), data = pumas_allyears,
                        family = poisson)
summary(pumas_lagreg2011)
summary(pumas_lagreg2012)

## Obtain residuals
pumas_allyears$resid_crime2010 <- residuals(pumas_crimereg2010)
pumas_allyears$resid_crime2011 <- residuals(pumas_crimereg2011)
pumas_allyears$resid_crime2012 <- residuals(pumas_crimereg2012)
pumas_allyears$resid_lag2011 <- residuals(pumas_lagreg2011)
pumas_allyears$resid_lag2012 <- residuals(pumas_lagreg2012)

#### Choropleths (tracts) ####
## 2010
choropleth(NYC_tracts, tracts_allyears$stops2010, lwd = 0.1) # Stops, 2010
choropleth(NYC_tracts, tracts_allyears$stoprate2010, lwd = 0.1) # Stop rates, 2010

## 2011
choropleth(NYC_tracts, tracts_allyears$stops2011, lwd = 0.1) # Stops, 2011
choropleth(NYC_tracts, tracts_allyears$stoprate2011, lwd = 0.1) # Stop rates, 2011

## 2012
choropleth(NYC_tracts, tracts_allyears$stops2012, lwd = 0.1) # Stops, 2012
choropleth(NYC_tracts, tracts_allyears$stoprate2012, lwd = 0.1) # Stop rates, 2012

#### Choropleths (PUMAs) ####
## Straight-up stop rates
choropleth(NYC_PUMAs, pumas_allyears$stops2010/pumas_allyears$pop2010)
choropleth(NYC_PUMAs, pumas_allyears$stops2011/pumas_allyears$pop2011)
choropleth(NYC_PUMAs, pumas_allyears$stops2012/pumas_allyears$pop2012)

## Crime regression residuals
choropleth(NYC_PUMAs, pumas_allyears$resid_crime2010)
choropleth(NYC_PUMAs, pumas_allyears$resid_crime2011)
choropleth(NYC_PUMAs, pumas_allyears$resid_crime2012)

## Lagged stop rate residuals
choropleth(NYC_PUMAs, pumas_allyears$resid_lag2011)
choropleth(NYC_PUMAs, pumas_allyears$resid_lag2012)

#### Hypothesis testing ####
set.seed(2015)
## Geary's C first
## Stop rates
geary.mc(pumas_allyears$stops2010/pumas_allyears$pop2010, NYC_PUMAs.lw, nsim = 999)
geary.mc(pumas_allyears$stops2011/pumas_allyears$pop2011, NYC_PUMAs.lw, nsim = 999)
geary.mc(pumas_allyears$stops2012/pumas_allyears$pop2012, NYC_PUMAs.lw, nsim = 999)

## Crime residuals
geary.mc(pumas_allyears$resid_crime2010, NYC_PUMAs.lw, nsim = 999)
geary.mc(pumas_allyears$resid_crime2011, NYC_PUMAs.lw, nsim = 999)
geary.mc(pumas_allyears$resid_crime2012, NYC_PUMAs.lw, nsim = 999)

## Lag stop rate residuals
geary.mc(pumas_allyears$resid_lag2011, NYC_PUMAs.lw, nsim = 999)
geary.mc(pumas_allyears$resid_lag2012, NYC_PUMAs.lw, nsim = 999)

## Moran's I
## Stop rates
moran.mc(pumas_allyears$stops2010/pumas_allyears$pop2010, NYC_PUMAs.lw, nsim = 999)
moran.mc(pumas_allyears$stops2011/pumas_allyears$pop2011, NYC_PUMAs.lw, nsim = 999)
moran.mc(pumas_allyears$stops2012/pumas_allyears$pop2012, NYC_PUMAs.lw, nsim = 999)

## Crime residuals
moran.mc(pumas_allyears$resid_crime2010, NYC_PUMAs.lw, nsim = 999)
moran.mc(pumas_allyears$resid_crime2011, NYC_PUMAs.lw, nsim = 999)
moran.mc(pumas_allyears$resid_crime2012, NYC_PUMAs.lw, nsim = 999)

## Lag stop rate residuals
moran.mc(pumas_allyears$resid_lag2011, NYC_PUMAs.lw, nsim = 999)
moran.mc(pumas_allyears$resid_lag2012, NYC_PUMAs.lw, nsim = 999)