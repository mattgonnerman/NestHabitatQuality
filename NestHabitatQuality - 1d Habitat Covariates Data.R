### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel'), require, character.only = T)

# ########################
# ### ORIGINAL RASTERS ###
# ########################
# #Raster prep was completed in ArcGIS Pro, NestHabitatQuality.gdb
# 
# ## NLCD
# # Rasters are just 1's and 0's for each category, for use in moving window.
# # NLCD Shrub (51,52)
# Shrub_raster <- raster("./GIS/Shrub.tif")
# plot(DisttoRoad_raster)
# 
# # NLCD Developed (21,22,23,24)
# Developed_raster <- raster("./GIS/Developed.tif")
# plot(Developed_raster)
# 
# # NLCD Agriculture (81,82)
# Agriculture_raster <- raster("./GIS/Agriculture.tif")
# plot(Agriculture_raster)
# 
# # NLCD Herbaceous (71,72)
# Herbaceous_raster <- raster("./GIS/Herbaceous.tif")
# plot(Herbaceous_raster)
# 
# 
# ## LiDAR
# #Basal Area
# BA_raster <- raster("./GIS/BA_19.tif")
# 
# #Mean Tree Height
# meanHT_raster_raw <- raster("./GIS/HT_Mean_19.tif")
# 
# #Percent Softwoods
# SW_raster_raw <- raster("./GIS/Prercent_SW_19.tif")
# 
# 
# ## Distance to Road
# #Rasterized MDOT Public Road Polyline, 10m
# #Copied RoadRaster and set all cells to 0 in copy
# #Used Euclidean Distance tool, with 0's raster as "input raster or feature source data", cell size = 30, no other inputs
# DisttoRoad_raster <- raster("./GIS/DisttoRoad.tif")
# plot(DisttoRoad_raster)
# 
# ## Distance to Forest Edge
# #Used Mean Tree height LiDAR Layer. If Null, then non forested, otherwise forested
# #For distance to nonforest, Used Euclidean Distance tool, with HT_Mean_19 raster as "input raster or feature source data", cell size = 10, no other inputs
# #For distance to forest, flipped HT_Mean_19 so null's had a value (1) and otherwise were set to null, then used this raster in Euclidean Distance
# #Combined rasters to make distance to edge raster
# DisttoForestEdge_raster <- raster("./GIS/DisttoEdge.tif")
# 
# ### Create Distance to Riparian Zone
# #Used National Hydrography Dataset
# #Subset Line feature to only StreamRiver FCode
# #Rasterized subsetted lines, 30m cell size
# #Euclidean distance to get distance to riparian areas
# DisttoRiparian_raster <- raster("./GIS/DisttoRip.tif")
# 
# 
# ##############################################################
# ### USE MOVEMENT DATA TO DETERMINE MOVING WINDOW DISTANCES ###
# ##############################################################
# #Login info to download data from Movebank directly
# login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")
# 
# #Number of cores for do parallel
# numCores <- detectCores()/2
# 
# ### Load relevant databases
# # Trapping data
# trap.raw <- read.csv("Trapping - Data.csv") %>%
#   filter(Recapture != 1) %>%
#   dplyr::select(BirdID = AlumBand, Trans.Type, CapAge = Age, CapDate = Date, CapLoc = Location)
# 
# # Nest site information
# nestfate.raw <- read.csv("Nest Monitoring - Nest Info.csv") %>%
#   dplyr::select(NestID, BirdID = Alum.Band.ID, Nest.Attempt, NestLat, NestLong,
#                 LayDate = Est.Laying.Initiation, HatchDate = EstimatedHatch, NestYear = Year)
# 
# # Merge Capture location information with trapping data
# capsites.df <- read.csv("CaptureSites - Sheet1.csv") %>% dplyr::select(CapLoc = Location.Name, CapLat = Latitude, CapLong = Longitude)
# trap.sites <- merge(trap.raw, capsites.df, by = "CapLoc", all.x = T)
# 
# # Merge nest information with individual and capture site information
# nestinfo <- merge(nestfate.raw, trap.sites, by = 'BirdID', all.x = T) %>%
#   filter(!is.na(NestLat)) %>%
#   filter(Nest.Attempt == 1) %>%
#   mutate(CapDate = as.POSIXct(CapDate, format = '%m/%d/%Y')) %>%
#   mutate(LayDate = as.POSIXct(LayDate, format = '%m/%d/%Y')) %>%
#   mutate(HatchDate = as.POSIXct(HatchDate, format = '%m/%d/%Y')) %>%
#   mutate(CapYear = year(CapDate)) %>%
#   mutate(Age = ifelse(CapYear < NestYear, "A", CapAge)) %>%
#   dplyr::select(-CapAge)
# 
# # Identify GPS versus VHF Nests
# gps_nests <- nestinfo %>% filter(Trans.Type == "GPS_Back") %>%
#   filter(!is.na(BirdID)) %>%
#   arrange(NestID)
# vhf_nests <- nestinfo %>% filter(Trans.Type != "GPS_Back") %>%
#   filter(!is.na(BirdID)) %>%
#   arrange(NestID)
# 
# # Load Timestamps for nesting stages of GPS birds
# # Timestamps are based on visually expecting movement tracks and using brownian motion variance to discern changes in movement
# gps.nest.TS <-  read.csv("GPSPRenestStart.csv") %>%
#   mutate(NestID = paste(BirdID, Year, 1, sep = "-"))
# gps_nest_info <- merge(gps_nests, gps.nest.TS, by = c("NestID", "BirdID"), all = T)
# 
# #Timestamps for Movebank points
# gps.nest.TS <-  gps_nest_info %>%
#   mutate(NestID = paste(BirdID, Year, 1, sep = "-")) %>%
#   mutate(PrenestStart = as.POSIXct(PrenestStart, format = "%Y/%m/%d %H:%M:%S.000")) %>%
#   mutate(FirstNestVisit = as.POSIXct(FirstNestVisit, format = "%Y/%m/%d %H:%M:%S.000")) %>%
#   mutate(IncubationStart = as.POSIXct(IncubationStart, format = "%Y/%m/%d %H:%M:%S.000")) %>%
#   mutate(Prev7_FirstNest = FirstNestVisit - as.difftime(7, unit="days")) %>%
#   mutate(Prev1_FirstNest = FirstNestVisit - as.difftime(1, unit="days")) %>%
#   mutate(Day1NestYear = paste(year(PrenestStart), "0101000000000", sep = "")) %>%
#   mutate(PrenestStart = paste(gsub("[^0-9]", "", PrenestStart), "000", sep = "")) %>%
#   mutate(FirstNestVisit = paste(gsub("[^0-9]", "", FirstNestVisit), "000", sep = "")) %>%
#   mutate(IncubationStart = paste(gsub("[^0-9]", "", IncubationStart), "000", sep = "")) %>%
#   mutate(Prev7_FirstNest = paste(gsub("[^0-9]", "", Prev7_FirstNest), "000", sep = "")) %>%
#   mutate(Prev1_FirstNest = paste(gsub("[^0-9]", "", Prev1_FirstNest), "000", sep = ""))
# 
# animalID.df <- getMovebankAnimals(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login)
# gps.nest.TS$BirdID %in% animalID.df$local_identifier
# 
# ### Mean Winter to Nest Distance
# # registerDoParallel(numCores)
# gps.W2N.maxdist.list <- foreach(i=1:nrow(gps.nest.TS), .packages = c("geosphere", "sp", "move")) %do% {
#   max(
#     distm(
#       coordinates(
#         getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
#                                                                   login = login,
#                                                                   animal = as.character(gps.nest.TS$BirdID[i]),
#                                                                   timestamp_start = gps.nest.TS$Day1NestYear[i],
#                                                                   timestamp_end = gps.nest.TS$FirstNestVisit[i])
#         )
#       )
#     )
# }
# # stopImplicitCluster()
# gps.W2N.maxdist <- unlist(gps.W2N.maxdist.list)
# mean(gps.W2N.maxdist)
# #8334.538
# 
# ### Max Prenesting Edge-to-Edge distance
# # registerDoParallel(numCores)
# gps.Prelaying.maxdist.list <- foreach(i=1:nrow(gps.nest.TS)) %do% {
#   animalname <- as.character(gps.nest.TS$BirdID[i])
#   timestart <- gps.nest.TS$PrenestStart[i]
#   timeend <- gps.nest.TS$FirstNestVisit[i]
#   year <- gps.nest.TS$NestYear[i]
#   nestid <- gps.nest.TS$NestID[i]
#   
#   turkeygps <- move::getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
#                                login = login,
#                                animal = animalname,
#                                timestamp_start = timestart,
#                                timestamp_end = timeend)
#   max(geosphere::distm(sp::coordinates(turkeygps)))
# }
# # stopImplicitCluster()
# gps.Prelaying.maxdist <- unlist(gps.Prelaying.maxdist.list)
# mean(gps.Prelaying.maxdist)
# #3360.267
# 
# ### Max Distance Week prior to laying
# # registerDoParallel(numCores)
# gps.7Days.maxdist.list <- foreach(i=1:nrow(gps.nest.TS)) %do% {
#   animalname <- as.character(gps.nest.TS$BirdID[i])
#   timestart <- gps.nest.TS$Prev7_FirstNest[i]
#   timeend <- gps.nest.TS$FirstNestVisit[i]
#   year <- gps.nest.TS$NestYear[i]
#   nestid <- gps.nest.TS$NestID[i]
#   
#   turkeygps <- move::getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
#                                login = login,
#                                animal = animalname,
#                                timestamp_start = timestart,
#                                timestamp_end = timeend)
#   max(geosphere::distm(sp::coordinates(turkeygps)))
# }
# # stopImplicitCluster()
# gps.7Days.maxdist <- unlist(gps.7Days.maxdist.list)
# mean(gps.7Days.maxdist)
# #3055.122
# 
# ### Max Distance 1 Day prior to laying
# # registerDoParallel(numCores)
# gps.1Day.maxdist.list <- foreach(i=1:nrow(gps.nest.TS)) %do% {
#   animalname <- as.character(gps.nest.TS$BirdID[i])
#   timestart <- gps.nest.TS$Prev1_FirstNest[i]
#   timeend <- gps.nest.TS$FirstNestVisit[i]
#   year <- gps.nest.TS$NestYear[i]
#   nestid <- gps.nest.TS$NestID[i]
#   
#   turkeygps <- move::getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
#                                login = login,
#                                animal = animalname,
#                                timestamp_start = timestart,
#                                timestamp_end = timeend)
#   max(geosphere::distm(sp::coordinates(turkeygps)))
# }
# # stopImplicitCluster()
# gps.1Day.maxdist <- unlist(gps.1Day.maxdist.list)
# mean(gps.1Day.maxdist)
# # 1328.168

#############################
### MOVING WINDOW RASTERS ###
#############################
### For each raster, use the Focal Statistic to quantify 
## Foc1 = Winter to Nest = 8334.538
#floor(8334.538/30) = 277
#floor(8334.538/10) = 833
## Foc2 = Prenesting = 3360.267
#floor(3360.267/30) = 112
# floor(3360.267/10) = 336
## Foc 3 = Prior 24 Hours = 1328.168
#floor(1328.168/30) = 44
# floor(1328.168/10) = 132
## Foc 4 = Immediate Surroundings = 120
#floor(120/30) = 4
# floor(120/10) = 12
## Mean, Rectangular Neighborhood


### Load Moving Window Rasters
## Proporition Agriculture
# Foc1
ag_foc1 <- raster("./GIS/FinalRasters/Ag_Foc1.tif")
# Foc2
ag_foc2 <- raster("./GIS/FinalRasters/Ag_Foc2.tif")
# Foc3
ag_foc3 <- raster("./GIS/FinalRasters/Ag_Foc3.tif")
# Foc4
ag_foc4 <- raster("./GIS/FinalRasters/Ag_Foc4.tif")

## Proporition Developed
# Foc1
dev_foc1 <- raster("./GIS/FinalRasters/Dev_Foc1.tif")
# Foc2
dev_foc2 <- raster("./GIS/FinalRasters/Dev_Foc2.tif")
# Foc3
dev_foc3 <- raster("./GIS/FinalRasters/Dev_Foc3.tif")
# Foc4
dev_foc4 <- raster("./GIS/FinalRasters/Dev_Foc4.tif")

## Proporition Shrub/Scrub
# Foc1
shrub_foc1 <- raster("./GIS/FinalRasters/Shrub_Foc1.tif")
# Foc2
shrub_foc2 <- raster("./GIS/FinalRasters/Shrub_Foc2.tif")
# Foc3
shrub_foc3 <- raster("./GIS/FinalRasters/Shrub_Foc3.tif")
# Foc4
shrub_foc4 <- raster("./GIS/FinalRasters/Shrub_Foc4.tif")

## Proporition Herbaceous
# Foc1
herb_foc1 <- raster("./GIS/FinalRasters/Herb_Foc1.tif")
# Foc2
herb_foc2 <- raster("./GIS/FinalRasters/Herb_Foc2.tif")
# Foc3
herb_foc3 <- raster("./GIS/FinalRasters/Herb_Foc3.tif")
# Foc4
herb_foc4 <- raster("./GIS/FinalRasters/Herb_Foc4.tif")

## Mean Basal Area
#Ignore null = only assessing forested area within range
# Foc1
BA_foc1 <- raster("./GIS/FinalRasters/BA_Foc1.tif")
# Foc2
BA_foc2 <- raster("./GIS/FinalRasters/BA_Foc2.tif")
# Foc3
BA_foc3 <- raster("./GIS/FinalRasters/BA_Foc3.tif")
# Foc4
BA_foc4 <- raster("./GIS/FinalRasters/BA_Foc4.tif")

## Mean Tree Height
#Ignore null = only assessing forested area within range
# Foc1
HT_foc1 <- raster("./GIS/FinalRasters/HT_Foc1.tif")
# Foc2
HT_foc2 <- raster("./GIS/FinalRasters/HT_Foc2.tif")
# Foc3
HT_foc3 <- raster("./GIS/FinalRasters/HT_Foc3.tif")
# Foc4
HT_foc4 <- raster("./GIS/FinalRasters/HT_Foc4.tif")

## Percent Softwood
#Ignore null = only assessing forested area within range
# Foc1
SW_foc1 <- raster("./GIS/FinalRasters/SW_Foc1.tif")
# Foc2
SW_foc2 <- raster("./GIS/FinalRasters/SW_Foc2.tif")
# Foc3
SW_foc3 <- raster("./GIS/FinalRasters/SW_Foc3.tif")
# Foc4
SW_foc4 <- raster("./GIS/FinalRasters/SW_Foc4.tif")

## Mean Distance to Edge
# Foc1
D2Edge_foc1 <- raster("./GIS/FinalRasters/D2Edge_Foc1.tif")
# Foc2
D2Edge_foc2 <- raster("./GIS/FinalRasters/D2Edge_Foc2.tif")
# Foc3
D2Edge_foc3 <- raster("./GIS/FinalRasters/D2Edge_Foc3.tif")
# Foc4
D2Edge_foc4 <- raster("./GIS/FinalRasters/D2Edge_Foc4.tif")

## Mean Distance to Road
# Foc1
D2Road_foc1 <- raster("./GIS/FinalRasters/D2Road_Foc1.tif")
# Foc2
D2Road_foc2 <- raster("./GIS/FinalRasters/D2Road_Foc2.tif")
# Foc3
D2Road_foc3 <- raster("./GIS/FinalRasters/D2Road_Foc3.tif")
# Foc4
D2Road_foc4 <- raster("./GIS/FinalRasters/D2Road_Foc4.tif")

## Mean Distance to Riparian Area
# Foc1
D2Rip_foc1 <- raster("./GIS/FinalRasters/D2Rip_Foc1.tif")
# Foc2
D2Rip_foc2 <- raster("./GIS/FinalRasters/D2Rip_Foc2.tif")
# Foc3
D2Rip_foc3 <- raster("./GIS/FinalRasters/D2Rip_Foc3.tif")
# Foc4
D2Rip_foc4 <- raster("./GIS/FinalRasters/D2Rip_Foc4.tif")

## Is the point in water (NLCD)
Water <- raster("./GIS/Water.tif")

#Combine rasters into list
rasterlist <- list(ag_foc1,ag_foc2,ag_foc3,ag_foc4,
                   dev_foc1,dev_foc2,dev_foc3,dev_foc4,
                   shrub_foc1,shrub_foc2,shrub_foc3,shrub_foc4,
                   herb_foc1,herb_foc2,herb_foc3,herb_foc4,
                   BA_foc1,BA_foc2,BA_foc3,BA_foc4,
                   HT_foc1,HT_foc2,HT_foc3,HT_foc4,
                   SW_foc1,SW_foc2,SW_foc3,SW_foc4,
                   D2Edge_foc1,D2Edge_foc2,D2Edge_foc3,D2Edge_foc4,
                   D2Road_foc1,D2Road_foc2,D2Road_foc3,D2Road_foc4,
                   D2Rip_foc1,D2Rip_foc2,D2Rip_foc3,D2Rip_foc4,
                   Water)

names(rasterlist) <- c("ag_foc1","ag_foc2","ag_foc3","ag_foc4",
                       "dev_foc1","dev_foc2","dev_foc3","dev_foc4",
                       "shrub_foc1","shrub_foc2","shrub_foc3","shrub_foc4",
                       "herb_foc1","herb_foc2","herb_foc3","herb_foc4",
                       "BA_foc1","BA_foc2","BA_foc3","BA_foc4",
                       "HT_foc1","HT_foc2","HT_foc3","HT_foc4",
                       "SW_foc1","SW_foc2","SW_foc3","SW_foc4",
                       "D2Edge_foc1","D2Edge_foc2","D2Edge_foc3","D2Edge_foc4",
                       "D2Road_foc1","D2Road_foc2","D2Road_foc3","D2Road_foc4",
                       "D2Rip_foc1","D2Rip_foc2","D2Rip_foc3","D2Rip_foc4",
                       "Water")

##########################
### EXTRACT COVARIATES ###
##########################
#https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points

### PreLaying Selection
pl.vhf.used <- st_read("./GIS/prelaying.vhf.used.points.shp") %>%
  mutate(Used = 1) %>%
  mutate(GPS = 0)
pl.vhf.avail <- st_read("./GIS/prelaying.vhf.avail.points.shp")%>%
  mutate(Used = 0) %>%
  mutate(GPS = 0)
pl.gps.used <- st_read("./GIS/prelaying.gps.used.points.shp")%>%
  mutate(Used = 1) %>%
  mutate(GPS = 1) %>%
  dplyr::select(-timestamp)
pl.gps.avail <- st_read("./GIS/prelaying.gps.avail.points.shp")%>%
  mutate(Used = 0) %>%
  mutate(GPS = 1)

prelaying.points <- rbind(pl.vhf.used, pl.vhf.avail, pl.gps.used, pl.gps.avail)

#Create an R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sf)

# Run parallelized 'extract' function and stop cluster
prelaying.extract <- sfSapply(rasterlist, extract, y=prelaying.points)
colnames(prelaying.extract) <- names(rasterlist)
sfStop()

prelaying.covs <- cbind(prelaying.points, prelaying.extract) %>%
  filter(Water == 0)
prelaying.covs.z <- prelaying.covs %>%
  mutate_at(names(rasterlist),funs(c(scale(.))))
st_write(prelaying.covs.z, "./GIS/Prelaying_Covs_Z.shp")

### Laying Selection
lay.vhf.used <- st_read("./GIS/laying.vhf.used.points.shp") %>%
  mutate(Used = 1) %>%
  mutate(GPS = 0)
lay.vhf.avail <- st_read("./GIS/laying.vhf.avail.points.shp")%>%
  mutate(Used = 0) %>%
  mutate(GPS = 0)
lay.gps.used <- st_read("./GIS/laying.gps.used.points.shp")%>%
  mutate(Used = 1) %>%
  mutate(GPS = 1) %>%
  dplyr::select(-timestamp)
lay.gps.avail <- st_read("./GIS/laying.gps.avail.points.shp")%>%
  mutate(Used = 0) %>%
  mutate(GPS = 1)

laying.points <- rbind(lay.vhf.used, lay.vhf.avail, lay.gps.used, lay.gps.avail)

#Create an R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sf)

# Run parallelized 'extract' function and stop cluster
laying.extract <- sfSapply(rasterlist, extract, y=laying.points)
colnames(laying.extract) <- names(rasterlist)
sfStop()

laying.covs <- cbind(laying.points, laying.extract) %>%
  filter(Water == 0)
laying.covs.z <- laying.covs %>%
  mutate_at(names(rasterlist),funs(c(scale(.))))
st_write(laying.covs.z, "./GIS/Laying_Covs_Z.shp")

### Nest Site Selection
nest.vhf.used <- st_read("./GIS/nest.vhf.used.points.shp") %>%
  mutate(Used = "1") %>%
  mutate(GPS = "0")
nest.vhf.avail <- st_read("./GIS/nest.vhf.avail.points.shp")%>%
  mutate(Used = "0") %>%
  mutate(GPS = "0")
nest.gps.used <- st_read("./GIS/nest.gps.used.points.shp")%>%
  mutate(Used = "1") %>%
  mutate(GPS = "1")
nest.gps.avail <- st_read("./GIS/nest.gps.avail.points.shp")%>%
  mutate(Used = "0") %>%
  mutate(GPS = "1")

nest.points <- rbind(nest.vhf.used, nest.vhf.avail, nest.gps.used, nest.gps.avail)

#Create an R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sf)

# Run parallelized 'extract' function and stop cluster
nest.extract <- sfSapply(rasterlist, extract, y=nest.points)
colnames(nest.extract) <- names(rasterlist)
sfStop()

nest.covs <- cbind(nest.points, nest.extract) %>%
  filter(Water == 0)
nest.covs.z <- nest.covs %>%
  mutate_at(names(rasterlist),funs(c(scale(.))))
st_write(nest.covs.z, "./GIS/Nest_Covs_Z.shp")


### Nest Success 
##Load Nest Info to get X,Y, and NestID
nestlocations <- read.csv("Nest Monitoring - Nest Info.csv") %>%
  filter(!is.na(NestLat)) %>%
  dplyr::select(NestID, NestLat, NestLong)
nestsuccess.sf <- st_as_sf(nestlocations,
         coords = c("NestLong", "NestLat"), crs = 4326)
nestsuccess.sf <- st_transform(nestsuccess.sf, 32619)

#Create an R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sf)

# Run parallelized 'extract' function and stop cluster
nestsuccess.extract <- sfSapply(rasterlist, extract, y=nestsuccess.sf)
colnames(nestsuccess.extract) <- names(rasterlist)
sfStop()

nestsuccess.covs <- cbind(nestsuccess.sf, nestsuccess.extract)
nestsuccess.covs.z <- nestsuccess.covs %>%
  mutate_at(names(rasterlist),funs(c(scale(.))))
st_write(nestsuccess.covs.z, "./GIS/NestSuccess_Covs_Z.shp")


### Hen Survival