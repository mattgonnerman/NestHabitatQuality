### Load relevant packages
lapply(c('dplyr', 'sf', 'move', 'raster', 'lubridate', 'ggplot2', 'foreach', 'doParallel'), require, character.only = T)

#Login info to download data from Movebank directly
login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")

#Number of cores for do parallel
numCores <- detectCores() - 1

### Load relevant databases
# Trapping data
trap.raw <- read.csv("Trapping - Data.csv") %>%
  filter(Recapture != "Y") %>%
  dplyr::select(BirdID = AlumBand, Trans.Type, CapAge = Age, CapDate = Date, CapLoc = Location)

# Nest site information
nestfate.raw <- read.csv("Nest Monitoring - Nest Info.csv") %>%
  dplyr::select(NestID, BirdID = Alum.Band.ID, Nest.Attempt, NestLat, NestLong,
                LayDate = Est.Laying.Initiation, HatchDate = EstimatedHatch, NestYear = Year)

# Merge Capture location information with trapping data
capsites.df <- read.csv("CaptureSites - Sheet1.csv") %>% dplyr::select(CapLoc = Location.Name, CapLat = Latitude, CapLong = Longitude)
trap.sites <- merge(trap.raw, capsites.df, by = "CapLoc", all.x = T)

# Merge nest information with individual and capture site information
nestinfo <- merge(nestfate.raw, trap.sites, by = 'BirdID', all.x = T) %>%
  filter(!is.na(NestLat)) %>%
  filter(Nest.Attempt == 1) %>%
  mutate(CapDate = as.POSIXct(CapDate, format = '%m/%d/%Y')) %>%
  mutate(LayDate = as.POSIXct(LayDate, format = '%m/%d/%Y')) %>%
  mutate(HatchDate = as.POSIXct(HatchDate, format = '%m/%d/%Y')) %>%
  mutate(CapYear = year(CapDate)) %>%
  mutate(Age = ifelse(CapYear < NestYear, "A", CapAge)) %>%
  dplyr::select(-CapAge)

# Identify GPS versus VHF Nests
gps_nests <- nestinfo %>% filter(Trans.Type == "GPS_Back") %>%
  filter(!is.na(BirdID)) %>%
  arrange(NestID)
vhf_nests <- nestinfo %>% filter(Trans.Type != "GPS_Back") %>%
  filter(!is.na(BirdID)) %>%
  arrange(NestID)

# Load Timestamps for nesting stages of GPS birds
# Timestamps are based on visually expecting movement tracks and using brownian motion variance to discern changes in movement
gps.nest.TS <-  read.csv("GPSPRenestStart.csv") %>%
  mutate(NestID = paste(BirdID, Year, 1, sep = "-"))
gps_nest_info <- merge(gps_nests, gps.nest.TS, by = c("NestID", "BirdID"), all = T)

#####################################################
### Pre Laying Selection Used and Available Sites ###
#####################################################
### Used = PreLaying Home Range
### Available = Jan 1 through Prelaying Home Range

## GPS BIRDS ##
# USED
#Format timestamps for movebank
prelaying.gps.used.times <- gps_nest_info %>%
  filter(!is.na(NestYear)) %>%
  dplyr::select(NestID, BirdID, NestYear, StartTimestamp = PrenestStart, EndTimestamp = FirstNestVisit) %>%
  mutate(StartTimestamp = gsub("[^0-9]", "", StartTimestamp)) %>%
  mutate(EndTimestamp = gsub("[^0-9]", "", EndTimestamp)) %>%
  filter(!is.na(StartTimestamp))

rm("prelaying.gps.used.polygon")
rm("prelaying.gps.used")
#Loop through timestamps and create dBBMM polygons for each nest
for(i in 1:nrow(prelaying.gps.used.times)){
  animalname <- as.character(prelaying.gps.used.times$BirdID[i])
  timestart <- prelaying.gps.used.times$StartTimestamp[i]
  timeend <- prelaying.gps.used.times$EndTimestamp[i]
  year <- prelaying.gps.used.times$NestYear[i]
  nestid <- prelaying.gps.used.times$NestID[i]
  
  turkeygps <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
                               login = login,
                               animal = animalname,
                               timestamp_start = timestart,
                               timestamp_end = timeend)
  t_turkeygps <- spTransform(turkeygps, crs = 32619, center=T)
  turk_dBBMM <- brownian.bridge.dyn(t_turkeygps, raster = 30, location.error = 17, margin = 5, window.size = 15, ext = 1.5)
  turkey_UD <-raster2contour(turk_dBBMM, level=c(.95))
  dBBMM_line <- st_as_sf(turkey_UD, "SpatialLines")
  dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
  dBBMM_poly$NestID <- nestid
  
  if(exists("prelaying.gps.used")){
    dBBMM_poly <- st_transform(dBBMM_poly, st_crs(prelaying.gps.used.polygon))
    prelaying.gps.used.polygon <- rbind(prelaying.gps.used.polygon, dBBMM_poly)
    prelaying.gps.used <- rbind(prelaying.gps.used, as.data.frame(turkeygps@data) %>% mutate(NestID = nestid))
  }else{
    prelaying.gps.used.polygon <- dBBMM_poly
    prelaying.gps.used <- as.data.frame(turkeygps@data) %>% mutate(NestID = nestid)
  }
}
#Transform to UTM 19N
prelaying.gps.used.polygon <- st_transform(prelaying.gps.used.polygon, 32619)
#Number of used points for each nest, used in determining available points
prelaying.gps.used.length <- prelaying.gps.used %>% group_by(NestID) %>%
  summarize(Total = n())

#Save relevant files as shapefiles/csv
write.csv(prelaying.gps.used, "prelaying_gps_used.csv", row.names = F)
st_write(prelaying.gps.used.polygon, dsn = "./GIS", layer = "prelaying.gps.used.polygon", driver = "ESRI Shapefile", delete_layer = T)
prelaying.gps.used.points <- st_as_sf(prelaying.gps.used, coords = c("location_long", "location_lat"),
                                      dim = "XY", crs = 4326) %>%
  dplyr::select(NestID, geometry, timestamp)
prelaying.gps.used.points <- st_transform(prelaying.gps.used.points, 32619)
st_write(prelaying.gps.used.points, dsn = "./GIS", layer = "prelaying.gps.used.points", driver = "ESRI Shapefile", delete_layer = T)


# AVAILABLE
#Format timestamps for movebank
prelaying.gps.avail.times <- gps_nest_info %>%
  filter(!is.na(NestYear)) %>%
  dplyr::select(NestID, BirdID, NestYear, EndTimestamp = FirstNestVisit) %>%
  mutate(StartTimestamp = paste(NestYear, "0101000000000", sep = "")) %>%
  mutate(EndTimestamp = gsub("[^0-9]", "", EndTimestamp)) %>%
  filter(!is.na(EndTimestamp))

rm("prelaying.gps.avail.polygon")
#Loop through timestamps and create dBBMM polygons for each nest
for(i in 1:nrow(prelaying.gps.avail.times)){
  animalname <- as.character(prelaying.gps.avail.times$BirdID[i])
  timestart <- prelaying.gps.avail.times$StartTimestamp[i]
  timeend <- prelaying.gps.avail.times$EndTimestamp[i]
  year <- prelaying.gps.avail.times$NestYear[i]
  nestid <- prelaying.gps.avail.times$NestID[i]
  
  turkeygps <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
                               login = login,
                               animal = animalname,
                               timestamp_start = timestart,
                               timestamp_end = timeend)
  t_turkeygps <- spTransform(turkeygps, center=T)
  turk_dBBMM <- brownian.bridge.dyn(t_turkeygps, raster = 30, location.error = 17, margin = 5, window.size = 15, ext = 1.3)
  turkey_UD <-raster2contour(turk_dBBMM, level=c(.95))
  dBBMM_line <- st_as_sf(turkey_UD, "SpatialLines")
  dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
  dBBMM_poly$NestID <- nestid
  
  if(exists("prelaying.gps.avail.polygon")){
    dBBMM_poly <- st_transform(dBBMM_poly, st_crs(prelaying.gps.avail.polygon))
    prelaying.gps.avail.polygon <- rbind(prelaying.gps.avail.polygon, dBBMM_poly)
  }else{
    prelaying.gps.avail.polygon <- dBBMM_poly
  }
}
#Transform to UTM 19N
prelaying.gps.avail.polygon <- st_transform(prelaying.gps.avail.polygon, 32619)

#Number of available points for each nest
num.avail.prelaying <- 11*prelaying.gps.used.length$Total

#Parallel approach to st_sample, speeds things up for sampling available
registerDoParallel(numCores)
prelaying.gps.avail.points <- foreach(i=1:nrow(prelaying.gps.avail.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(prelaying.gps.avail.polygon[i,],
                 exact = T,
                 by_polygon = F,
                 size = num.avail.prelaying[i],
                 crs = 4326))
}
stopImplicitCluster()
#Format outputs so its useable
prelaying.gps.avail.points <- do.call(rbind, prelaying.gps.avail.points)
prelaying.gps.avail.points$NestID <- rep(prelaying.gps.avail.polygon$NestID, num.avail.prelaying)
colnames(prelaying.gps.avail.points)[1] <- "geometry"
st_geometry(prelaying.gps.avail.points) <- "geometry"
prelaying.gps.avail.points <- st_transform(prelaying.gps.avail.points, 32619)

#Save relevant polygons/points as shapefiles
st_write(prelaying.gps.avail.polygon, dsn = "./GIS", layer = "prelaying.gps.avail.polygon", driver = "ESRI Shapefile", delete_layer = T)
st_write(prelaying.gps.avail.points, dsn = "./GIS", layer = "prelaying.gps.avail.points", driver = "ESRI Shapefile", delete_layer = T)


## VHF BIRDS ##
# USED
#Use area of prelaying home range for GPS birds to determine buffer for VHF birds
vhf.prelaying.used.buffer.area <- mean(st_area(prelaying.gps.used.polygon))*1.25
vhf.prelaying.used.buffer.radius <- (vhf.prelaying.used.buffer.area/pi)^(1/2)
vhf.nests.xy <- vhf_nests %>% dplyr::select(NestID, x = NestLong, y = NestLat)
vhf.prelaying.used.nests <- st_as_sf(vhf.nests.xy,
         coords = c("x", "y"), crs = 4326)
vhf.prelaying.used.nests <- st_transform(vhf.prelaying.used.nests, 32619)
prelaying.vhf.used.polygon <- st_buffer(vhf.prelaying.used.nests, vhf.prelaying.used.buffer.radius)

#Parallel approach to sampling vhf used points
registerDoParallel(numCores)
prelaying.vhf.used.points <- foreach(i=1:nrow(prelaying.vhf.used.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(prelaying.vhf.used.polygon[i,],
                          exact = T,
                          by_polygon = F,
                          size = ceiling(mean(prelaying.gps.used.length$Total)*1.25)))
}
stopImplicitCluster()
prelaying.vhf.used.points <- do.call(rbind, prelaying.vhf.used.points)
prelaying.vhf.used.points$NestID <- rep(prelaying.vhf.used.polygon$NestID, ceiling(mean(prelaying.gps.used.length$Total)*1.25))
colnames(prelaying.vhf.used.points)[1] <- "geometry"
st_geometry(prelaying.vhf.used.points) <- "geometry"

#Save relevant polygons/points as shapefiles
st_write(prelaying.vhf.used.polygon, dsn = "./GIS", layer = "prelaying.vhf.used.polygon", driver = "ESRI Shapefile", delete_layer = T)
st_write(prelaying.vhf.used.points, dsn = "./GIS", layer = "prelaying.vhf.used.points", driver = "ESRI Shapefile", delete_layer = T)


#AVAILABLE 
#Create a line from capture to nest, then buffer it using the radius of prenesting home range
vhf.nests.xy <- vhf_nests %>% dplyr::select(NestID, x = NestLong, y = NestLat)
vhf.cap.xy <- vhf_nests %>% dplyr::select(NestID, x = CapLong, y = CapLat)

# #For birds that nested the year following capture, need a different location. Use first telemetry location of that year
# nestfollowingyear <- vhf_nests[which(year(vhf_nests$LayDate) != year(vhf_nests$CapDate)),] %>%
#   mutate(NestID = paste(BirdID, NestYear, 1, sep = "-"))
# firstlocationofyear <- read.csv("Telemetry_Data - Telemetry.csv") %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   dplyr::select(BirdID = AlumBand, CapLat = Lat1, CapLong = Long1, Date, Fate) %>%
#   mutate(NestID = paste(BirdID, year(Date), 1, sep = "-")) %>%
#   filter(NestID %in% nestfollowingyear$NestID) %>%
#   group_by(NestID) %>%
#   arrange(Date) %>%
#   slice(1L)
# 
# telempoint <- st_as_sf(firstlocationofyear, coords = c("CapLong", "CapLat"), crs = 4326) %>%
#   arrange(NestID)
# cappoint <- st_as_sf(vhf.cap.xy[which(vhf.cap.xy$NestID %in% firstlocationofyear$NestID),], coords = c("x", "y"), crs = 4326) %>%
#   arrange(NestID)
# 
# mean(st_distance(telempoint, cappoint, by_element = T))
# 
# firstlocationofyear_all <- read.csv("Telemetry_Data - Telemetry.csv") %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   dplyr::select(BirdID = AlumBand, CapLat = Lat1, CapLong = Long1, Date, Fate) %>%
#   mutate(NestID = paste(BirdID, year(Date), 1, sep = "-")) %>%
#   filter(NestID %in% vhf.nests.xy$NestID) %>%
#   group_by(NestID) %>%
#   arrange(Date) %>%
#   filter(!is.na(CapLat)) %>%
#   slice(1L)
# telempoint_all <- st_as_sf(firstlocationofyear_all, coords = c("CapLong", "CapLat"), crs = 4326) %>%
#   arrange(NestID)
# cappoint_all <- st_as_sf(vhf.cap.xy[which(vhf.cap.xy$NestID %in% firstlocationofyear_all$NestID),], coords = c("x", "y"), crs = 4326) %>%
#   arrange(NestID)
# mean(st_distance(telempoint_all, cappoint_all, by_element = T))
# 
# for(i in 1:nrow(firstlocationofyear)){
#   vhf.cap.xy$x[which(vhf.cap.xy$NestID==firstlocationofyear$NestID[i])] <- firstlocationofyear$CapLong[i]
#   vhf.cap.xy$y[which(vhf.cap.xy$NestID==firstlocationofyear$NestID[i])] <- firstlocationofyear$CapLat[i]
# }



#Combine nest and capture/first of year locations and create shapefiles
vhf.xy <- rbind(vhf.nests.xy, vhf.cap.xy) %>%
  group_by(NestID) %>%
  split(.,.[,"NestID"])
vhf.xy.mat <- lapply(vhf.xy, function(x) as.matrix(x[names(x) %in% c("x","y")]))
vhf.multilines.sfg <- st_multilinestring(vhf.xy.mat, dim = "XY")
vhf.multilines.sfc <- st_sfc(vhf.multilines.sfg, crs = 4326)
vhf.multilines.sfc <- st_transform(vhf.multilines.sfc, 32619)
vhf.lines.sfc <- st_cast(vhf.multilines.sfc, "LINESTRING")
vhf.captonest.line <- st_sf(data.frame(vhf.lines.sfc, vhf_nests))
# buffer2 <- ifelse(st_length(vhf.captonest.line) < mean(st_length(vhf.captonest.line)), mean(st_length(vhf.captonest.line)),st_length(vhf.captonest.line))
prelaying.vhf.avail.polygon <- st_buffer(vhf.captonest.line, vhf.prelaying.used.buffer.radius)

#Parallel sampling from within line buffers
registerDoParallel(numCores)
prelaying.vhf.avail.points <- foreach(i=1:nrow(prelaying.vhf.avail.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(prelaying.vhf.avail.polygon[i,],
                          exact = T,
                          by_polygon = F,
                          size = 11*ceiling(mean(prelaying.gps.used.length$Total)*1.25),
                          crs = 4326))
}
stopImplicitCluster()
prelaying.vhf.avail.points <- do.call(rbind, prelaying.vhf.avail.points)
prelaying.vhf.avail.points$NestID <- rep(prelaying.vhf.avail.polygon$NestID, 11*ceiling(mean(prelaying.gps.used.length$Total)*1.25))
colnames(prelaying.vhf.avail.points)[1] <- "geometry"
st_geometry(prelaying.vhf.avail.points) <- "geometry"

#Save relevant polygons/points as shapefiles
st_write(prelaying.vhf.avail.polygon, dsn = "./GIS", layer = "prelaying.vhf.avail.polygon", driver = "ESRI Shapefile", delete_layer = T)
st_write(prelaying.vhf.avail.points, dsn = "./GIS", layer = "prelaying.vhf.avail.points", driver = "ESRI Shapefile", delete_layer = T)




#################################################
### Laying Selection Used and Available Sites ###
#################################################
### Used = Laying Home Range
### Available = PreLaying + Laying Home Range 

## GPS BIRDS ##
# USED
#Format timestamps for movebank
laying.gps.used.times <- gps_nest_info %>%
  filter(!is.na(NestYear)) %>%
  dplyr::select(NestID, BirdID, NestYear, StartTimestamp = FirstNestVisit, EndTimestamp = IncubationStart) %>%
  mutate(StartTimestamp = gsub("[^0-9]", "", StartTimestamp)) %>%
  mutate(EndTimestamp = gsub("[^0-9]", "", EndTimestamp)) %>%
  filter(!is.na(StartTimestamp))

rm("laying.gps.used.polygon")
rm("laying.gps.used")
#Loop through timestamps and create dBBMM polygons for each nest
for(i in 1:nrow(laying.gps.used.times)){
  animalname <- as.character(laying.gps.used.times$BirdID[i])
  timestart <- laying.gps.used.times$StartTimestamp[i]
  timeend <- laying.gps.used.times$EndTimestamp[i]
  year <- laying.gps.used.times$NestYear[i]
  nestid <- laying.gps.used.times$NestID[i]
  
  turkeygps <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
                               login = login,
                               animal = animalname,
                               timestamp_start = timestart,
                               timestamp_end = timeend)
  t_turkeygps <- spTransform(turkeygps, crs = 32619, center=T)
  turk_dBBMM <- brownian.bridge.dyn(t_turkeygps, raster = 30, location.error = 17, margin = 5, window.size = 15, ext = 1.9)
  turkey_UD <-raster2contour(turk_dBBMM, level=c(.95))
  dBBMM_line <- st_as_sf(turkey_UD, "SpatialLines")
  dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
  dBBMM_poly$NestID <- nestid
  
  if(exists("laying.gps.used")){
    dBBMM_poly <- st_transform(dBBMM_poly, st_crs(laying.gps.used.polygon))
    laying.gps.used.polygon <- rbind(laying.gps.used.polygon, dBBMM_poly)
    laying.gps.used <- rbind(laying.gps.used, as.data.frame(turkeygps@data) %>% mutate(NestID = nestid))
  }else{
    laying.gps.used.polygon <- dBBMM_poly
    laying.gps.used <- as.data.frame(turkeygps@data) %>% mutate(NestID = nestid)
  }
}
#Transform to UTM 19N
laying.gps.used.polygon <- st_transform(laying.gps.used.polygon, 32619)
#Number of used points for each nest, used in determining available points
laying.gps.used.length <- laying.gps.used %>% group_by(NestID) %>%
  summarize(Total = n())

#Save relevant files as shapefiles/csv
write.csv(laying.gps.used, "laying_gps_used.csv", row.names = F)
st_write(laying.gps.used.polygon, dsn = "./GIS", layer = "laying.gps.used.polygon", driver = "ESRI Shapefile", delete_layer = T)
laying.gps.used.points <- st_as_sf(laying.gps.used, coords = c("location_long", "location_lat"),
                                      dim = "XY", crs = 4326) %>%
  dplyr::select(NestID, geometry, timestamp)
laying.gps.used.points <- st_transform(laying.gps.used.points, 32619)
st_write(laying.gps.used.points, dsn = "./GIS", layer = "laying.gps.used.points", driver = "ESRI Shapefile", delete_layer = T)


# AVAILABLE
#Format timestamps for movebank
laying.gps.avail.times <- gps_nest_info %>%
  filter(!is.na(NestYear)) %>%
  dplyr::select(NestID, BirdID, NestYear, StartTimestamp = PrenestStart, EndTimestamp = IncubationStart) %>%
  mutate(StartTimestamp = gsub("[^0-9]", "", StartTimestamp)) %>%
  mutate(EndTimestamp = gsub("[^0-9]", "", EndTimestamp)) %>%
  filter(!is.na(StartTimestamp))

rm("laying.gps.avail.polygon")
rm("laying.gps.avail")
#Loop through timestamps and create dBBMM polygons for each nest
for(i in 1:nrow(laying.gps.avail.times)){
  animalname <- as.character(laying.gps.avail.times$BirdID[i])
  timestart <- laying.gps.avail.times$StartTimestamp[i]
  timeend <- laying.gps.avail.times$EndTimestamp[i]
  year <- laying.gps.avail.times$NestYear[i]
  nestid <- laying.gps.avail.times$NestID[i]
  
  turkeygps <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
                               login = login,
                               animal = animalname,
                               timestamp_start = timestart,
                               timestamp_end = timeend)
  t_turkeygps <- spTransform(turkeygps, crs = 32619, center=T)
  turk_dBBMM <- brownian.bridge.dyn(t_turkeygps, raster = 30, location.error = 17, margin = 5, window.size = 15, ext = 1.5)
  turkey_UD <-raster2contour(turk_dBBMM, level=c(.95))
  dBBMM_line <- st_as_sf(turkey_UD, "SpatialLines")
  dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
  dBBMM_poly$NestID <- nestid
  
  if(exists("laying.gps.avail")){
    dBBMM_poly <- st_transform(dBBMM_poly, st_crs(laying.gps.avail.polygon))
    laying.gps.avail.polygon <- rbind(laying.gps.avail.polygon, dBBMM_poly)
    laying.gps.avail <- rbind(laying.gps.avail, as.data.frame(turkeygps@data) %>% mutate(NestID = nestid))
  }else{
    laying.gps.avail.polygon <- dBBMM_poly
    laying.gps.avail <- as.data.frame(turkeygps@data) %>% mutate(NestID = nestid)
  }
}
#Transform to UTM 19N
laying.gps.avail.polygon <- st_transform(laying.gps.avail.polygon, 32619)
#Parallel sampling from within line buffers
registerDoParallel(numCores)
laying.gps.avail.points <- foreach(i=1:nrow(laying.gps.avail.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(laying.gps.avail.polygon[i,],
                          exact = T,
                          by_polygon = F,
                          size = 11*ceiling(mean(laying.gps.used.length$Total)*1.25),
                          crs = 4326))
}
stopImplicitCluster()
laying.gps.avail.points <- do.call(rbind, laying.gps.avail.points)
laying.gps.avail.points$NestID <- rep(laying.gps.avail.polygon$NestID, 11*ceiling(mean(laying.gps.used.length$Total)*1.25))
colnames(laying.gps.avail.points)[1] <- "geometry"
st_geometry(laying.gps.avail.points) <- "geometry"

#Save relevant polygons/points as shapefiles
st_write(laying.gps.avail.polygon, dsn = "./GIS", layer = "laying.gps.avail.polygon", driver = "ESRI Shapefile", delete_layer = T)
st_write(laying.gps.avail.points, dsn = "./GIS", layer = "laying.gps.avail.points", driver = "ESRI Shapefile", delete_layer = T)


## VHF BIRDS ##
# USED
#Use area of laying home range for GPS birds to determine buffer for VHF birds
laying.vhf.used.buffer.area <- mean(st_area(laying.gps.used.polygon))*1.25
laying.vhf.used.buffer.radius <- (laying.vhf.used.buffer.area/pi)^(1/2)
vhf.nests.xy <- vhf_nests %>% dplyr::select(NestID, x = NestLong, y = NestLat)
laying.vhf.used.nests <- st_as_sf(vhf.nests.xy,
                                     coords = c("x", "y"), crs = 4326)
laying.vhf.used.nests <- st_transform(laying.vhf.used.nests, 32619)
laying.vhf.used.polygon <- st_buffer(laying.vhf.used.nests, laying.vhf.used.buffer.radius)

#Parallel approach to sampling vhf used points
registerDoParallel(numCores)
laying.vhf.used.points <- foreach(i=1:nrow(laying.vhf.used.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(laying.vhf.used.polygon[i,],
                          exact = T,
                          by_polygon = F,
                          size = ceiling(mean(laying.gps.used.length$Total)*1.25)))
}
stopImplicitCluster()
laying.vhf.used.points <- do.call(rbind, laying.vhf.used.points)
laying.vhf.used.points$NestID <- rep(laying.vhf.used.polygon$NestID, ceiling(mean(laying.gps.used.length$Total)*1.25))
colnames(laying.vhf.used.points)[1] <- "geometry"
st_geometry(laying.vhf.used.points) <- "geometry"

#Save relevant polygons/points as shapefiles
st_write(laying.vhf.used.polygon, dsn = "./GIS", layer = "laying.vhf.used.polygon", driver = "ESRI Shapefile", delete_layer = T)
st_write(laying.vhf.used.points, dsn = "./GIS", layer = "laying.vhf.used.points", driver = "ESRI Shapefile", delete_layer = T)

# AVAILABLE
laying.vhf.avail.polygon <- prelaying.vhf.used.polygon

#Parallel sampling from within line buffers
registerDoParallel(numCores)
laying.vhf.avail.points <- foreach(i=1:nrow(laying.vhf.avail.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(laying.vhf.avail.polygon[i,],
                          exact = T,
                          by_polygon = F,
                          size = 11*ceiling(mean(laying.gps.used.length$Total)*1.25)))
}
stopImplicitCluster()
laying.vhf.avail.points <- do.call(rbind, laying.vhf.avail.points)
laying.vhf.avail.points$NestID <- rep(laying.vhf.avail.polygon$NestID, 11*ceiling(mean(laying.gps.used.length$Total)*1.25))
colnames(laying.vhf.avail.points)[1] <- "geometry"
st_geometry(laying.vhf.avail.points) <- "geometry"

#Save relevant polygons/points as shapefiles
st_write(laying.vhf.avail.polygon, dsn = "./GIS", layer = "laying.vhf.avail.polygon", driver = "ESRI Shapefile", delete_layer = T)
st_write(laying.vhf.avail.points, dsn = "./GIS", layer = "laying.vhf.avail.points", driver = "ESRI Shapefile", delete_layer = T)




####################################################
### Nest Site Selection Used and Available Sites ###
####################################################
### Used = Nest Location
### Available = PreLaying Home Range

## GPS BIRDS ##
# USED
gps.nests.xy <- gps_nests %>% dplyr::select(NestID, x = NestLong, y = NestLat)
gps.nest.used.nests <- st_as_sf(gps.nests.xy,
                                coords = c("x", "y"), crs = 4326)
nest.gps.used.points <- st_transform(gps.nest.used.nests, 32619)
st_write(nest.gps.used.points, dsn = "./GIS", layer = "nest.gps.used.points", driver = "ESRI Shapefile", delete_layer = T)

# AVAILABLE
#Format timestamps for movebank
nest.gps.avail.polygon <- prelaying.gps.used.polygon
#Parallel sampling from within line buffers
registerDoParallel(numCores)
nest.gps.avail.points <- foreach(i=1:nrow(nest.gps.avail.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(nest.gps.avail.polygon[i,],
                          exact = T,
                          by_polygon = F,
                          size = 15,
                          crs = 4326))
}
stopImplicitCluster()
nest.gps.avail.points <- do.call(rbind, nest.gps.avail.points)
nest.gps.avail.points$NestID <- rep(nest.gps.avail.polygon$NestID, 15)
colnames(nest.gps.avail.points)[1] <- "geometry"
st_geometry(nest.gps.avail.points) <- "geometry"

#Save relevant files as shapefiles/csv
st_write(nest.gps.avail.points, dsn = "./GIS", layer = "nest.gps.avail.points", driver = "ESRI Shapefile", delete_layer = T)
st_write(nest.gps.avail.polygon, dsn = "./GIS", layer = "nest.gps.avail.polygon", driver = "ESRI Shapefile", delete_layer = T)


## VHF BIRDS ##
# USED
vhf.nests.xy <- vhf_nests %>% dplyr::select(NestID, x = NestLong, y = NestLat)
nest.vhf.used.points <- st_as_sf(vhf.nests.xy,
                                  coords = c("x", "y"), crs = 4326)
nest.vhf.used.points <- st_transform(nest.vhf.used.points, 32619)
st_write(nest.vhf.used.points, dsn = "./GIS", layer = "nest.vhf.used.points", driver = "ESRI Shapefile", delete_layer = T)

# AVAILABLE
nest.vhf.avail.buffer.area <- mean(st_area(nest.gps.avail.polygon))*1.25
nest.vhf.avail.buffer.radius <- (nest.vhf.avail.buffer.area/pi)^(1/2)
nest.vhf.avail.polygon <- st_buffer(nest.vhf.used.points, nest.vhf.avail.buffer.radius)
#Parallel sampling from within line buffers
registerDoParallel(numCores)
nest.vhf.avail.points <- foreach(i=1:nrow(nest.vhf.avail.polygon)) %dopar% {
  sf::st_sf(sf::st_sample(nest.vhf.avail.polygon[i,],
                          exact = T,
                          by_polygon = F,
                          size = 15,
                          crs = 4326))
}
stopImplicitCluster()
nest.vhf.avail.points <- do.call(rbind, nest.vhf.avail.points)
nest.vhf.avail.points$NestID <- rep(nest.vhf.avail.polygon$NestID, 15)
colnames(nest.vhf.avail.points)[1] <- "geometry"
st_geometry(nest.vhf.avail.points) <- "geometry"

#Save relevant files as shapefiles/csv
st_write(nest.vhf.avail.points, dsn = "./GIS", layer = "nest.vhf.avail.points", driver = "ESRI Shapefile", delete_layer = T)
st_write(nest.vhf.avail.polygon, dsn = "./GIS", layer = "nest.vhf.avail.polygon", driver = "ESRI Shapefile", delete_layer = T)

