###NEED A CSV WITH THE EXACT TIMESTAMP FOR EACH NEST INITIATION
###FOR VHF BIRDS THAT NESTED THE YEAR AFTER CAPTURE, NEED TO CHOOSE A BETTER POINT FOR WINTER LOCATION BESIDES CAPTURE SITE
### MULTIPLE GPS BIRDS DONT HAVE NEST INFO BUT DEFINITELY NESTED



### Load relevant packages
lapply(c('dplyr', 'sf', 'move', 'raster', 'lubridate', "ggplot2"), require, character.only = T)

#Login info to download data from Movebank directly
login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")

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
prelaying.gps.used.times <- gps_nest_info %>%
  filter(!is.na(NestYear)) %>%
  dplyr::select(NestID, BirdID, NestYear, StartTimestamp = PrenestStart, EndTimestamp = FirstNestVisit) %>%
  mutate(StartTimestamp = gsub("[^0-9]", "", StartTimestamp)) %>%
  mutate(EndTimestamp = gsub("[^0-9]", "", EndTimestamp))

rm("prelaying.gps.used.polygon")
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
  t_turkeygps <- spTransform(turkeygps, center=T)
  turk_dBBMM <- brownian.bridge.dyn(t_turkeygps, raster = 30, location.error = 17, margin = 5, window.size = 15, ext = 1.5)
  turkey_UD <-raster2contour(turk_dBBMM, level=c(.95))
  dBBMM_line <- st_as_sf(turkey_UD, "SpatialLines")
  dBBMM_poly <- st_cast(dBBMM_line, "POLYGON")
  dBBMM_poly$NestID <- nestid
  
  if(exists("prelaying.gps.used.polygon")){
    dBBMM_poly <- st_transform(dBBMM_poly, st_crs(prelaying.gps.used.polygon))
    prelaying.gps.used.polygon <- rbind(prelaying.gps.used.polygon, dBBMM_poly)
  }else{
    prelaying.gps.used.polygon <- dBBMM_poly
  }
}

prelaying.gps.used


# AVAILABLE
prelaying.gps.avail.times <- gps_nest_info %>%
  filter(!is.na(NestYear)) %>%
  dplyr::select(NestID, BirdID, NestYear, EndTimestamp = FirstNestVisit) %>%
  mutate(StartTimestamp = paste(NestYear, "0101000000000", sep = "")) %>%
  mutate(EndTimestamp = gsub("[^0-9]", "", EndTimestamp))

rm("prelaying.gps.avail.polygon")
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


## VHF BIRDS ##
#Create an ellipse using capture to nest information, extend distance 125%
vhf.nests.xy <- vhf_nests %>% dplyr::select(NestID, x = NestLong, y = NestLat)
vhf.cap.xy <- vhf_nests %>% dplyr::select(NestID, x = CapLong, y = CapLat)
vhf.xy <- rbind(vhf.nests.xy, vhf.cap.xy) %>%
  group_by(NestID) %>%
  split(.,.[,"NestID"])
vhf.xy.mat <- lapply(vhf.xy, function(x) as.matrix(x[names(x) %in% c("x","y")]))
vhf.multilines.sfg <- st_multilinestring(vhf.xy.mat, dim = "XY")
vhf.multilines.sfc <- st_sfc(vhf.multilines.sfg)
vhf.lines.sfc <- st_cast(vhf.multilines.sfc, "LINESTRING")
vhf.captonest.line <- st_sf(data.frame(vhf.lines.sfc, vhf_nests))
buffer2 <- ifelse(st_length(vhf.captonest.line) < mean(st_length(vhf.captonest.line)), mean(st_length(vhf.captonest.line)),st_length(vhf.captonest.line))
vhf.captonest.buffer <- st_buffer(vhf.captonest.line, buffer2*.25)
vhf.captonest.used <- st_buffer(vhf.captonest.line, mean(st_length(vhf.captonest.line))*.5)
vhf.captonest.avail <- st_buffer(vhf.captonest.line, mean(st_length(vhf.captonest.line))*1.5)

ggplot() +
  geom_sf(data = vhf.captonest.avail, alpha = .5, color = "red") +
  geom_sf(data = vhf.captonest.used, aes(), alpha = .5, color = "green") +
  geom_sf(data = vhf.captonest.line, color = "blue")


#################################################
### Laying Selection Used and Available Sites ###
#################################################
### Used = Laying Home Range
### Available = PreLaying Home Range 

## GPS BIRDS ##
#Load dataframe with prenesting range start (dispersal/winter end) and end (incubation starts) dates 
#Convert to movebank format.
GPSpreneststart <- read.csv("GPSPrenestStart.csv")

#Loop through

## VHF BIRDS ##





#############################################
### Nest Site Selection Used and Available Sites ###
#############################################
### Used = Nest Location
### Available = PreLaying Home Range

## GPS BIRDS ##


## VHF BIRDS ##




