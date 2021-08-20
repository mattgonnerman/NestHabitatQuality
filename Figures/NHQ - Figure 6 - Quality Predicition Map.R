#IDEAS:
#State map, 2 example maps (Bangor and Portland Areas), observed nests on maps
#Also could have side by side NLCD and single NHQ map to show how landcover translates to NHQ

#Load packages
lapply(c('dplyr', 'ggplot2', 'sf', 'raster', 'ggmap'), require, character.only = T)

##############################
### PREPARE MAP COMPONENTS ###
##############################
### NHQ Raster
nhq.values <- read.csv("./Final/Final Model Summary.csv") %>%
  rename(LCL = X95.CI_low, UCL = X95.CI_upp) %>%
  mutate(Range = UCL - LCL) %>% 
  filter(grepl(x = X, pattern = "NHQ"))

NHQ.covs <- st_read("./GIS/NHQ_covs.shp")
NHQ.covs <- st_transform(NHQ.covs, 32619)
NHQ.points <- NHQ.covs %>%
  dplyr::select(geometry) %>%
  mutate(Mean = nhq.values$Mean,
         Median = nhq.values$Median,
         LCL = nhq.values$LCL,
         UCL = nhq.values$UCL,
         Err_Range = UCL-LCL,)
NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 500, ext = extend(extent(NHQ.points), 2000))
NHQ.raster <- shift(NHQ.raster, dx = 250, dy = 250)
NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = log(NHQ.points$Mean))
NHQ.raster <- crop(NHQ.raster, NHQ.covs)
NHQ.raster <- projectRaster(NHQ.raster, crs = 4326)
NHQ.raster <- crop(NHQ.raster, extent(NHQ.raster, 10, nrow(NHQ.raster)-9, 10, ncol(NHQ.raster)-9))

nhqmean.df <- as.data.frame(xyFromCell(NHQ.raster, 1:ncell(NHQ.raster)))
nhqmean.df$NHQ_logMean <- getValues(NHQ.raster)

### State Boundary
statepoly <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
  filter(LAND == "y",
         ISLAND == "n") 
state.merge <- st_union(statepoly)

### NLCD
NLCD <- raster("E:/Maine Drive/GIS/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
NLCD.adj <- projectRaster(NHQ.raster, crs = crs(NLCD))
NLCD.crop <- crop(NLCD, NLCD.adj)
NLCD.crop <-  projectRaster(NLCD.crop, NHQ.raster)

### Nest Locations
nestsites <- st_read("./GIS/NestSuccess_Covs_Z.shp") %>%
  st_transform(4326) 
nestsites$x <- st_coordinates(nestsites)[,1]
nestsites$y <- st_coordinates(nestsites)[,2]
nestcrop <- st_transform(NHQ.covs, 4326)
nestsites <- st_crop(nestsites, nestcrop)

### City points and Major Roads
cities <- st_centroid(statepoly) %>%
  st_transform(4326)
cities$x <- st_coordinates(cities)[,1]
cities$y <- st_coordinates(cities)[,2]
cities <- st_crop(cities, nestcrop) %>%
  filter(TOWN %in% c("Bangor", "Unity", "Greenfield Twp", "Charleston"))

### Capture Sites
# Load Capture Site Information
capsites.raw <- read.csv("CaptureSites - Sheet1.csv")
capsites.slim <- capsites.raw %>%
  dplyr::select(Town, CapLoc = Location.Name, Lat = Latitude, Long = Longitude)
capsites.sf <- st_as_sf(capsites.slim, coords = c("Long", "Lat"), crs = 4326) %>%
  dplyr::select(-Town)

nestfate.raw <- read.csv("Nest Monitoring - Nest Info.csv")
trap.raw <- read.csv("Trapping - Data.csv") %>% 
  rename(CapLoc = Location) %>%
  filter(AlumBand %in% nestfate.raw$Alum.Band.ID)
capsites.trans <- merge(capsites.sf, trap.raw, by = "CapLoc", all.y = T) %>%
  dplyr::select(CapLoc) %>%
  distinct()
capsites.trans$x <- st_coordinates(capsites.trans)[,1]
capsites.trans$y <- st_coordinates(capsites.trans)[,2]

#################################################
### Map of Study Area depicting NHQ estimates ###
#################################################
# Load and Format Model Estimates


myMap <- get_stamenmap(bbox = c(left = min(nhqmean.df$x),
                                bottom = min(nhqmean.df$y),
                                right = max(nhqmean.df$x),
                                top = max(nhqmean.df$y)),
                       maptype = "terrain-background",
                       crop = T,
                       zoom = 11,
                       color = "color")


ggmap(myMap) +
  geom_tile(data = nhqmean.df, aes(fill = NHQ_logMean)) +
  # geom_sf(data = nestsites, color = "red", shape = 17, size = 2) +
  # geom_sf(data = capsites.trans, color = "blue", shape = 19, size = 2) +
  # # geom_sf(data = cities, color = "black", shape = 19, size = 2) +
  # geom_sf_text(data = cities, aes(label = TOWN), alpha = .6) +
  # scale_fill_gradientn(name = "Habitat\nQuality\n(log)", colors = terrain.colors(10), na.value = NA) +
  # theme_linedraw() +
  coord_sf(xlim = c(min(nhqmean.df$x[which(!is.na(nhqmean.df$NHQ_logMean))]), max(nhqmean.df$x[which(!is.na(nhqmean.df$NHQ_logMean))])),
            ylim = c(min(nhqmean.df$y[which(!is.na(nhqmean.df$NHQ_logMean))]), max(nhqmean.df$y[which(!is.na(nhqmean.df$NHQ_logMean))])),
           expand = F, label_graticule = "NSEW") +
  ylab("") + xlab("") 

  
