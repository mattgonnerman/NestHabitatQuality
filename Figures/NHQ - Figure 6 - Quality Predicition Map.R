#IDEAS:
#State map, 2 example maps (Bangor and Portland Areas), observed nests on maps
#Also could have side by side NLCD and single NHQ map to show how landcover translates to NHQ

#Load packages
lapply(c('dplyr', 'ggplot2', 'sf', 'raster', 'ggmap'), require, character.only = T)

######################################
### INDIVIDUAL COMPONENT ESTIMATES ###
######################################
### Get Beta Estimates
beta.values <- read.csv("./Final/Final Model Summary.csv") %>%
  rename(LCL = X95.CI_low, UCL = X95.CI_upp) %>%
  mutate(Range = UCL - LCL) %>% 
  filter(!grepl(x = X, pattern = "NHQ"))
z.vals <- read.csv("./ZStand_MeanSD.csv")

NHQ.covs <- st_read("./GIS/NHQ_covs.shp") %>%
  mutate(Prob_PLS = exp(((ag_foc3-z.vals$Mean[1])/z.vals$SD[1])*beta.values$Mean[41] + ((dev_fc1-z.vals$Mean[2])/z.vals$SD[2])*beta.values$Mean[42] +
                          ((shrb_f1-z.vals$Mean[3])/z.vals$SD[3])*beta.values$Mean[43] + ((hrb_fc1-z.vals$Mean[4])/z.vals$SD[4])*beta.values$Mean[44] +
                          ((BA_foc1-z.vals$Mean[5])/z.vals$SD[5])*beta.values$Mean[45] + ((HT_foc1-z.vals$Mean[6])/z.vals$SD[6])*beta.values$Mean[46] +
                          ((SW_foc1-z.vals$Mean[7])/z.vals$SD[7])*beta.values$Mean[47] + ((D2Edg_2-z.vals$Mean[8])/z.vals$SD[8])*beta.values$Mean[48] + 
                          ((D2Rd_f1-z.vals$Mean[9])/z.vals$SD[9])*beta.values$Mean[49] + ((D2Rp_f2-z.vals$Mean[10])/z.vals$SD[10])*beta.values$Mean[50])/(1+exp(((ag_foc3-z.vals$Mean[1])/z.vals$SD[1])*beta.values$Mean[41] + ((dev_fc1-z.vals$Mean[2])/z.vals$SD[2])*beta.values$Mean[42] +
                                                                                                                                                                  ((shrb_f1-z.vals$Mean[3])/z.vals$SD[3])*beta.values$Mean[43] + ((hrb_fc1-z.vals$Mean[4])/z.vals$SD[4])*beta.values$Mean[44] +
                                                                                                                                                                  ((BA_foc1-z.vals$Mean[5])/z.vals$SD[5])*beta.values$Mean[45] + ((HT_foc1-z.vals$Mean[6])/z.vals$SD[6])*beta.values$Mean[46] +
                                                                                                                                                                  ((SW_foc1-z.vals$Mean[7])/z.vals$SD[7])*beta.values$Mean[47] + ((D2Edg_2-z.vals$Mean[8])/z.vals$SD[8])*beta.values$Mean[48] + 
                                                                                                                                                                  ((D2Rd_f1-z.vals$Mean[9])/z.vals$SD[9])*beta.values$Mean[49] + ((D2Rp_f2-z.vals$Mean[10])/z.vals$SD[10])*beta.values$Mean[50]))) %>%
  mutate(Prob_LS = exp(((ag_foc1-z.vals$Mean[11])/z.vals$SD[11])*beta.values$Mean[11] + ((dev_fc4-z.vals$Mean[12])/z.vals$SD[12])*beta.values$Mean[12] +
                          ((shrb_f4-z.vals$Mean[13])/z.vals$SD[13])*beta.values$Mean[13] + ((hrb_fc1-z.vals$Mean[14])/z.vals$SD[14])*beta.values$Mean[14] +
                          ((BA_foc1-z.vals$Mean[15])/z.vals$SD[15])*beta.values$Mean[15] + ((HT_foc1-z.vals$Mean[16])/z.vals$SD[16])*beta.values$Mean[16] +
                          ((SW_foc3-z.vals$Mean[17])/z.vals$SD[17])*beta.values$Mean[17] + ((D2Edg_2-z.vals$Mean[18])/z.vals$SD[18])*beta.values$Mean[18] + 
                          ((D2Rd_f4-z.vals$Mean[19])/z.vals$SD[19])*beta.values$Mean[19] + ((D2Rp_f3-z.vals$Mean[20])/z.vals$SD[20])*beta.values$Mean[20])/(1+exp(((ag_foc1-z.vals$Mean[11])/z.vals$SD[11])*beta.values$Mean[11] + ((dev_fc4-z.vals$Mean[12])/z.vals$SD[12])*beta.values$Mean[12] +
                                                                                                                                                                    ((shrb_f4-z.vals$Mean[13])/z.vals$SD[13])*beta.values$Mean[13] + ((hrb_fc1-z.vals$Mean[14])/z.vals$SD[14])*beta.values$Mean[14] +
                                                                                                                                                                    ((BA_foc1-z.vals$Mean[15])/z.vals$SD[15])*beta.values$Mean[15] + ((HT_foc1-z.vals$Mean[16])/z.vals$SD[16])*beta.values$Mean[16] +
                                                                                                                                                                    ((SW_foc3-z.vals$Mean[17])/z.vals$SD[17])*beta.values$Mean[17] + ((D2Edg_2-z.vals$Mean[18])/z.vals$SD[18])*beta.values$Mean[18] + 
                                                                                                                                                                    ((D2Rd_f4-z.vals$Mean[19])/z.vals$SD[19])*beta.values$Mean[19] + ((D2Rp_f3-z.vals$Mean[20])/z.vals$SD[20])*beta.values$Mean[20]))) %>%
  mutate(Prob_NS = exp(((ag_foc4-z.vals$Mean[21])/z.vals$SD[21])*beta.values$Mean[31] + ((dev_fc2-z.vals$Mean[22])/z.vals$SD[22])*beta.values$Mean[32] +
                         ((shrb_f2-z.vals$Mean[23])/z.vals$SD[23])*beta.values$Mean[33] + ((hrb_fc4-z.vals$Mean[24])/z.vals$SD[24])*beta.values$Mean[34] +
                         ((BA_foc1-z.vals$Mean[25])/z.vals$SD[25])*beta.values$Mean[35] + ((HT_foc1-z.vals$Mean[26])/z.vals$SD[26])*beta.values$Mean[36] +
                         ((SW_foc4-z.vals$Mean[27])/z.vals$SD[27])*beta.values$Mean[37] + ((D2Edg_4-z.vals$Mean[28])/z.vals$SD[28])*beta.values$Mean[38] + 
                         ((D2Rd_f1-z.vals$Mean[29])/z.vals$SD[29])*beta.values$Mean[39] + ((D2Rp_f3-z.vals$Mean[30])/z.vals$SD[30])*beta.values$Mean[40])/(1+exp(((ag_foc4-z.vals$Mean[21])/z.vals$SD[21])*beta.values$Mean[31] + ((dev_fc2-z.vals$Mean[22])/z.vals$SD[22])*beta.values$Mean[32] +
                                                                                                                                                                   ((shrb_f2-z.vals$Mean[23])/z.vals$SD[23])*beta.values$Mean[33] + ((hrb_fc4-z.vals$Mean[24])/z.vals$SD[24])*beta.values$Mean[34] +
                                                                                                                                                                   ((BA_foc1-z.vals$Mean[25])/z.vals$SD[25])*beta.values$Mean[35] + ((HT_foc1-z.vals$Mean[26])/z.vals$SD[26])*beta.values$Mean[36] +
                                                                                                                                                                   ((SW_foc4-z.vals$Mean[27])/z.vals$SD[27])*beta.values$Mean[37] + ((D2Edg_4-z.vals$Mean[28])/z.vals$SD[28])*beta.values$Mean[38] + 
                                                                                                                                                                   ((D2Rd_f1-z.vals$Mean[29])/z.vals$SD[29])*beta.values$Mean[39] + ((D2Rp_f3-z.vals$Mean[30])/z.vals$SD[30])*beta.values$Mean[40]))) %>%
  mutate(NDFR = exp(beta.values$Mean[52] + ((ag_foc3-z.vals$Mean[31])/z.vals$SD[31])*beta.values$Mean[21] + ((dev_fc4-z.vals$Mean[32])/z.vals$SD[32])*beta.values$Mean[22] +
                         ((shrb_f2-z.vals$Mean[33])/z.vals$SD[33])*beta.values$Mean[23] + ((hrb_fc4-z.vals$Mean[34])/z.vals$SD[34])*beta.values$Mean[24] +
                         ((BA_foc4-z.vals$Mean[35])/z.vals$SD[35])*beta.values$Mean[25] + ((HT_foc1-z.vals$Mean[36])/z.vals$SD[36])*beta.values$Mean[26] +
                         ((SW_foc1-z.vals$Mean[37])/z.vals$SD[37])*beta.values$Mean[27] + ((D2Edg_2-z.vals$Mean[38])/z.vals$SD[38])*beta.values$Mean[28] + 
                         ((D2Rd_f1-z.vals$Mean[39])/z.vals$SD[39])*beta.values$Mean[29] + ((D2Rp_f3-z.vals$Mean[40])/z.vals$SD[40])*beta.values$Mean[30])) %>%
  mutate(HDMR = exp(beta.values$Mean[51] + ((ag_foc3-z.vals$Mean[41])/z.vals$SD[41])*beta.values$Mean[1] + ((dev_fc3-z.vals$Mean[42])/z.vals$SD[42])*beta.values$Mean[2] +
                      ((shrb_f4-z.vals$Mean[43])/z.vals$SD[43])*beta.values$Mean[3] + ((hrb_fc4-z.vals$Mean[44])/z.vals$SD[44])*beta.values$Mean[4] +
                      ((BA_foc3-z.vals$Mean[45])/z.vals$SD[45])*beta.values$Mean[5] + ((HT_foc2-z.vals$Mean[46])/z.vals$SD[46])*beta.values$Mean[6] +
                      ((SW_foc3-z.vals$Mean[47])/z.vals$SD[47])*beta.values$Mean[7] + ((D2Edg_3-z.vals$Mean[48])/z.vals$SD[48])*beta.values$Mean[8] + 
                      ((D2Rd_f1-z.vals$Mean[49])/z.vals$SD[49])*beta.values$Mean[9] + ((D2Rp_f3-z.vals$Mean[50])/z.vals$SD[50])*beta.values$Mean[10])) %>%
  mutate(Prob_NDSR = ((1/(1+NDFR+HDMR))^40))
           
##################################
### PREPARE NHQ MAP COMPONENTS ###
##################################
### NHQ Raster
nhq.values <- read.csv("./Final/Final Model Summary.csv") %>%
  rename(LCL = X95.CI_low, UCL = X95.CI_upp) %>%
  mutate(Range = UCL - LCL) %>% 
  filter(grepl(x = X, pattern = "NHQ"))

# NHQ.covs <- st_read("./GIS/NHQ_covs.shp")
# NHQ.covs <- st_transform(NHQ.covs, 4326)
# NHQ.points <- NHQ.covs %>%
#   dplyr::select(geometry) %>%
#   mutate(Mean = nhq.values$Mean,
#          Median = nhq.values$Median,
#          LCL = nhq.values$LCL,
#          UCL = nhq.values$UCL,
#          Err_Range = UCL-LCL,)
# nhqmean.df <- as.data.frame(st_coordinates(NHQ.points)) %>%
#   rename(lon = X, lat = Y)
# nhqmean.df$NHQ_Mean <- NHQ.points$Mean

NHQ.covs <- st_transform(NHQ.covs, 32619)
NHQ.points <- NHQ.covs %>%
  dplyr::select(Prob_PLS, Prob_LS, Prob_NS, Prob_NDSR, geometry) %>%
  # mutate(NonBayMean = Prob_PLS * Prob_LS * Prob_NS * Prob_NDSR) %>%
  mutate(Mean = nhq.values$Mean,
         Median = nhq.values$Median,
         LCL = nhq.values$LCL,
         UCL = nhq.values$UCL,
         Err_Range = UCL-LCL,)
NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 500, ext = extend(extent(NHQ.points), 2000))
NHQ.raster <- shift(NHQ.raster, dx = 250, dy = 250)
#NHQ Metric
NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$Mean)
NHQ.raster <- crop(NHQ.raster, NHQ.covs)
NHQ.raster <- projectRaster(NHQ.raster, crs = 4326)
NHQ.raster <- crop(NHQ.raster, extent(NHQ.raster, 10, nrow(NHQ.raster)-9, 10, ncol(NHQ.raster)-9))
nhqmean.df <- as.data.frame(xyFromCell(NHQ.raster, 1:ncell(NHQ.raster))) %>%
  rename(lon = x, lat = y)
nhqmean.df$NHQ_Mean <- getValues(NHQ.raster)
#PLS Estimate
NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 500, ext = extend(extent(NHQ.points), 2000))
NHQ.raster <- shift(NHQ.raster, dx = 250, dy = 250)
NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$Prob_PLS)
NHQ.raster <- crop(NHQ.raster, NHQ.covs)
NHQ.raster <- projectRaster(NHQ.raster, crs = 4326)
NHQ.raster <- crop(NHQ.raster, extent(NHQ.raster, 10, nrow(NHQ.raster)-9, 10, ncol(NHQ.raster)-9))
nhqmean.df$PLS_Mean <- getValues(NHQ.raster)
#LS Estimate
NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 500, ext = extend(extent(NHQ.points), 2000))
NHQ.raster <- shift(NHQ.raster, dx = 250, dy = 250)
NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$Prob_LS)
NHQ.raster <- crop(NHQ.raster, NHQ.covs)
NHQ.raster <- projectRaster(NHQ.raster, crs = 4326)
NHQ.raster <- crop(NHQ.raster, extent(NHQ.raster, 10, nrow(NHQ.raster)-9, 10, ncol(NHQ.raster)-9))
nhqmean.df$LS_Mean <- getValues(NHQ.raster)
#NS Estimate
NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 500, ext = extend(extent(NHQ.points), 2000))
NHQ.raster <- shift(NHQ.raster, dx = 250, dy = 250)
NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$Prob_NS)
NHQ.raster <- crop(NHQ.raster, NHQ.covs)
NHQ.raster <- projectRaster(NHQ.raster, crs = 4326)
NHQ.raster <- crop(NHQ.raster, extent(NHQ.raster, 10, nrow(NHQ.raster)-9, 10, ncol(NHQ.raster)-9))
nhqmean.df$NS_Mean <- getValues(NHQ.raster)
#NDSR Estimate
NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 500, ext = extend(extent(NHQ.points), 2000))
NHQ.raster <- shift(NHQ.raster, dx = 250, dy = 250)
NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$Prob_NDSR)
NHQ.raster <- crop(NHQ.raster, NHQ.covs)
NHQ.raster <- projectRaster(NHQ.raster, crs = 4326)
NHQ.raster <- crop(NHQ.raster, extent(NHQ.raster, 10, nrow(NHQ.raster)-9, 10, ncol(NHQ.raster)-9))
nhqmean.df$NDSR_Mean <- getValues(NHQ.raster)

# ### State Boundary
# statepoly <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp") %>%
#   filter(LAND == "y",
#          ISLAND == "n") 
# state.merge <- st_union(statepoly)
# 
# ### NLCD
# NLCD <- raster("E:/Maine Drive/GIS/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img")
# NLCD.adj <- projectRaster(NHQ.raster, crs = crs(NLCD))
# NLCD.crop <- crop(NLCD, NLCD.adj)
# NLCD.crop <-  projectRaster(NLCD.crop, NHQ.raster)

### Nest Locations
nestsites <- st_read("./GIS/NestSuccess_Covs_Z.shp") %>%
  st_transform(4326) 
nestsites$x <- st_coordinates(nestsites)[,1]
nestsites$y <- st_coordinates(nestsites)[,2]
nestcrop <- st_transform(NHQ.covs, 4326)
nestsites <- st_crop(nestsites, nestcrop) %>%
  rename(lat = y, lon = x)

### City points and Major Roads
cities <- st_centroid(statepoly) %>%
  st_transform(4326)
cities$lon <- st_coordinates(cities)[,1]
cities$lat <- st_coordinates(cities)[,2]
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
capsites.trans$lon <- st_coordinates(capsites.trans)[,1]
capsites.trans$lat <- st_coordinates(capsites.trans)[,2]

#################################################
### Map of Study Area depicting NHQ estimates ###
#################################################
# Load and Format Model Estimates


# myMap <- get_stamenmap(bbox = c(left = min(nhqmean.df$lon),
#                                 bottom = min(nhqmean.df$lat),
#                                 right = max(nhqmean.df$lon),
#                                 top = max(nhqmean.df$lat)),
#                        maptype = "terrain-background",
#                        crop = T,
#                        zoom = 11,
#                        color = "color")


# predict.map <- ggmap(myMap) +
#   geom_tile(data = nhqmean.df, aes(fill = NHQ_Mean)) +
#   geom_sf(data = nestsites, color = "red", shape = 17, size = 2) +
#   geom_sf(data = capsites.trans, color = "blue", shape = 19, size = 2) +
#   # geom_sf(data = cities, color = "black", shape = 19, size = 2) +
#   geom_sf_text(data = cities, aes(label = TOWN), alpha = .6, size = 7) +
#   scale_fill_gradientn(name = "Habitat\nQuality", colors = terrain.colors(10), na.value = NA) +
#   theme_linedraw(base_size = 24) +
#   coord_sf(xlim = c(min(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))])),
#             ylim = c(min(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))])),
#            expand = F, label_graticule = "NSEW") +
#   ylab("") + xlab("")

nhq.plot <- ggplot(data = nhqmean.df, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = NHQ_Mean)) +
  geom_sf(data = nestsites, color = "red", shape = 17, size = 5) +
  geom_sf(data = capsites.trans, color = "blue", shape = 19, size = 5) +
  # geom_sf(data = cities, color = "black", shape = 19, size = 2) +
  geom_sf_text(data = cities, aes(label = TOWN), alpha = .6, size = 15) +
  scale_fill_gradientn(name = "Habitat\nQuality",
                       breaks = seq(0,1,.1),
                       colors = terrain.colors(10), na.value = NA) +
  theme_linedraw(base_size = 44) +
  coord_sf(xlim = c(min(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))])),
            ylim = c(min(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))])),
           expand = F, label_graticule = "SW") +
  ylab("") + xlab("")


ggsave(predict.map, file = "./Figures/Fig6 - Prediction Surface.jpg",
       height = 16, width = 24)


######################
### Series of Maps ###
######################
pls.plot <- ggplot(data = nhqmean.df, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = PLS_Mean)) +
  scale_fill_gradientn(name = "Habitat\nQuality",
                       breaks = seq(0,1,.1),
                       colors = terrain.colors(10), na.value = NA) +
  theme_linedraw(base_size = 24) +
  coord_sf(xlim = c(min(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))])),
           ylim = c(min(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))])),
           expand = F) +
  ylab("") + xlab("") +
  theme(legend.position = "none") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ls.plot <- ggplot(data = nhqmean.df, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = LS_Mean)) +
  scale_fill_gradientn(name = "Habitat\nQuality",
                       breaks = seq(0,1,.1),
                       colors = terrain.colors(10), na.value = NA) +
  theme_linedraw(base_size = 24) +
  coord_sf(xlim = c(min(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))])),
           ylim = c(min(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))])),
           expand = F) +
  ylab("") + xlab("") +
  theme(legend.position = "none") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ns.plot <- ggplot(data = nhqmean.df, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = NS_Mean)) +
  scale_fill_gradientn(name = "Habitat\nQuality",
                       breaks = seq(0,1,.1),
                       colors = terrain.colors(10), na.value = NA) +
  theme_linedraw(base_size = 24) +
  coord_sf(xlim = c(min(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))])),
           ylim = c(min(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))])),
           expand = F) +
  ylab("") + xlab("") +
  theme(legend.position = "none") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ndsr.plot <- ggplot(data = nhqmean.df, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = NDSR_Mean)) +
  scale_fill_gradientn(name = "Habitat\nQuality",
                       breaks = seq(0,1,.1),
                       colors = terrain.colors(10), na.value = NA) +
  theme_linedraw(base_size = 24) +
  coord_sf(xlim = c(min(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lon[which(!is.na(nhqmean.df$NHQ_Mean))])),
           ylim = c(min(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))]), max(nhqmean.df$lat[which(!is.na(nhqmean.df$NHQ_Mean))])),
           expand = F) +
  ylab("") + xlab("") +
  theme(legend.position = "none") + 
  theme(axis.text = element_blank(),
    axis.ticks = element_blank())

require(patchwork)
row1.plots <- (pls.plot | ls.plot)
row2.plots <- (ns.plot | ndsr.plot)
small.plots <- row1.plots/row2.plots
row2.plots/nhq.plot

final.fig <- wrap_plots(row1.plots, row2.plots, nhq.plot, nrow = 3,
           heights = c(1,1,2))

ggsave(final.fig, file = "./Figures/Fig6 - Habitat Quality Maps.jpg", 
       width = 28, height = 30)
