### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel', 'units'), require, character.only = T)

memory.limit(56000)

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

#Load polygon of Maine for use in creating grid
mainepoly <- st_read("E:/Maine Drive/GIS/Maine Polygon/Maine_Boundaries_County_Polygon.shp")
mainepoly <- mainepoly %>% dplyr::select(geometry)
mainepoly <- st_cast(mainepoly, "POLYGON")
mainepoly$Area <- st_area(mainepoly) 
mainepoly <- mainepoly %>%
  arrange(desc(Area)) %>%
  filter(Area > as_units(84187782, "m2"))

### Create points to extract raster values to
NHQpoints <- st_as_sf(st_make_grid(mainepoly, cellsize = 1000, what = "centers"))

#Create an R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sf)

# Run parallelized 'extract' function and stop cluster
NHQ.extract <- sfSapply(rasterlist, extract, y=NHQpoints)
colnames(NHQ.extract) <- names(rasterlist)
sfStop()

NHQ.covs <- st_intersection(NHQ.covs,mainepoly)
NHQ.covs <- NHQ.covs %>% dplyr::select(-Area)

NHQ.covs <- cbind(NHQpoints, NHQ.extract) %>%
  filter(!is.na(D2Road_foc4)) %>%
  filter(!is.na(HT_foc4)) %>%
  filter(!is.na(D2Rip_foc4))
st_write(NHQ.covs, "./GIS/NHQ_covs.shp", delete_layer = T)
NHQ.covs <- st_read("./GIS/NHQ_covs.shp")
