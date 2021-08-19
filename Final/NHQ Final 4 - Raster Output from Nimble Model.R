require("dplyr")
require("ggplot2")
require("raster")

df <- read.csv("./Final/Final Model Summary.csv") %>%
  rename(LCL = X95.CI_low, UCL = X95.CI_upp) %>%
  mutate(Range = UCL - LCL)

NHQ.covs <- st_read("./GIS/NHQ_covs.shp")

NHQest.df <- df %>% 
  filter(grepl(x = X, pattern = "NHQ"))

NHQ.points <- NHQ.covs %>%
  dplyr::select(geometry)

NHQ.covs$FinalNHQMean <- NHQest.df$Mean
NHQ.covs$FinalNHQMedian <- NHQest.df$Median
NHQ.covs$FinalNHQLCL <- NHQest.df$LCL
NHQ.covs$FinalNHQUCL <- NHQest.df$UCL
NHQ.covs$FinalNHQRange <- NHQest.df$Range

NHQpointsmean <- NHQ.covs %>% dplyr::select(FinalNHQMean)
NHQpointsmed <- NHQ.covs %>% dplyr::select(FinalNHQMedian)
NHQpointslcl <- NHQ.covs %>% dplyr::select(FinalNHQLCL)
NHQpointsucl <- NHQ.covs %>% dplyr::select(FinalNHQUCL)
NHQpointsrange <- NHQ.covs %>% dplyr::select(FinalNHQRange)

NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 30, ext = extent(NHQ.points))
NHQ.raster <- shift(NHQ.raster, dx = 1, dy = 1)
NHQ.raster.mean <- rasterize(st_coordinates(NHQpointsmean)[,1:2], NHQ.raster, field = NHQpointsmean$FinalNHQMean)
NHQ.raster.med <- rasterize(st_coordinates(NHQpointsmed)[,1:2], NHQ.raster, field = NHQpointsmed$FinalNHQMedian)
NHQ.raster.lcl <- rasterize(st_coordinates(NHQpointslcl)[,1:2], NHQ.raster, field = NHQpointslcl$FinalNHQLCL)
NHQ.raster.ucl <- rasterize(st_coordinates(NHQpointsucl)[,1:2], NHQ.raster, field = NHQpointsucl$FinalNHQUCL)
NHQ.raster.range <- rasterize(st_coordinates(NHQpointsrange)[,1:2], NHQ.raster, field = NHQpointsrange$FinalNHQRange)

writeRaster(NHQ.raster.mean, filename = "./Final/NHQ_Mean.tif", format = "GTiff")
writeRaster(NHQ.raster.med, filename = "./Final/NHQ_Med.tif", format = "GTiff")
writeRaster(NHQ.raster.lcl, filename = "./Final/NHQ_LCL.tif", format = "GTiff")
writeRaster(NHQ.raster.ucl, filename = "./Final/NHQ_UCL.tif", format = "GTiff")
writeRaster(NHQ.raster.range, filename = "./Final/NHQ_Range.tif", format = "GTiff")



### Examine Model Outputs
#Create NHQ Raster from Nimble Model Outputs, no formatting necessary
# createNHQraster <- function(df){
#   rawdf <- as.data.frame(df)
#   
#   NHQest <- NHQest.df$Mean
#   NHQ.points <- NHQ.covs %>%
#     dplyr::select(geometry)
#   NHQ.points$NHQ <- NHQest
#   NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 30, ext = extent(NHQ.points))
#   NHQ.raster <- shift(NHQ.raster, dx = 1, dy = 1)
#   NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$NHQ)
#   NHQ.raster
# }
