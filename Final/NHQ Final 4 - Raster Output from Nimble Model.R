require("dplyr")
require("ggplot2")
require("raster")

df <- read.csv("./Final/Final Model Summary.csv")

NHQ.covs <- st_read("./GIS/NHQ_covs.shp")

### Examine Model Outputs
#Create NHQ Raster from Nimble Model Outputs, no formatting necessary
createNHQraster <- function(df){
  rawdf <- as.data.frame(df)
  NHQest.df <- rawdf %>% 
    filter(grepl(x = X, pattern = "NHQ"))
  NHQest <- NHQest.df$Mean
  NHQ.points <- NHQ.covs %>%
    dplyr::select(geometry)
  NHQ.points$NHQ <- NHQest
  NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 30, ext = extent(NHQ.points))
  NHQ.raster <- shift(NHQ.raster, dx = 1, dy = 1)
  NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$NHQ)
  NHQ.raster
}

modelraster <- createNHQraster(rasterinput)
plot(NHQ.raster)

ggplot(data = NHQ.raster) +
  geom_raster()
