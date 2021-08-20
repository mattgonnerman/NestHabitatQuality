#Load packages
lapply(c('dplyr', 'ggplot2', 'patchwork', 'tidyr', 'sf', 'ggmap'), require, character.only = T)

laying.gps.avail <- st_read("./GIS/laying.gps.avail.polygon.shp") %>%
  filter(NestID == "286-2018-1") %>%
  st_transform(4326)
laying.gps.used <- st_read("./GIS/laying.gps.used.polygon.shp") %>%
  filter(NestID == "286-2018-1") %>%
  st_transform(4326)
laying.vhf.avail <- st_read("./GIS/laying.vhf.avail.polygon.shp") %>%
  filter(NestID == "362-2018-1") %>%
  st_transform(4326)
laying.vhf.used <- st_read("./GIS/laying.vhf.used.polygon.shp") %>%
  filter(NestID == "362-2018-1") %>%
  st_transform(4326)

prelaying.gps.avail <- st_read("./GIS/prelaying.gps.avail.polygon.shp") %>%
  filter(NestID == "286-2018-1") %>%
  st_transform(4326)
prelaying.gps.used <- st_read("./GIS/prelaying.gps.used.polygon.shp") %>%
  filter(NestID == "286-2018-1") %>%
  st_transform(4326)
prelaying.vhf.avail <- st_read("./GIS/prelaying.vhf.avail.polygon.shp") %>%
  filter(NestID == "362-2018-1") %>%
  st_transform(4326)
prelaying.vhf.used <- st_read("./GIS/prelaying.vhf.used.polygon.shp") %>%
  filter(NestID == "362-2018-1") %>%
  st_transform(4326)

nest.gps.avail <- st_read("./GIS/nest.gps.avail.polygon.shp") %>%
  filter(NestID == "286-2018-1") %>%
  st_transform(4326)
nest.gps.used <- st_read("./GIS/nest.gps.used.points.shp") %>%
  filter(NestID == "286-2018-1") %>%
  st_transform(4326)
nest.vhf.avail <- st_read("./GIS/nest.vhf.avail.polygon.shp") %>%
  filter(NestID == "362-2018-1") %>%
  st_transform(4326)
nest.vhf.used <- st_read("./GIS/nest.vhf.used.points.shp") %>%
  filter(NestID == "362-2018-1") %>%
  st_transform(4326)

capsites <- st_read("./GIS/CaptureSites.shp")

myMap <- get_stamenmap(bbox = c(left = -68.415,
                                bottom = 44.92,
                                right = -68.365,
                                top = 44.963),
                       maptype = "terrain-background",
                       crop = T,
                       zoom = 11,
                       color = "color")

ggmap(myMap) +
  # coord_sf(crs = st_crs(32619)) + # force the ggplot2 map to be in 3857
  geom_sf(data = prelaying.gps.avail, inherit.aes = FALSE) +
  geom_sf(data = prelaying.gps.used, inherit.aes = FALSE) +
  geom_sf(data = laying.gps.avail, inherit.aes = FALSE) +
  geom_sf(data = laying.gps.used, inherit.aes = FALSE) +
  geom_sf(data = nest.gps.avail, inherit.aes = FALSE) +
  geom_sf(data = nest.gps.used, inherit.aes = FALSE) 
