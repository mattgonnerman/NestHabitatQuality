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

capsites <- st_read("./GIS/CaptureSites.shp")%>%
  st_transform(4326)

### GPS MAP
gpsMap <- get_stamenmap(bbox = c(left = -68.415,
                                bottom = 44.92,
                                right = -68.365,
                                top = 44.963),
                       maptype = "terrain-background",
                       crop = T,
                       zoom = 11,
                       color = "color")

gps.map <- ggmap(gpsMap) +
  coord_sf(crs = st_crs(32619)) + # force the ggplot2 map to be in 3857
  geom_sf(data = prelaying.gps.avail, fill = NA, aes(color = "Prelaying - Available"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = prelaying.gps.used, fill = NA, aes(color = "Prelaying - Used"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.gps.avail, fill = NA, aes(color = "Laying - Available"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.gps.used, fill = NA, aes(color = "Laying - Used"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.gps.avail, fill = NA, aes(color = "Nest Site - Available"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.gps.used, aes(color = "Nest Site - Used"), inherit.aes = FALSE, size = 3) +
  geom_sf(data = capsites, aes(color = "Capture Site"), inherit.aes = FALSE, size = 3) +
  scale_color_manual(name = "Scale", values = c("#003f5c", "#374c80", "#7a5195","#ef5675","#ef5675","#ff764a", "#ffa600"),
                     labels = c("Capture Site", "Prelaying - Available", "Prelaying - Used",
                                "Laying - Available", "Laying - Used", "Nest Site - Available", "Nest Site - Used")) +
  # scale_fill_manual(name = "Scale", values = alpha(c("#003f5c", "#374c80", "#7a5195","#ef5675","#ef5675","#ff764a", "#ffa600"), 0),
  #                    labels = c("Capture Site", "Prelaying - Available", "Prelaying - Used",
  #                               "Laying - Available", "Laying - Used", "Nest Site - Available", "Nest Site - Used")) +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(fill = c("#003f5c", "#374c80", "#7a5195","#ef5675","#ef5675","#ff764a", "#ffa600"))))


### GPS MAP
vhfMap <- get_stamenmap(bbox = c(left = -68.47,
                                bottom = 45.05,
                                right = -68.3,
                                top = 45.085 ),
                       maptype = "terrain-background",
                       crop = T,
                       zoom = 11,
                       color = "color")

vhf.map <- ggmap(vhfMap) +
  coord_sf(crs = st_crs(32619)) + # force the ggplot2 map to be in 3857
  geom_sf(data = prelaying.vhf.avail, fill = NA, aes(color = "Prelaying - Available"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = prelaying.vhf.used, fill = NA, aes(color = "Prelaying - Used"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.vhf.avail, fill = NA, aes(color = "Laying - Available"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.vhf.used, fill = NA, aes(color = "Laying - Used"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.vhf.avail, fill = NA, aes(color = "Nest Site - Available"), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.vhf.used, aes(color = "Nest Site - Used"), inherit.aes = FALSE, size = 3) +
  geom_sf(data = capsites, aes(color = "Capture Site"), inherit.aes = FALSE, size = 3) +
  scale_color_manual(name = "Scale", values = c("#003f5c", "#374c80", "#7a5195","#ef5675","#ef5675","#ff764a", "#ffa600"),
                     labels = c("Capture Site", "Prelaying - Available", "Prelaying - Used",
                                "Laying - Available", "Laying - Used", "Nest Site - Available", "Nest Site - Used")) +
  # scale_fill_manual(name = "Scale", values = alpha(c("#003f5c", "#374c80", "#7a5195","#ef5675","#ef5675","#ff764a", "#ffa600"), 0),
  #                    labels = c("Capture Site", "Prelaying - Available", "Prelaying - Used",
  #                               "Laying - Available", "Laying - Used", "Nest Site - Available", "Nest Site - Used")) +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(fill = c("#003f5c", "#374c80", "#7a5195","#ef5675","#ef5675","#ff764a", "#ffa600"))))

vhf.map
