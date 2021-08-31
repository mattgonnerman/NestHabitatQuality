#Load packages
lapply(c('dplyr', 'ggplot2', 'patchwork', 'tidyr', 'sf', 'ggmap', 'cowplot'), require, character.only = T)

laying.gps.avail <- st_read("./GIS/laying.gps.avail.polygon.shp") %>%
  filter(NestID == "1616-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
laying.gps.used <- st_read("./GIS/laying.gps.used.polygon.shp") %>%
  filter(NestID == "1616-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
laying.vhf.avail <- st_read("./GIS/laying.vhf.avail.polygon.shp") %>%
  filter(NestID == "449-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
laying.vhf.used <- st_read("./GIS/laying.vhf.used.polygon.shp") %>%
  filter(NestID == "449-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)

prelaying.gps.avail <- st_read("./GIS/prelaying.gps.avail.polygon.shp") %>%
  filter(NestID == "1616-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
prelaying.gps.used <- st_read("./GIS/prelaying.gps.used.polygon.shp") %>%
  filter(NestID == "1616-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
prelaying.vhf.avail <- st_read("./GIS/prelaying.vhf.avail.polygon.shp") %>%
  filter(NestID == "449-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
prelaying.vhf.used <- st_read("./GIS/prelaying.vhf.used.polygon.shp") %>%
  filter(NestID == "449-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)

nest.gps.avail <- st_read("./GIS/nest.gps.avail.polygon.shp") %>%
  filter(NestID == "1616-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
nest.gps.used <- st_read("./GIS/nest.gps.used.points.shp") %>%
  filter(NestID == "1616-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
nest.vhf.avail <- st_read("./GIS/nest.vhf.avail.polygon.shp") %>%
  filter(NestID == "449-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)
nest.vhf.used <- st_read("./GIS/nest.vhf.used.points.shp") %>%
  filter(NestID == "449-2020-1") %>%
  st_transform(4326) %>%
  dplyr::select(NestID)

capsites <- st_read("./GIS/CaptureSites.shp")%>%
  st_transform(4326)

### GPS MAP
gps.cap <- st_join(capsites, prelaying.gps.avail, join = st_within) %>% filter(CapLoc == "Bradbury")
gps.bbox.obj <- rbind(laying.gps.avail, laying.gps.used, prelaying.gps.avail, prelaying.gps.used)
gpsbounds <- as.numeric(st_bbox(gps.bbox.obj))
gpsMap <- get_stamenmap(bbox = c(left = gpsbounds[1] - .01,
                                bottom = gpsbounds[2] - .01,
                                right = gpsbounds[3] + .01,
                                top = gpsbounds[4] + .01),
                       maptype = "terrain-background",
                       crop = T,
                       zoom = 11,
                       color = "color")

colors <- c(ggsci::pal_locuszoom()(6), "black")

gps.map <- ggmap(gpsMap) +
  geom_sf(data = prelaying.gps.avail, fill = NA, aes(color = colors[1]), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = prelaying.gps.used, fill = NA, aes(color = colors[2]), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.gps.avail, fill = NA, aes(color = colors[3]), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.gps.used, fill = NA, aes(color = colors[4]), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.gps.avail, fill = NA, aes(color = colors[5]), inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.gps.used, aes(color = colors[6]), inherit.aes = FALSE, size = 3) +
  geom_sf(data = gps.cap, aes(color = colors[7]), inherit.aes = FALSE, size = 3) +
  theme_linedraw(base_size = 20) +
  scale_color_manual(name = "Scale", values = colors,
                     labels = c("Prelaying - Available", "Prelaying - Used", "Laying - Available", "Laying - Used",
                                "Nest Site - Available", "Nest Site - Used", "Capture Site")) +
  guides(color = guide_legend(override.aes = list(fill = colors)))
gpslegend <- get_legend(gps.map + theme(legend.position = "bottom",
                                        legend.title = element_blank(),
                                        legend.box.background = element_rect(colour = "black",
                                                                             size  = 1.3)))

gps.map <- ggmap(gpsMap) +
  geom_sf(data = prelaying.gps.avail, fill = NA, color = colors[1], inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = prelaying.gps.used, fill = NA, color = colors[2], inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.gps.avail, fill = NA, color = colors[3], inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.gps.used, fill = NA, color = colors[4], inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.gps.avail, fill = NA, color = colors[5], linetype = "dashed", inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.gps.used, color = colors[6], inherit.aes = FALSE, size = 3) +
  geom_sf(data = gps.cap, color = colors[7], inherit.aes = FALSE, size = 3) +
  theme_linedraw(base_size = 15) +
  theme(legend.position = "none",
        axis.title = element_blank())

### VHF MAP
vhf.cap <- st_join(capsites, prelaying.vhf.avail, join = st_within)

vhf.bbox.obj <- rbind(laying.vhf.avail, laying.vhf.used, prelaying.vhf.avail %>% dplyr::select(NestID), prelaying.vhf.used)
vhfbounds <- as.numeric(st_bbox(vhf.bbox.obj))
vhfMap <- get_stamenmap(bbox = c(left = vhfbounds[1] - .01,
                                 bottom = vhfbounds[2] - .01,
                                 right = vhfbounds[3] + .01,
                                 top = vhfbounds[4] + .01),
                        maptype = "terrain-background",
                        crop = T,
                        zoom = 11,
                        color = "color")

vhf.map <- ggmap(vhfMap) +
  geom_sf(data = prelaying.vhf.avail, fill = NA, color = colors[1], inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = prelaying.vhf.used, fill = NA, color = colors[2], inherit.aes = FALSE, lwd = 4) +
  geom_sf(data = laying.vhf.avail, fill = NA, color = colors[3], inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = laying.vhf.used, fill = NA, color = colors[4], inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.vhf.avail, fill = NA, color = colors[5], linetype = "dashed", inherit.aes = FALSE, lwd = 1.2) +
  geom_sf(data = nest.vhf.used, color = colors[6], inherit.aes = FALSE, size = 3) +
  geom_sf(data = vhf.cap, color = colors[7], inherit.aes = FALSE, size = 3) +
  theme_linedraw(base_size = 15) +
  theme(legend.position = "none",
        axis.title = element_blank())
# vhf.map

require(cowplot)

bothplots <- plot_grid(gps.map, vhf.map,
                       ncol = 2, labels = "AUTO",
                       rel_widths = c(1.5, 1))



finalplot <- plot_grid(bothplots, gpslegend, ncol = 1, rel_heights = c(7,1))
# finalplot

ggsave(finalplot, file = "./Figures/Fig2 - Example Used and Avail.jpg",
       height = 10, width = 15)
