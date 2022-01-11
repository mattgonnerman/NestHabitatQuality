#Load packages
lapply(c('dplyr', 'ggplot2', 'patchwork', 'tidyr', 'sf', 'ggmap'), require, character.only = T)

setwd("E:/GitHub/NestHabitatQuality/")
### IDEAS
# Popout map witht the study areas showing capture sites where transmitters were deployed.
# Large map is state map with study areas outlined, general terrain basemap
# Smaller maps have bold town outlines ontop of either NLCD or detailed terrain maps
# 2 colored boxed with different colors on Maine map depicting the study areas

# Load full Maine state boundary
mainepoly <- st_read("E:/Maine Drive/GIS/Maine Polygon/Maine_Boundaries_County_Polygon.shp")

# Load Capture Sites
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

# Load Study Area Polygons
fulltowns <- st_read("E:/Maine Drive/GIS/Maine_Town_and_Townships_Boundary_Polygons_Feature.shp")

cap_in_town <- st_join(capsites.trans, fulltowns, join = st_within)

bothSA <- fulltowns %>%
  filter(TOWN %in% cap_in_town$TOWN)

southSA <- bothSA %>%
  filter(TOWN %in% c("Buckfield", "Gray", "Gorham", "Poland"))

northSA <- bothSA %>%
  filter(TOWN %in% setdiff(bothSA$TOWN, southSA$TOWN))

southSA_bb = st_as_sfc(st_bbox(southSA))
northSA_bb = st_as_sfc(st_bbox(northSA))

# State Map
state_coords <- as.numeric(st_bbox(mainepoly))

# Nest Locations
nestsites <- read.csv("Nest Monitoring - Nest Info.csv") %>%
  filter(Nest.Attempt == 1) %>%
  filter(!is.na(NestLat)) %>%
  filter(!is.na(Alum.Band.ID)) %>%
  dplyr::select(NestID, NestLat, NestLong) %>%
  mutate(lat = NestLat, lon = NestLong) %>%
  st_as_sf(coords = c("NestLong", "NestLat"), crs = 4326)

# stateMap <- get_stamenmap(bbox = c(left = state_coords[1],
#                                    bottom = state_coords[2],
#                                    right = state_coords[3],
#                                    top = state_coords[4]),
#                           maptype = "terrain-background",
#                           crop = T,
#                           zoom = 1,
#                           color = "color")


state.plot <- ggplot(data = mainepoly) +
  geom_sf(fill = NA) +
  geom_sf(data = southSA_bb, fill = NA, color = "#006cad", lwd = 1.2) +
  geom_sf(data = northSA_bb, fill = NA, color = "#009203", lwd = 1.2) +
  geom_sf(data = capsites.trans, size = 2, color = "black") +
  theme_void()

#Northern Study Area
northSA_coords <- as.numeric(st_bbox(northSA))

northMap <- get_stamenmap(bbox = c(left = northSA_coords[1],
                                bottom = northSA_coords[2],
                                right = northSA_coords[3],
                                top = northSA_coords[4]),
                       maptype = "terrain",
                       crop = F,
                       zoom = 11,
                       color = "color")


north.plot <- ggmap(northMap) +
  geom_sf(data = capsites.trans, color = "black", size = 5) +
  geom_sf(data = nestsites, fill = "red", size = 5, shape = 24 ) +
  theme_linedraw(base_size = 30) +
  coord_sf(xlim = northSA_coords[c(1,3)],
            ylim = northSA_coords[c(2,4)],
           expand = F, label_graticule = "SE") + 
  theme(axis.title=element_blank()) +
  ggsn::scalebar(x.min = northSA_coords[1] ,
                 y.min = northSA_coords[2] + .05,
                 x.max = northSA_coords[3] - .05,
                 y.max = northSA_coords[4],
                 dist = 10, st.size=5, st.dist = 0.02,
                 dist_unit = "km", height=0.02, transform = T, model = "WGS84",
                 location = "bottomright") +
  theme(panel.border = element_rect(color = "#009203", size = 2, fill = NA))
# north.plot

#Southern Study Area
southSA_coords <- as.numeric(st_bbox(southSA))

southMap <- get_stamenmap(bbox = c(left = southSA_coords[1],
                                   bottom = southSA_coords[2],
                                   right = southSA_coords[3],
                                   top = southSA_coords[4]),
                          maptype = "terrain",
                          crop = F,
                          zoom = 11,
                          color = "color")


south.plot <- ggmap(southMap) +
  geom_sf(data = capsites.trans, color = "black", size = 5) +
  geom_sf(data = nestsites, fill = "red", size = 5, shape = 24 ) +
  theme_linedraw(base_size = 30) +
  coord_sf(xlim = southSA_coords[c(1,3)],
           ylim = southSA_coords[c(2,4)],
           expand = F, label_graticule = "WS",
           ndiscr = 3) + 
  theme(axis.title=element_blank()) +
  ggsn::scalebar(x.min = southSA_coords[1] + .03,
                 y.min = southSA_coords[2] + .03,
                 x.max = southSA_coords[3],
                 y.max = southSA_coords[4],
                 dist = 4, st.size=5, st.dist = 0.02,
                 dist_unit = "km", height=0.02, transform = T, model = "WGS84",
                 location = "bottomleft") +
  theme(panel.border = element_rect(color = "#006cad", size = 2, fill = NA)) +
  scale_x_continuous(breaks = c(-70.50, -70.30))
# south.plot

# Combine into single figure
require(patchwork)
layout <- "
AABBBBBB
CCBBBBBB
CCBBBBBB
"
combo.plots <- state.plot + north.plot + south.plot + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 30))

# leftplots <- wrap_plots(state.plot, south.plot, nrow = 2,
#            heights = c(1,2)) 
# 
# combo.plots <- wrap_plots(leftplots, north.plot, nrow = 1,
#                         widths = c(1,3), tag_level = 'keep') + 
#   plot_annotation(tag_levels = "A") & 
#   theme(plot.tag = element_text(size = 30))

ggsave(combo.plots, file = "./Figures/Fig1 - Study Areas Map.jpg",
       width = 21, height = 15, dpi = 600)

