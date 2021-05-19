### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel'), require, character.only = T)

memory.limit(56000)
##################################################################################
### Prelaying Selection
prelaying.covs.z <- st_read("./GIS/Prelaying_Covs_Z.shp")

yPL <- prelaying.covs.z$Used
cov_PLSel <- as.matrix(st_drop_geometry(prelaying.covs.z[,which(grepl(covname, colnames(prelaying.covs.z)))]))
Ind_PLSel <- as.numeric(as.factor(prelaying.covs.z$NestID))
NInd_PLSel <- length(unique(prelaying.covs.z$NestID))


# ##################################################################################
# ### Laying Selection
# laying.covs.z <- st_drop_geometry(st_read("./GIS/Laying_Covs_Z.shp"))
# 
# 
# ##################################################################################
# ### Nest Site Selection
# nest.covs.z <- st_drop_geometry(st_read("./GIS/Nest_Covs_Z.shp"))
# 
# 
# ##################################################################################
# ### Nest Success
# source(file = "NestHabitatQuality - 1b Nest Success Data.R")
# 
# 
# ##################################################################################
# ### Hen Survival
# # source(file = "NestHabitatQuality - 1c Hen Survival Data.R")
# 
# 
# ##################################################################################
# ### Habitat Covariates
# source(file = "NestHabitatQuality - 1d Habitat Covariates Data.R")