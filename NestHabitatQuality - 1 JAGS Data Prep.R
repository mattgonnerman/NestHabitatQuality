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
N_PLSel <- nrow(prelaying.covs.z)


##################################################################################
### Laying Selection
laying.covs.z <- st_drop_geometry(st_read("./GIS/Laying_Covs_Z.shp"))

yL <- laying.covs.z$Used
cov_LSel <- as.matrix(st_drop_geometry(laying.covs.z[,which(grepl(covname, colnames(laying.covs.z)))]))
Ind_LSel <- as.numeric(as.factor(laying.covs.z$NestID))
NInd_LSel <- length(unique(laying.covs.z$NestID))
N_LSel <- nrow(laying.covs.z)


##################################################################################
### Nest Site Selection
nest.covs.z <- st_drop_geometry(st_read("./GIS/Nest_Covs_Z.shp"))

yN <- nest.covs.z$Used
cov_NSel <- as.matrix(st_drop_geometry(nest.covs.z[,which(grepl(covname, colnames(nest.covs.z)))]))
Ind_NSel <- as.numeric(as.factor(nest.covs.z$NestID))
NInd_NSel <- length(unique(nest.covs.z$NestID))
N_NSel <- nrow(nest.covs.z)


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