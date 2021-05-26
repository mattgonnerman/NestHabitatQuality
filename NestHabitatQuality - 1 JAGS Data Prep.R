### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel'), require, character.only = T)

memory.limit(56000)


# ##################################################################################
# ### Prelaying Selection
# # prelaying.covs <- st_read("./GIS/Prelaying_Covs_Z.shp")
# prelaying.covs <- st_read("./GIS/Prelaying_Covs.shp")
# 
# yPL <- prelaying.covs$Used
# weightsPL <- ifelse(yPL == "0", 1000, 1)
# cov_PLSel <- as.matrix(st_drop_geometry(prelaying.covs[,which(grepl(covname, colnames(prelaying.covs)))]))
# Ind_PLSel <- as.numeric(as.factor(prelaying.covs$NestID))
# NInd_PLSel <- length(unique(prelaying.covs$NestID))
# N_PLSel <- nrow(prelaying.covs)
# 
# 
# ##################################################################################
# ### Laying Selection
# # laying.covs <- st_read("./GIS/Laying_Covs_Z.shp")
# laying.covs <- st_read("./GIS/Laying_Covs.shp")
# 
# yL <- laying.covs$Used
# weightsL <- ifelse(yL == "0", 1000, 1)
# cov_LSel <- as.matrix(st_drop_geometry(laying.covs[,which(grepl(covname, colnames(laying.covs)))]))
# Ind_LSel <- as.numeric(as.factor(laying.covs$NestID))
# NInd_LSel <- length(unique(laying.covs$NestID))
# N_LSel <- nrow(laying.covs)


##################################################################################
### Nest Site Selection
# nest.covs <- st_read("./GIS/Nest_Covs_Z.shp")
nest.covs <- st_read("./GIS/Nest_Covs.shp")

yN <- nest.covs$Used
weightsN <- ifelse(yN == "0", 1000, 1)
cov_NSel <- as.matrix(st_drop_geometry(nest.covs[,which(grepl(covname, colnames(nest.covs)))]))
Ind_NSel <- as.numeric(as.factor(nest.covs$NestID))
NInd_NSel <- length(unique(nest.covs$NestID))
N_NSel <- nrow(nest.covs)


##################################################################################
### Nest Success
NDSR_order <- ns_eh_matrix$NestID
# nestsuccess.covs <- st_read("./GIS/NestSuccess_Covs_Z.shp") %>%
#   arrange(match(NestID, NDSR_order))
nestsuccess.covs <- st_read("./GIS/NestSuccess_Covs.shp") %>%
  arrange(match(NestID, NDSR_order))
cov_NDSR <- as.matrix(st_drop_geometry(nestsuccess.covs[,which(grepl(covname, colnames(nestsuccess.covs)))]))


# ##################################################################################
# ### Hen Survival
# # source(file = "NestHabitatQuality - 1c Hen Survival Data.R")
# 
# 
# ##################################################################################
# ### Habitat Covariates
# source(file = "NestHabitatQuality - 1d Habitat Covariates Data.R")


##################################################################################
### Full Raster Values for Final NHQ metric
NHQ.covs <- st_read("./GIS/NHQ_covs.shp")
NHQ.covs <- st_transform(NHQ.covs, 32619)

# #>>>For testing purposes, filter to small portion of study area
# testextent <- st_read("./GIS/testrasterextent.shp") %>%
#   mutate(Test = 1)
# testextent <- st_transform(testextent, 32619)
# NHQ.covs <- st_intersection(NHQ.covs,testextent)
#<<<For testing purposes, filter to small portion of study area

cov_NHQ <- as.matrix(st_drop_geometry(NHQ.covs[,which(grepl(covname, colnames(NHQ.covs)))]))
