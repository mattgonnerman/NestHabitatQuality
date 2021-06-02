### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel'), require, character.only = T)

memory.limit(56000)

#Weight for logistic regression
w <- 10000

# ##################################################################################
# ### Prelaying Selection
# # prelaying.covs <- st_read("./GIS/Prelaying_Covs_Z.shp")
# prelaying.covs <- st_read("./GIS/Prelaying_Covs.shp") %>%
#   arrange(NestID, Used)
# prelaying.df <- st_drop_geometry(prelaying.covs)
# 
# 
# yPL <- prelaying.covs$Used
# weightsPL <- ifelse(yPL == "0", w, 1)
# cov_PLSel <- as.matrix(st_drop_geometry(prelaying.covs[,which(grepl(covname, colnames(prelaying.covs)))]))
# Ind_PLSel <- as.numeric(as.factor(prelaying.covs$NestID))
# NInd_PLSel <- length(unique(prelaying.covs$NestID))
# N_PLSel <- nrow(prelaying.covs)
# 
# 
# ##################################################################################
# ### Laying Selection
# # laying.covs <- st_read("./GIS/Laying_Covs_Z.shp")
# laying.covs <- st_read("./GIS/Laying_Covs.shp") %>%
#   arrange(NestID, Used)
# 
# yL <- laying.covs$Used
# weightsL <- ifelse(yL == "0", w, 1)
# cov_LSel <- as.matrix(st_drop_geometry(laying.covs[,which(grepl(covname, colnames(laying.covs)))]))
# Ind_LSel <- as.numeric(as.factor(laying.covs$NestID))
# NInd_LSel <- length(unique(laying.covs$NestID))
# N_LSel <- nrow(laying.covs)


##################################################################################
### Nest Site Selection
# nest.covs <- st_read("./GIS/Nest_Covs_Z.shp")
nest.covs <- st_read("./GIS/Nest_Covs.shp") %>%
  arrange(NestID, Used)

# yN <- nest.covs$Used
# wtN <- ifelse(yN == "0", w, 1)
# cov_NSel <- st_drop_geometry(nest.covs[,which(grepl(covname, colnames(nest.covs)))])
# cov_N_F <- match(unique(nest.covs$NestID), nest.covs$NestID)
# cov_N_L <- c(cov_N_F[2:length(cov_N_F)]-1, nrow(cov_NSel))
# NInd_NSel <- length(unique(nest.covs$NestID))

nest.df <- st_drop_geometry(nest.covs)
yN <- as.matrix(nest.df %>% dplyr::select(NestID, Used) %>%
  group_by(NestID) %>% mutate(Number = row_number()) %>%
  dcast(NestID ~ Number, value.var = "Used") %>%
  dplyr::select(-NestID))
weightsN <- ifelse(yN == "0", w, ifelse(yN == 1, 1, NA))
nNLocs <- rowSums(!is.na(yN))
NInd_NSel <- nrow(yN)

cov_NSel <- st_drop_geometry(nest.covs[,c(1,which(grepl(covname, colnames(nest.covs))))])

cov_NSel <- split(cov_NSel[,2:5], cov_NSel$NestID)

covstoarray <- function(covlist){
  maxrows <- max(sapply(cov_NSel, nrow))
  for(i in 1:length(covlist)){
    if(nrow(covlist[[i]]) != maxrows){covlist[[i]][(nrow(covlist[[i]])+1):maxrows,] <- NA}
  }
  
  array(as.numeric(unlist(covlist)), dim=c(nrow(covlist[[1]]), 4, length(covlist)))
}

cov_NSel <- covstoarray(cov_NSel)





# ##################################################################################
# ### Nest Success
# NDSR_order <- ns_eh_matrix$NestID
# # nestsuccess.covs <- st_read("./GIS/NestSuccess_Covs_Z.shp") %>%
# #   arrange(match(NestID, NDSR_order))
# nestsuccess.covs <- st_read("./GIS/NestSuccess_Covs.shp") %>%
#   arrange(match(NestID, NDSR_order))
# cov_NDSR <- as.matrix(st_drop_geometry(nestsuccess.covs[,which(grepl(covname, colnames(nestsuccess.covs)))]))
# 
# 
# # ##################################################################################
# # ### Habitat Covariates
# # source(file = "NestHabitatQuality - 1d Habitat Covariates Data.R")
# 
# 
# ##################################################################################
# ### Full Raster Values for Final NHQ metric
# NHQ.covs <- st_read("./GIS/NHQ_covs.shp")
# NHQ.covs <- st_transform(NHQ.covs, 32619)
# 
# # #>>>For testing purposes, filter to small portion of study area
# testextent <- st_read("./GIS/testrasterextent.shp") %>%
#   mutate(Test = 1)
# testextent <- st_transform(testextent, 32619)
# testextent <- st_buffer(testextent, 5000)
# NHQ.covs <- st_intersection(NHQ.covs,testextent)
# #<<<For testing purposes, filter to small portion of study area
# 
# cov_NHQ <- as.matrix(st_drop_geometry(NHQ.covs[,which(grepl(covname, colnames(NHQ.covs)))]))
