### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel', 'reshape2'), require, character.only = T)

memory.limit(56000)

#Weight for logistic regression
w <- 10000

##################################################################################
### Prelaying Selection
pl.covs <- st_read("./GIS/Prelaying_Covs.shp") %>%
  arrange(NestID, Used)

pl.used <- pl.covs %>% filter(Used == 1) %>%
  group_by(NestID) %>%
  mutate(PairID = row_number())
pl.avail <- pl.covs %>% filter(Used == 0) %>%
  group_by(NestID) %>%
  mutate(PairID = ceiling(row_number()/10)) %>%  #10 for PL and L, 100 for N
  ungroup() %>%
  mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  group_by(ID) %>%
  slice(1:10) %>%
  dplyr::select(-ID)

pl.df <- st_drop_geometry(rbind(pl.used, pl.avail)) %>%
  mutate(PairID2 = paste(NestID, PairID, sep = "_")) %>%
  arrange(PairID2, desc(Used)) %>%
  group_by(PairID2) %>%
  filter(!all(Used != 1)) %>%
  filter(!all(Used != 0)) %>%
  ungroup()

y_PL <- as.matrix(pl.df %>% dplyr::select(PairID2, Used) %>%
                  group_by(PairID2) %>% mutate(Number = row_number()) %>%
                  dcast(PairID2 ~ Number, value.var = "Used") %>%
                  dplyr::select(-PairID2))
weightsPL <- ifelse(y_PL == "0", w, ifelse(y_PL == 1, 1, NA))
nPLLocs <- rowSums(!is.na(y_PL))
NInd_PLSel <- nrow(y_PL)

foridpair1 <- pl.df %>% dplyr::select(PairID2, Used) %>%
                    group_by(PairID2) %>% mutate(Number = row_number()) %>%
                    dcast(PairID2 ~ Number, value.var = "Used")
foridpair2 <- pl.df %>% dplyr::select(NestID, PairID2) %>% distinct()
foridpair <- merge(foridpair1, foridpair2, all.x = T, all.y = F, by = "PairID2")
Ind_PLSel <- as.numeric(as.factor(foridpair$NestID))
N_PLSel <- length(unique(Ind_PLSel))


##################################################################################
### Laying Selection
#Read the laying covariate point shapefile
l.covs <- st_read("./GIS/Laying_Covs.shp") %>%
  arrange(NestID, Used)

l.used <- l.covs %>% filter(Used == 1) %>%
  group_by(NestID) %>%
  mutate(PairID = row_number())
l.avail <- l.covs %>% filter(Used == 0) %>%
  group_by(NestID) %>%
  mutate(PairID = ceiling(row_number()/10)) %>%  #10 for PL and L, 100 for N
  ungroup() %>%
  mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  group_by(ID) %>%
  slice(1:10) %>%
  dplyr::select(-ID)

l.df <- st_drop_geometry(rbind(l.used, l.avail)) %>%
  mutate(PairID2 = paste(NestID, PairID, sep = "_")) %>%
  arrange(PairID2, desc(Used)) %>%
  group_by(PairID2) %>%
  filter(!all(Used != 1)) %>%
  filter(!all(Used != 0)) %>%
  ungroup()

y_L <- as.matrix(l.df %>% dplyr::select(PairID2, Used) %>%
                    group_by(PairID2) %>% mutate(Number = row_number()) %>%
                    dcast(PairID2 ~ Number, value.var = "Used") %>%
                    dplyr::select(-PairID2))
weightsL <- ifelse(y_L == "0", w, ifelse(y_L == 1, 1, NA))
nLLocs <- rowSums(!is.na(y_L))
NInd_LSel <- nrow(y_L)
foridpair1 <- l.df %>% dplyr::select(PairID2, Used) %>%
  group_by(PairID2) %>% mutate(Number = row_number()) %>%
  dcast(PairID2 ~ Number, value.var = "Used")
foridpair2 <- l.df %>% dplyr::select(NestID, PairID2) %>% distinct()
foridpair <- merge(foridpair1, foridpair2, all.x = T, all.y = F, by = "PairID2")
Ind_LSel <- as.numeric(as.factor(foridpair$NestID))
N_LSel <- length(unique(Ind_LSel))


##################################################################################
### Nest Site Selection
nest.covs <- st_read("./GIS/Nest_Covs.shp") %>%
  arrange(NestID, Used)

nest.used <- nest.covs %>% filter(Used == 1) %>%
  group_by(NestID) %>%
  mutate(PairID = row_number())
nest.avail <- nest.covs %>% filter(Used == 0) %>%
  group_by(NestID) %>%
  mutate(PairID = ceiling(row_number()/10)) %>%  #10 for PL and L, 100 for N
  ungroup() %>%
  mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  group_by(ID) %>%
  slice(1:10) %>%
  dplyr::select(-ID)

nest.df <- st_drop_geometry(rbind(nest.used, nest.avail)) %>%
  mutate(PairID2 = paste(NestID, PairID, sep = "_")) %>%
  arrange(PairID2, desc(Used)) %>%
  group_by(PairID2) %>%
  filter(!all(Used != 1)) %>%
  filter(!all(Used != 0)) %>%
  ungroup()

y_N <- as.matrix(nest.df %>% dplyr::select(PairID2, Used) %>%
                   group_by(PairID2) %>% mutate(Number = row_number()) %>%
                   dcast(PairID2 ~ Number, value.var = "Used") %>%
                   dplyr::select(-PairID2))
class(y_N) <- "numeric"
weightsN <- ifelse(y_N == "0", w, ifelse(y_N == 1, 1, NA))
nNLocs <- rowSums(!is.na(y_N))
NInd_NSel <- nrow(y_N)
foridpair1 <- nest.df %>% dplyr::select(PairID2, Used) %>%
  group_by(PairID2) %>% mutate(Number = row_number()) %>%
  dcast(PairID2 ~ Number, value.var = "Used")
foridpair2 <- nest.df %>% dplyr::select(NestID, PairID2) %>% distinct()
foridpair <- merge(foridpair1, foridpair2, all.x = T, all.y = F, by = "PairID2")
Ind_NSel <- unique(as.numeric(as.factor(foridpair$NestID)))
N_NSel <- length(unique(Ind_NSel))


##################################################################################
### Nest Success
source(file = "NestHabitatQuality - 1b Nest Success Data.R")
NDSR_order <- ns_eh_matrix$NestID

nestsuccess.covs <- st_read("./GIS/NestSuccess_Covs.shp") %>%
  arrange(match(NestID, NDSR_order))


##################################################################################
### Full Raster Values for Final NHQ metric
# NHQ.covs <- st_read("./GIS/NHQ_covs.shp")
# NHQ.covs <- st_transform(NHQ.covs, 32619)


##################################################################################
### Change what you can to integer to preserve some space
class(y_PL) <- "integer"
class(y_L) <- "integer"
class(y_N) <- "integer"
class(weightsPL) <- "integer"
class(weightsL) <- "integer"
class(weightsN) <- "integer"
class(ns_succ.mat) <- "integer"
class(ns_interval) <- "integer"
class(Ind_PLSel) <- "integer"
class(NInd_PLSel) <- "integer"
class(N_PLSel) <- "integer"
class(Ind_LSel) <- "integer"
class(NInd_LSel) <- "integer"
class(N_LSel) <- "integer"
class(Ind_NSel) <- "integer"
class(NInd_NSel) <- "integer"
class(N_NSel) <- "integer"
class(ns_ID) <- "integer"


##################################################################################
### Loop through individual covariates to create data files
for(i in 4:8){
  covname <- covSelnames[i]
  
  #Pre-Laying Selection
  cov_PLSel <- st_drop_geometry(pl.covs[,c(1,which(grepl(covname, colnames(pl.covs))))])
  cov_PLSel <- merge(st_drop_geometry(pl.used) %>% dplyr::select(NestID, PairID), cov_PLSel, all.y = T) %>%
    mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
    group_by(ID) %>%
    dplyr::select(-NestID, -PairID)
  cov_PLSel <- split(cov_PLSel[,1:4], cov_PLSel$ID)
  cov_PLSel <- array(as.numeric(unlist(cov_PLSel)), dim=c(11, 4, length(cov_PLSel)))
  
  #Laying Selection
  cov_LSel <- st_drop_geometry(l.covs[,c(1,which(grepl(covname, colnames(l.covs))))])
  cov_LSel <- merge(st_drop_geometry(l.used) %>% dplyr::select(NestID, PairID), cov_LSel, all.y = T) %>%
    mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
    group_by(ID) %>%
    dplyr::select(-NestID, -PairID)
  cov_LSel <- split(cov_LSel[,1:4], cov_LSel$ID)
  cov_LSel <- array(as.numeric(unlist(cov_LSel)), dim=c(11, 4, length(cov_LSel)))
  
  #Nest Selection
  cov_NSel <- st_drop_geometry(nest.covs[,c(1,which(grepl(covname, colnames(nest.covs))))])
  cov_NSel <- merge(st_drop_geometry(nest.used) %>% dplyr::select(NestID, PairID), cov_NSel, all.y = T) %>%
    mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
    group_by(ID) %>%
    dplyr::select(-NestID, -PairID)
  cov_NSel <- split(cov_NSel[,1:4], cov_NSel$ID)
  cov_NSel <- array(as.numeric(unlist(cov_NSel)), dim=c(11, 4, length(cov_NSel)))
  
  #Nest Success
  cov_NDSR <- as.matrix(st_drop_geometry(nestsuccess.covs[,which(grepl(covname, colnames(nestsuccess.covs)))]))
  
  #NHQ
  # cov_NHQ <- as.matrix(st_drop_geometry(NHQ.covs[,which(grepl(covname, colnames(NHQ.covs)))]))

  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - NSel.R")
  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - LSel.R")
  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - PLSel.R")
  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - NDSR.R")
  # source(file = "NestHabitatQuality - 2N NIMBLE Prep.R")
}





