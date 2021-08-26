### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel', 'reshape2'), require, character.only = T)

memory.limit(56000)

#Weight for RSF infinitely weighted logistic regression
w <- 10000

`%notin%` <- Negate(`%in%`)

########################
### DATA PREPARATION ###
########################
#Vectors for cycling through each model component and covariates
compnames <- c("PLSel", "LSel", "NSel", "NDSR")
covSelnames <- c("ag_", "dev_", "shrub_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")
##################################################################################
### Prelaying Selection
#Load selection points
pl.covs <- st_read("./GIS/Prelaying_Covs.shp") %>%
  arrange(NestID, Used) %>%
  mutate(MergeID = row_number())
colnames(pl.covs)[12:15] <- paste("shrub_f", 1:4, sep = "")

#Create Object of Used Locations
pl.used <- pl.covs %>% filter(Used == 1) %>%
  group_by(NestID) %>%
  mutate(PairID = row_number()) %>%
  mutate(MatchID = paste(NestID, PairID, sep = "_"))

#Filter down available locations to just 10 per used point
pl.avail <- pl.covs %>% filter(Used == 0) %>%
  group_by(NestID) %>%
  mutate(PairID = ceiling(row_number()/10)) %>%  #10 for PL and L, 100 for N
  mutate(MatchID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  filter(MatchID %in% pl.used$MatchID) %>%
  ungroup()

#Bind used and available data frames
pl.df <- st_drop_geometry(rbind(pl.used, pl.avail)) %>%
  arrange(NestID, PairID, desc(Used)) %>%
  group_by(MatchID) %>%
  filter(!all(Used != 1)) %>%
  filter(!all(Used != 0)) %>%
  ungroup()

# Matrix designating Used (1) or Available (0)
y_PL <- pl.df %>% dplyr::select(MatchID, Used) %>%
                  group_by(MatchID) %>% mutate(Number = row_number()) %>%
                  dcast(MatchID ~ Number, value.var = "Used")
PL_NAs <- which(!complete.cases(y_PL))

pl.df <- pl.df %>% filter(MatchID %notin% y_PL$MatchID[PL_NAs])
y_PL <- as.matrix(y_PL[-PL_NAs,] %>%
                  dplyr::select(-MatchID))

#Weights for Infinitely Weighted logistic regression
weightsPL <- ifelse(y_PL == "0", w, ifelse(y_PL == 1, 1, NA))

#Indicates number of Locations per used point; commented as all should have 10:1
# nPLLocs <- rowSums(!is.na(y_PL))

#Number of Used:Available groups
NInd_PLSel <- nrow(y_PL)

#Create Nest ID indexing vector
foridpair1 <- pl.df %>% dplyr::select(MatchID, Used) %>%
                    group_by(MatchID) %>% mutate(Number = row_number()) %>%
                    dcast(MatchID ~ Number, value.var = "Used")
foridpair2 <- pl.df %>% dplyr::select(NestID, PairID, MatchID) %>% distinct()
foridpair <- merge(foridpair1, foridpair2, all.x = T, all.y = F, by = "MatchID") %>%
  arrange(NestID, PairID)
Ind_PLSel <- as.numeric(as.factor(foridpair$NestID))

#Total Number of Nests in Analysis
N_PLSel <- length(unique(Ind_PLSel))


##################################################################################
### Laying Selection
#Load selection points
l.covs <- st_read("./GIS/Laying_Covs.shp") %>%
  arrange(NestID, Used)  %>%
  mutate(MergeID = row_number())
colnames(l.covs)[12:15] <- paste("shrub_f", 1:4, sep = "")

#Create Object of Used Locations
l.used <- l.covs %>% filter(Used == 1) %>%
  group_by(NestID) %>%
  mutate(PairID = row_number()) %>%
  mutate(MatchID = paste(NestID, PairID, sep = "_"))

#Filter down available locations to just 10 per used point
l.avail <- l.covs %>% filter(Used == 0) %>%
  group_by(NestID) %>%
  mutate(PairID = ceiling(row_number()/10)) %>%  #10 for PL and L, 100 for N
  mutate(MatchID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  filter(MatchID %in% l.used$MatchID) %>%
  ungroup()

#Bind used and available data frames
l.df <- st_drop_geometry(rbind(l.used, l.avail)) %>%
  arrange(NestID, PairID, desc(Used)) %>%
  group_by(MatchID) %>%
  filter(!all(Used != 1)) %>%
  filter(!all(Used != 0)) %>%
  ungroup()

# Matrix designating Used (1) or Available (0)
y_L <- as.matrix(l.df %>% dplyr::select(MatchID, Used) %>%
                    group_by(MatchID) %>% mutate(Number = row_number()) %>%
                    dcast(MatchID ~ Number, value.var = "Used") %>%
                    dplyr::select(-MatchID))

L_NAs <- which(!complete.cases(y_PL))

#Weights for Infinitely Weighted logistic regression
weightsL <- ifelse(y_L == "0", w, ifelse(y_L == 1, 1, NA))

#Indicates number of Locations per used point; commented as all should have 10:1
# nPLLocs <- rowSums(!is.na(y_PL))

#Number of Used:Available groups
NInd_LSel <- nrow(y_L)

#Create Nest ID indexing vector
foridpair1 <- l.df %>% dplyr::select(MatchID, Used) %>%
  group_by(MatchID) %>% mutate(Number = row_number()) %>%
  dcast(MatchID ~ Number, value.var = "Used")
foridpair2 <- l.df %>% dplyr::select(NestID, PairID, MatchID) %>% distinct()
foridpair <- merge(foridpair1, foridpair2, all.x = T, all.y = F, by = "MatchID") %>%
  arrange(NestID, PairID)
Ind_LSel <- as.numeric(as.factor(foridpair$NestID))

#Total Number of Nests in Analysis
N_LSel <- length(unique(Ind_LSel))


##################################################################################
### Nest Site Selection
#Load selection points
nest.covs <- st_read("./GIS/Nest_Covs.shp") %>%
  arrange(NestID, Used) %>%
  mutate(Used = as.numeric(Used)) %>%
  filter(NestID != "1561-2020-1", NestID != "1563-2020-1") %>% #No Data Sheets, GPS with clear nest though 
  mutate(MergeID = row_number())
colnames(nest.covs)[12:15] <- paste("shrub_f", 1:4, sep = "")

#Create Object of Used Locations
nest.used <- nest.covs %>% filter(Used == 1) %>%
  mutate(PairID = 1)

#Filter down available locations to just 10 per used point
nest.avail <- nest.covs %>% filter(Used == 0) %>%
  group_by(NestID) %>%
  mutate(PairID = row_number()) %>%
  filter(PairID < 11) %>%
  ungroup()

#Bind used and available data frames
nest.df <- st_drop_geometry(rbind(nest.used, nest.avail)) %>%
  arrange(NestID, PairID, desc(Used)) %>%
  group_by(NestID) %>%
  filter(!all(Used != 1)) %>%
  filter(!all(Used != 0)) %>%
  ungroup()

# Matrix designating Used (1) or Available (0)
y_N <- as.matrix(nest.df %>% dplyr::select(NestID, PairID, Used) %>%
                   group_by(NestID) %>% arrange(PairID) %>% mutate(Number = row_number()) %>%
                   dcast(NestID ~ Number, value.var = "Used") %>%
                   dplyr::select(-NestID))

#Weights for Infinitely Weighted logistic regression
weightsN <- ifelse(y_N == "0", w, ifelse(y_N == 1, 1, NA))

#Number of Used:Available groups
NInd_NSel <- nrow(y_N)


##################################################################################
### Nest Success
source(file = "./Old Code/NestHabitatQuality - 1b Nest Success Data.R")
NDSR_order <- ns_eh_matrix$NestID

nestsuccess.covs <- st_read("./GIS/NestSuccess_Covs.shp") %>%
  arrange(match(NestID, NDSR_order))
colnames(nestsuccess.covs)[10:13] <- paste("shrub_f", 1:4, sep = "")

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
class(NInd_NSel) <- "integer"
class(ns_ID) <- "integer"


##################################################################################
### Loop through individual covariates to create data files
covSelnames <- c("ag_", "dev_", "shrub_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

for(i in 1:10){
  covname <- covSelnames[i]
  
  #Pre-Laying Selection
  cov_PLSel1 <- st_drop_geometry(pl.covs[,c(1,which(grepl(covname, colnames(pl.covs))), ncol(pl.covs))])
  cov_PLSel2 <- merge(pl.df %>% dplyr::select(NestID, PairID, MergeID, Used), cov_PLSel1, by = c("NestID", "MergeID")) %>%
    arrange(NestID, PairID, desc(Used), MergeID) %>%
    mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
    group_by(ID) %>%
    dplyr::select(-NestID, -PairID, -Used, -MergeID)
  
  #Z - Scale
  cov_PLSel2[,1] <- scale(cov_PLSel2[,1], center = TRUE, scale = TRUE)
  cov_PLSel2[,2] <- scale(cov_PLSel2[,2], center = TRUE, scale = TRUE)
  cov_PLSel2[,3] <- scale(cov_PLSel2[,3], center = TRUE, scale = TRUE)
  cov_PLSel2[,4] <- scale(cov_PLSel2[,4], center = TRUE, scale = TRUE)
  
  cov_PLSel3 <- split(cov_PLSel2[,1:4], cov_PLSel2$ID)
  cov_PLSel <- array(as.numeric(unlist(cov_PLSel3)), dim=c(11, 4, length(cov_PLSel3)))
  
  #Laying Selection
  cov_LSel1 <- st_drop_geometry(l.covs[,c(1,which(grepl(covname, colnames(l.covs))), ncol(l.covs))])
  cov_LSel2 <- merge(l.df %>% dplyr::select(NestID, PairID, MergeID, Used), cov_LSel1, by = c("NestID", "MergeID")) %>%
    arrange(NestID, PairID, desc(Used), MergeID) %>%
    mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
    group_by(ID) %>%
    dplyr::select(-NestID, -PairID, -Used, -MergeID)
  
  #Z - Scale
  cov_LSel2[,1] <- scale(cov_LSel2[,1], center = TRUE, scale = TRUE)
  cov_LSel2[,2] <- scale(cov_LSel2[,2], center = TRUE, scale = TRUE)
  cov_LSel2[,3] <- scale(cov_LSel2[,3], center = TRUE, scale = TRUE)
  cov_LSel2[,4] <- scale(cov_LSel2[,4], center = TRUE, scale = TRUE)
  
  cov_LSel3 <- split(cov_LSel2[,1:4], cov_LSel2$ID)
  cov_LSel <- array(as.numeric(unlist(cov_LSel3)), dim=c(11, 4, length(cov_LSel3)))
  
  #Nest Selection
  cov_NSel1 <- st_drop_geometry(nest.covs[,c(1,which(grepl(covname, colnames(nest.covs))), ncol(nest.covs))])
  cov_NSel2 <- merge(nest.df %>% dplyr::select(NestID, PairID, MergeID, Used), cov_NSel1, by = c("NestID", "MergeID")) %>%
    arrange(NestID, PairID, desc(Used), MergeID) %>%
    group_by(NestID) %>%
    dplyr::select(-PairID, -Used, -MergeID)
  
  #Z - Scale
  cov_NSel2[,5] <- scale(cov_NSel2[,5], center = TRUE, scale = TRUE)
  cov_NSel2[,2] <- scale(cov_NSel2[,2], center = TRUE, scale = TRUE)
  cov_NSel2[,3] <- scale(cov_NSel2[,3], center = TRUE, scale = TRUE)
  cov_NSel2[,4] <- scale(cov_NSel2[,4], center = TRUE, scale = TRUE)
  
  cov_NSel3 <- split(cov_NSel2[,2:5], cov_NSel2$NestID)
  cov_NSel <- array(as.numeric(unlist(cov_NSel3)), dim=c(11, 4, length(cov_NSel3)))
  
  #Nest Success
  cov_NDSR <- as.matrix(st_drop_geometry(nestsuccess.covs[,which(grepl(covname, colnames(nestsuccess.covs)))]))
  
  #Z - Scale
  cov_NDSR[,1] <- scale(cov_NDSR[,1], center = TRUE, scale = TRUE)
  cov_NDSR[,2] <- scale(cov_NDSR[,2], center = TRUE, scale = TRUE)
  cov_NDSR[,3] <- scale(cov_NDSR[,3], center = TRUE, scale = TRUE)
  cov_NDSR[,4] <- scale(cov_NDSR[,4], center = TRUE, scale = TRUE)
  
  #NHQ
  # cov_NHQ <- as.matrix(st_drop_geometry(NHQ.covs[,which(grepl(covname, colnames(NHQ.covs)))]))

  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - NSel.R")
  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - LSel.R")
  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - PLSel.R")
  source(file = "./NIMBLE IndComp Version/NestHabitatQuality - 2 IndComp NIMBLE Prep - NDSR.R")
  # source(file = "NestHabitatQuality - 2N NIMBLE Prep.R")
}
