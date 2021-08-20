### Final Model ###
# Global Model, look at confidence intervals to determine support

### Load Relevant R packages 
lapply(c('dplyr', 'sf', 'move', 'raster', 'snowfall', 'lubridate', 'ggplot2', 'foreach', 'doParallel', 'reshape2'), require, character.only = T)

memory.limit(56000)

#Weight for RSF infinitely weighted logistic regression
w <- 10000

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
y_PL <- as.matrix(pl.df %>% dplyr::select(MatchID, Used) %>%
                    group_by(MatchID) %>% mutate(Number = row_number()) %>%
                    dcast(MatchID ~ Number, value.var = "Used") %>%
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
### Clean Up Covariates
covSelnames <- c("ag_", "dev_", "shrub_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

scaleresults <- read.csv("./Final/Scale Selection Results.csv")

### Prelaying Covs
scaleresults %>% filter(Component == "PL")
  
cov_PLSel1 <- st_drop_geometry(pl.covs) %>%
  dplyr::select(NestID, MergeID,
                ag_foc3, dev_fc1, shrub_f1, hrb_fc1, BA_foc1,
                HT_foc1, SW_foc1, D2Edg_2, D2Rd_f1, D2Rp_f2)
cov_PLSel2 <- merge(pl.df %>% dplyr::select(NestID, PairID, MergeID, Used), cov_PLSel1, by = c("NestID", "MergeID")) %>%
  arrange(NestID, PairID, desc(Used), MergeID) %>%
  mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  group_by(ID) %>%
  dplyr::select(-NestID, -PairID, -Used, -MergeID)

#Z - Scale
for(i in 1:10){
  cov_PLSel2[,i] <- scale(cov_PLSel2[,i], center = TRUE, scale = TRUE)
}

cov_PLSel3 <- split(cov_PLSel2[,1:10], cov_PLSel2$ID)
cov_PLSel <- array(as.numeric(unlist(cov_PLSel3)), dim=c(11, 10, length(cov_PLSel3)))

### Laying Covs
scaleresults %>% filter(Component == "L")
cov_LSel1 <- st_drop_geometry(l.covs) %>%
  dplyr::select(NestID, MergeID,
                ag_foc1, dev_fc4, shrub_f4, hrb_fc1, BA_foc1,
                HT_foc1, SW_foc3, D2Edg_2, D2Rd_f4, D2Rp_f3)
cov_LSel2 <- merge(l.df %>% dplyr::select(NestID, PairID, MergeID, Used), cov_LSel1, by = c("NestID", "MergeID")) %>%
  arrange(NestID, PairID, desc(Used), MergeID) %>%
  mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  group_by(ID) %>%
  dplyr::select(-NestID, -PairID, -Used, -MergeID)

#Z - Scale
for(i in 1:10){
  cov_LSel2[,i] <- scale(cov_LSel2[,i], center = TRUE, scale = TRUE)
}

cov_LSel3 <- split(cov_LSel2[,1:10], cov_LSel2$ID)
cov_LSel <- array(as.numeric(unlist(cov_LSel3)), dim=c(11, 10, length(cov_LSel3)))

### Nest Site Covs
scaleresults %>% filter(Component == "N")
cov_NSel1 <- st_drop_geometry(nest.covs) %>%
  dplyr::select(NestID, MergeID,
                ag_foc4, dev_fc2, shrub_f2, hrb_fc4, BA_foc1,
                HT_foc1, SW_foc4, D2Edg_4, D2Rd_f1, D2Rp_f3)
cov_NSel2 <- merge(nest.df %>% dplyr::select(NestID, PairID, MergeID, Used), cov_NSel1, by = c("NestID", "MergeID")) %>%
  arrange(NestID, PairID, desc(Used), MergeID) %>%
  mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  group_by(ID) %>%
  dplyr::select(-NestID, -PairID, -Used, -MergeID)

#Z - Scale
for(i in 1:10){
  cov_NSel2[,i] <- scale(cov_NSel2[,i], center = TRUE, scale = TRUE)
}

cov_NSel3 <- split(cov_NSel2[,1:10], cov_NSel2$ID)
cov_NSel <- array(as.numeric(unlist(cov_NSel3)), dim=c(11, 10, length(cov_NSel3)))


#Nest Success
#NDSR
scaleresults %>% filter(Component == "NDSR")
cov_NDSR <- as.matrix(st_drop_geometry(nestsuccess.covs) %>%
  dplyr::select(ag_foc1, dev_fc4, shrub_f2, hrb_fc4, BA_foc4,
                HT_foc1, SW_foc1, D2Edg_2, D2Rd_f2, D2Rp_f3))

#Z - Scale
for(i in 1:10){
  cov_NDSR[,i] <- scale(cov_NDSR[,i], center = TRUE, scale = TRUE)
}

#HDSR
scaleresults %>% filter(Component == "HDSR")
cov_HDSR <- as.matrix(st_drop_geometry(nestsuccess.covs) %>%
                        dplyr::select(ag_foc2, dev_fc4, shrub_f2, hrb_fc4, BA_foc4,
                                      HT_foc1, SW_foc1, D2Edg_2, D2Rd_f2, D2Rp_f3))
#Z - Scale
for(i in 1:10){
  cov_HDSR[,i] <- scale(cov_HDSR[,i], center = TRUE, scale = TRUE)
}

##################################################################################
### NHQ Inputs
NHQ.covs <- st_read("./GIS/NHQ_covs.shp")
NHQ.covs <- st_transform(NHQ.covs, 32619)

nNHQ <- nrow(NHQ.covs)

cov_NHQ <- as.matrix(st_drop_geometry(NHQ.covs))
NHQ_covnames <- c(colnames(cov_PLSel2)[1:10],
                  colnames(cov_LSel2)[1:10],
                  colnames(cov_NSel2)[1:10],
                  colnames(cov_NDSR)[1:10], 
                  colnames(cov_HDSR)[1:10])
NHQ_covnames <- ifelse(NHQ_covnames == "shrub_f1", "shrb_f1", NHQ_covnames)
NHQ_covnames <- ifelse(NHQ_covnames == "shrub_f2", "shrb_f2", NHQ_covnames)
NHQ_covnames <- ifelse(NHQ_covnames == "shrub_f3", "shrb_f3", NHQ_covnames)
NHQ_covnames <- ifelse(NHQ_covnames == "shrub_f4", "shrb_f4", NHQ_covnames)
NHQ_covcol1 <- sapply(NHQ_covnames, function(x){which(colnames(cov_NHQ) == x)})
NHQ_covcol <- c(unlist(NHQ_covcol1))
cov_NHQ <- cov_NHQ[, NHQ_covcol]

#check that NHQ columns line of with covs
cov_col_names <- c(colnames(cov_PLSel2)[1:10], colnames(cov_LSel2)[1:10], colnames(cov_NSel2)[1:10], colnames(cov_NDSR), colnames(cov_HDSR))
NHQ_covnames[which(cov_col_names != NHQ_covnames)]
cov_col_names[which(cov_col_names != NHQ_covnames)]

#need mean and sd for each cov used to scale NHQ values
pl.scale <- pl.df[,colnames(cov_PLSel2)[1:10]]
l.scale <- l.df[,colnames(cov_LSel2)[1:10]]
n.scale <- nest.df[,colnames(cov_NSel2)[1:10]]
ndsr.temp <- st_read("./GIS/NestSuccess_Covs.shp") %>%
  arrange(match(NestID, NDSR_order)) %>%
  st_drop_geometry()
colnames(ndsr.temp)[10:13] <- paste("shrub_f", 1:4, sep = "")
ndsr.scale <- ndsr.temp[,colnames(cov_NDSR)[1:10]]
hdsr.scale <- ndsr.temp[,colnames(cov_HDSR)[1:10]]
pl.sd <- l.sd <- n.sd <- ndsr.sd <- hdsr.sd <- c()
for(i in 1:10){
  pl.sd[i] <- sd(as.data.frame(pl.scale)[,i])
  l.sd[i] <- sd(as.data.frame(l.scale)[,i])
  n.sd[i] <- sd(as.data.frame(n.scale)[,i])
  ndsr.sd[i] <- sd(as.data.frame(ndsr.scale)[,i])
  hdsr.sd[i] <- sd(as.data.frame(hdsr.scale)[,i])
}
           
           
nhq.scale.msd <- data.frame(Comp = c(rep("PLSel",10),rep("LSel",10),rep("NSel",10),rep("NDSR",10),rep("HDSR",10)),
           Cov = c(colnames(pl.scale), colnames(l.scale),colnames(n.scale),colnames(ndsr.scale),colnames(hdsr.scale)),
           Mean = c(colMeans(pl.scale),colMeans(l.scale),colMeans(n.scale),colMeans(ndsr.scale),colMeans(hdsr.scale)),
           SD = c(pl.sd,l.sd,n.sd,ndsr.sd,hdsr.sd))

nhq.scale.msd$Cov == colnames(cov_NHQ)

for(i in 1:nrow(nhq.scale.msd)){
  for(j in 1:nrow(cov_NHQ)){
    cov_NHQ[j,i] <- (cov_NHQ[j,i] - nhq.scale.msd$Mean[i])/nhq.scale.msd$SD[i]
  }
}
