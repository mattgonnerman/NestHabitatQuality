lapply(c("dplyr", 'nimble', 'tidyr'), require, character.only = T)

cover.raw <- read.csv("Coverboards.csv") %>%
  select(NestID, PointID = Random.Point.., Scale = Selection.Order, N, E, S, W) %>%
  mutate(PointAvg = (N + E + S + W)/4) %>%
  mutate(Scale = ifelse(is.na(Scale) & PointID == "Nest", "Nest", NA))

##################################################################################
### NEST SITE SELECTION
#Number of Used:Available groups
NInd_NSel <- length(unique(cover.raw$NestID))

# Matrix designating Used (1) or Available (0)
y_N <- matrix(rep(c(1,0,0,0,0,0,0), NInd_NSel), ncol = 7, nrow = NInd_NSel, byrow = T)

#Weights for Infinitely Weighted logistic regression
weightsN <- ifelse(y_N == "0", 1000, ifelse(y_N == 1, 1, NA))

#Nest Site Selection Covariates
cov_NSel <- as.matrix(cover.raw %>% select(NestID, PointID, PointAvg) %>%
                        pivot_wider(names_from = "PointID", id_cols = "NestID", values_from = "PointAvg") %>%
                        select(Nest, '1', '2', '3', '4', '5', '6'))

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({
  
  #############################################################################
  
  ### Nesting Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_NSel ~ dnorm(0, 0.001) 

  ## Likelihood
  for(i in 1:NNest_NSel){
    y_N[i, 1:7] ~ dmulti(p_N[i,1:7], 1)
    for(j in 1:7){
      p_N[i,j] <- e_N[i,j]/inprod(wt_N[i,1:7],e_N[i,1:7])
      log(e_N[i,j]) <- beta_SC_NSel*cov_NSel[i,j]
    }
  }
  
  #############################################################################
})

### Data for NIMBLE
NHQ.data <- list(
  ### Nesting Selection ###
  y_N = y_N, # Used/Available Specifications
  wt_N = weightsN, #Weights for IWLR
  cov_NSel = cov_NSel # Spatial Covariates (3Dim Array)
)


### Constants for NIMBLE
NHQ.constants <- list(
  NNest_NSel = NInd_NSel # Number of Nests
)

NHQ.initial <- list(
  beta_SC_NSel = 0
)



### Parameters monitors
NHQ.monitor <- c(
  "beta_SC_NSel"
)

NHQ.dimensions <- list(
  cov_NSel = dim(NHQ.data$cov_NSel)
)

### MCMC settings
ni <- 30000 #number of iterations
nt <- 1 #thinning
nb <- 20000 #burn in period
nc <- 1 #number of chains/parallel cores

NHQ.model <- nimbleModel(code = NHQ.code,
                         constants = NHQ.constants,
                         dimensions = NHQ.dimensions,
                         inits = NHQ.initial,
                         data = NHQ.data)
NHQ.comp.model <- compileNimble(NHQ.model)
NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                               monitors = NHQ.monitor,
                               enableWAIC = T)
NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                            niter = ni,
                            nburnin = nb,
                            nchain = nc,
                            thin = nt,
                            summary = T,
                            samples = T,
                            WAIC = TRUE)
  
NHQ.samples.MCMC$summary

save(NHQ.samples.MCMC,
     file = "NSel_Results_Coverboard.csv")





##################################################################################
### NEST SUCCESS

### Load Necessary Packages
lapply(c('dplyr', 'lubridate', 'reshape2'), require, character.only = T)

### Load relevant databases
trap.raw <- read.csv("Trapping - Data.csv")
nestfate.raw <- read.csv("Nest Monitoring - Nest Info.csv") %>%
  mutate(Alum.Band.ID = as.character(Alum.Band.ID)) %>%
  filter(!is.na(NestLat)) %>%
  filter(NestID %in% cover.raw$NestID)
nestmonitor.raw <- read.csv("Nest Monitoring - Nest Status Checks.csv") %>%
  filter(NestID %in% unique(nestfate.raw$NestID)) %>%
  filter(NestID %in% cover.raw$NestID)


### Clean up databases for EH construction
## Get relevant bird information
# Subset to relevant time period
# Remove males
bird.info <- trap.raw %>%
  dplyr::select(BirdID = AlumBand, BirdID2 = Rivet.Band, Age, Sex, Trans.Type, Trap.Date = Date) %>%
  filter(Sex == "F") %>%
  filter(!is.na(Trans.Type) & Trans.Type != "") %>%
  mutate(Trap.Date = as.POSIXct(Trap.Date, format = "%m/%d/%Y"),
         Trap.Year = year(Trap.Date))

##Get relevant nest monitoring data
nest.info <- nestfate.raw %>%
  dplyr::select(NestID, BirdID = Alum.Band.ID, Nest.Attempt, Nest.Year = Year,
                NestLat, NestLong, Est.Laying.Init = Est.Laying.Initiation, Est.Hatch = EstimatedHatch, Fate) %>%
  filter(!is.na(NestLat))

## Get relevant nest monitoring data
# Group by NestID
monitoring.info <- nestmonitor.raw %>% 
  dplyr::select(NestID, Date, Status) %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  filter(Status == 1 | Status == 0) %>%
  mutate(Status = ifelse(Status == 0, 1, 0)) %>%
  mutate(Init = "N")

nestinit.info <- nest.info %>%
  dplyr::select(NestID, Date = Est.Laying.Init) %>%
  mutate(Status = 1) %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(Init = "Y")

monitoring.comp <- rbind(nestinit.info, monitoring.info) %>%
  arrange(NestID, Date)

#Identify which nests have more than one off-nest status (0)
multioff <- monitoring.comp %>%
  filter(Status == 0) %>%
  group_by(NestID) %>%
  summarize(Total = n()) %>%
  filter(Total > 1)

#Ensure Nest Initiation Is the first date
firstinit <- monitoring.comp %>%
  group_by(NestID) %>%
  slice(1L) %>%
  filter(Init == "N")

# Clean up dataframe
monitoring.clean <- monitoring.comp %>%
  dplyr::select(-Init) %>%
  distinct() %>%
  group_by(NestID) %>%
  mutate(Day1 = min(Date)) %>%
  mutate(RefDate = 1+yday(Date) - yday(Day1)) %>%
  ungroup()


### Hen Mortality

#Get mortality dates for relevant birds
telem.raw <- read.csv("Telemetry_Data - Telemetry.csv")

death.dates <- telem.raw %>% dplyr::select(BirdID = AlumBand, Date, Fate) %>% 
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(Year = year(Date)) %>%
  filter(Fate == "D" ) %>%
  group_by(BirdID) %>%
  arrange(Date) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(NestID = paste(BirdID, Year, 1, sep = "-")) %>%
  filter(NestID %in% nest.info$NestID) %>%
  mutate(HenMort = 1)

#Get First off nest
monitoring.firstoff <- monitoring.clean %>% 
  filter(Status == 0) %>%
  group_by(NestID) %>%
  arrange(Date) %>%
  slice(1L)

killed.on.nest <- merge(monitoring.firstoff, death.dates, by = c("NestID", "Date")) %>%
  arrange(NestID, Date)

### Nest Abandoned post Flushing
#Flush Count Date
flush.date <- nestfate.raw %>%
  dplyr::select(NestID, Date = Date.Counted) %>%
  filter(!is.na(Date) & Date != "") %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(Flushed = 1)

# Last Day On Nest
monitoring.lastdayon <- monitoring.clean %>% 
  filter(Status == 1) %>%
  group_by(NestID) %>%
  arrange(desc(Date)) %>%
  slice(1L)

flushed.aband <- merge(monitoring.lastdayon, flush.date, by = c("NestID", "Date")) %>%
  dplyr::select(NestID, LastDayOn = Date)

# First Day Off Nest
monitoring.firstdayoff <- monitoring.clean %>% 
  filter(Status == 0) %>%
  group_by(NestID) %>%
  arrange(Date) %>%
  slice(1L) %>%
  dplyr::select(NestID, FirstDayOff = Date)

flushed.diff <- merge(flushed.aband, monitoring.firstdayoff, all.x = T) %>%
  mutate(Difference = FirstDayOff - LastDayOn)

flush.final <- flushed.diff %>% filter(Difference < 4)

#Check If Hens are in both flush and death lists
which(killed.on.nest$NestID %in% flush.final$NestID)
which(flush.final$NestID %in% killed.on.nest$NestID)
killed.on.nest <- killed.on.nest[-which(killed.on.nest$NestID %in% flush.final$NestID),]

#Transform into EH format
ns_eh_matrix <- monitoring.clean %>% dcast(NestID ~ RefDate, value.var = "Status")

ns_eh_failnomort <- ns_eh_matrix[2:ncol(ns_eh_matrix)]

#Flip Success history in prep for making failure type histories
ns_eh_failnomort <- ifelse(ns_eh_failnomort == 1, 0, ifelse(ns_eh_failnomort == 0, 1, NA))
ns_eh_failmort <- ifelse(ns_eh_failnomort == 1, 0, ifelse(ns_eh_failnomort == 0, 0, NA))

row.mort <- which(ns_eh_matrix$NestID %in% killed.on.nest$NestID)
row.flush <- which(ns_eh_matrix$NestID %in% flush.final$NestID)

for(i in 1:nrow(ns_eh_failnomort)){
  for(j in 1:ncol(ns_eh_failnomort)){
    
    ns_eh_failmort[i,j] <- ifelse(i %in% row.mort & ns_eh_failnomort[i,j] == 1, 1,
                                  ifelse(i %in% row.flush & ns_eh_failnomort[i,j] == 1, 0, ns_eh_failmort[i,j]))
    ns_eh_matrix[i,j+1] <- ifelse(i %in% row.flush & ns_eh_matrix[i,j+1] == 0, NA, ns_eh_matrix[i,j+1])
    ns_eh_failnomort[i,j] <- ifelse(i %in% row.mort & ns_eh_failnomort[i,j] == 1, 0,
                                    ifelse(i %in% row.flush & ns_eh_failnomort[i,j] == 1, 0, ns_eh_failnomort[i,j]))
    
  } #j
} #i


########################
### Prep EH For JAGS ###
########################
### Vectorize encounter histories ###
# Translate visitation history to exposure length history
ns_eh_matrix_edit <- ns_eh_matrix[,-c(1)]
ns_eh_failmort[is.na(ns_eh_matrix_edit)] <- NA
ns_eh_failnomort[is.na(ns_eh_matrix_edit)] <- NA
ns_n_ind <- nrow(ns_eh_matrix)
ns_exposure <- ncol(ns_eh_matrix)-1

#Vectorize encounter histories
ns_succ1 <- as.vector(t(ns_eh_matrix_edit))
ns_succ <- ns_succ1[!is.na(ns_succ1)]

ns_failnomort1 <- as.vector(t(ns_eh_failnomort))
ns_failnomort <- ns_failnomort1[!is.na(ns_failnomort1)]

ns_failmort1 <- as.vector(t(ns_eh_failmort))
ns_failmort <- ns_failmort1[!is.na(ns_failmort1)]

# ns_failflush1 <- as.vector(t(ns_eh_failflush))
# ns_failflush <- ns_failflush1[!is.na(ns_failflush1)]

#Format as a matrix for JAGS model
# ns_succ.mat <- matrix(c(ns_succ, ns_failnomort, ns_failmort, ns_failflush), ncol = 4, nrow = length(ns_failnomort), byrow = F)
ns_succ.mat <- matrix(c(ns_succ, ns_failnomort, ns_failmort), ncol = 3, nrow = length(ns_failnomort), byrow = F)

#Double check that all rows sum to 1 and the columns sum = length of number in each group
rowSums(ns_succ.mat)
colSums(ns_succ.mat)

ns_ID <- matrix(1:ns_n_ind, nrow = ns_n_ind, ncol = ns_exposure)
ns_ID <- as.vector(t(ns_ID))
ns_ID <- ns_ID[!is.na(ns_succ1)]      # ID marker

# Interval length between visits
ns_visit_ind <- matrix(NA, ncol = ncol(ns_eh_matrix), nrow = nrow(ns_eh_matrix))
get.last <- function(x) max(which(!is.na(x)))

for(i in 1:ns_n_ind){
  for(j in 2:(ns_exposure+1)){
    if(is.na(ns_eh_matrix[i,j]) == FALSE){
      ns_visit_ind[i,j] <- j - get.last(ns_eh_matrix[i,1:(j-1)])
    }
  }
}

ns_interval <- as.vector(t(ns_visit_ind))
ns_interval <- ns_interval[!is.na(ns_interval)]

NDSR_order <- ns_eh_matrix$NestID

cov_NDSR.df <- cover.raw %>%
  filter(NestID %in% NDSR_order) %>%
  filter(PointID == "Nest") %>%
  select(NestID, PointAvg) %>%
  arrange(match(NestID, NDSR_order))

cov_NDSR <- cov_NDSR.df$PointAvg

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({
  
  #############################################################################
  
  ### Nest/Hen Mort/Flush Daily Failure Risk ###
  ## Priors ##
  # Nest Only Failure
  intercept_NDSR ~ dnorm(0, 0.001)
  beta_SC_NDSR ~ dnorm(0, 0.001)
  
  # Hen Mortality
  intercept_HDSR ~ dnorm(0, 0.001)
  beta_SC_HDSR ~ dnorm(0, 0.001)
  
  #Likelihood
  for(n in 1:NDSR_nvisit){
    #Linear Predictors
    cts[n] <- 1
    ctn[n] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NDSR[NDSR_ID[n]])
    cth[n] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NDSR[NDSR_ID[n]])
    
    #Denominator
    den[n] <- cts[n] + ctn[n] + cth[n] #+ ctf[n]
    
    #Daily Survival Probability
    survp[n] <- cts[n]/den[n]
    
    #Interval Survival Probability
    p[n,1] <- pow(survp[n], NDSR_interval[n])
    
    #Interval Probabilities of Nest/Hen Loss
    p[n,2] <- ((ctn[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
    p[n,3] <- ((cth[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
    
    #Multinomial likelihood
    NDSR_succ[n,1:3] ~ dmulti(p[n,1:3],1)
  }
  
  #############################################################################
  
})

### Data for NIMBLE
NHQ.data <- list(
  ### Nest Success ###
  NDSR_succ = ns_succ.mat,
  NDSR_interval = ns_interval,
  cov_NDSR = cov_NDSR
)


### Constants for NIMBLE
NHQ.constants <- list(
  ### Nest Success ###
  NDSR_nvisit = length(ns_succ),
  NDSR_ID = ns_ID
)

NHQ.initial <- list(
  beta_SC_NDSR = 0,  
  beta_SC_HDSR = 0,
  intercept_HDSR = 0,
  intercept_NDSR = 0
)

### Parameters monitors
NHQ.monitor <- c(
  ### Nest Success ###
  "intercept_NDSR",
  "beta_SC_NDSR",
  "intercept_HDSR",
  "beta_SC_HDSR"
)

### MCMC settings
ni <- 100000 #number of iterations
nt <- 1 #thinning
nb <- 90000 #burn in period
nc <- 1 #number of chains/parallel cores

NHQ.model <- nimbleModel(code = NHQ.code,
                         constants = NHQ.constants,
                         dimensions = NHQ.dimensions,
                         inits = NHQ.initial,
                         data = NHQ.data)
NHQ.comp.model <- compileNimble(NHQ.model)
NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                               monitors = NHQ.monitor,
                               enableWAIC = T)
NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                            niter = ni,
                            nburnin = nb,
                            nchain = nc,
                            thin = nt,
                            summary = T,
                            samples = T,
                            WAIC = TRUE)

NHQ.samples.MCMC$summary

save(NHQ.samples.MCMC,
     file = "NDSR_Results_Coverboard.csv")