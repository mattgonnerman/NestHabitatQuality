###
setwd("E:/GitHub/NestHabitatQuality")

### Data Preperation
# Nest Site Selection
source(file = "NestHabitatQuality - 1a Selection Data.R")

# Nest Success
source(file = "NestHabitatQuality - 1b Nest Success Data.R")

# Hen Survival
source(file = "NestHabitatQuality - 1c Hen Survival Data.R")

# Habitat Covariates
source(file = "NestHabitatQuality - 1d Habitat Covariates Data.R")


### Run JAGS Model
#MCMC settings
ni <- 100 #number of iterations
nt <- 8 #thinning
nb <- 5 #burn in period
nc <- 5 #number of chains/parallel cores

#Spatial Covariate Column Prefixes
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
  "BA_", "HT_", "SW_",
  "D2Edg_", "D2Rd_", "D2Rp_")

# for(i in 1:length(covSelnames)){
for(i in 1){
  covname <- covSelnames[i]
  print(paste("Run", covname, "Start Time:", Sys.time(), sep = " "))
  # Prep Data
  source(file = "NestHabitatQuality - 1 JAGS Data Prep.R")
  
  source(file = "NestHabitatQuality - 2 Execute JAGS.R")
  print(paste("Run", covname, "End Time:", Sys.time(), sep = " "))
}


# source(file = "NestHabitatQuality - 3a JAGS Model.R")


### Examine Model Results