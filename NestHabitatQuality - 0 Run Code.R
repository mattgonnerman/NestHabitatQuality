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

source(file = "NestHabitatQuality - 2a Execute JAGS.R")

# source(file = "NestHabitatQuality - 3a JAGS Model.R")


### Examine Model Results