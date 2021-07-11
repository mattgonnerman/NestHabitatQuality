### Spatial Covariate Column Prefixes
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

### Data Prep
source(file = "NestHabitatQuality - 1a JAGS Data Prep - NIMBLE.R")

### Initial Model Prep
  rm(list=setdiff(ls(),
                  c("NHQ.monitor", "NHQ.code", "NHQ.data", "NHQ.constants", "NHQ.initial",
                    "covname", "ni", 'nc', "nb")))
  gc()
  
  load(file = "E:/Maine Drive/Analysis/NestHabitatQuality/ag_NHQdata.RData")
  NHQ.model <- nimbleModel(code = NHQ.code,
                         constants = NHQ.constants,
                         dimensions = NHQ.dimensions,
                         inits = NHQ.initial,
                         data = NHQ.data
)

### Run Model
#MCMC settings
ni <- 10000 #number of iterations
nt <- 1 #thinning
nb <- 2000 #burn in period
nc <- 3 #number of chains/parallel cores

### Run Model (Single Core)
#Multiple Line Invocation
require(nimble)

for(i in 1:length(covSelnames)){
  covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                   "BA_", "HT_", "SW_",
                   "D2Edg_", "D2Rd_", "D2Rp_")
  covname = covSelnames[i]
  
  
  load(file = paste("E:/Maine Drive/Analysis/NestHabtiatQuality/", covname, "NHQdata.RData", sep = ""))
  NHQ.model$setData(NHQ.data)
  NHQ.comp.model <- compileNimble(NHQ.model)
  NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                                 monitors = NHQ.monitor)
  NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
  NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
  NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                              niter = ni,
                              nburnin = nb,
                              nchain = nc,
                              summary = T,
                              WAIC = T)
  
  save(NHQ.model, NHQ.samples.MCMC, 
       file = paste("E:/Maine Drive/Analysis/NestHabitatQuality/",covname, "NimbleModelandMCMC.RData", sep = ""))
  write.csv(NHQ.samples.MCMC$summary, 
            paste("E:/Maine Drive/Analysis/NestHabtiatQuality/", covname, "rawmodeloutput.csv", sep = ""))
  
  require(dplyr)
  require(stringr)
  NHQ.df <- as.data.frame(NHQ.samples.MCMC$summary) %>%
    mutate(Names = row.names(NHQ.samples.MCMC$summary)) %>%
    filter(grepl("NHQ", Names)) %>%
    mutate(ID = as.numeric(gsub("\\D", "", Names)))
  write.csv(NHQ.df, 
            paste("E:/Maine Drive/Analysis/NestHabtiatQuality/", covname, "NHQ.csv", sep = ""))
}

### Create NHQ Value Rasters

