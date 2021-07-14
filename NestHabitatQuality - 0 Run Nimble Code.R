### Spatial Covariate Column Prefixes
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

### Data Prep
source(file = "NestHabitatQuality - 1a JAGS Data Prep - NIMBLE.R")

### Initial Model Prep
#May be necessary to restart R to get completely clear RAM
require(nimble)
rm(list=setdiff(ls(),
                c("NHQ.monitor", "NHQ.code", "NHQ.data", "NHQ.constants", "NHQ.initial",
                  "covname", "ni", 'nc', "nb")))
gc()
  
load(file = "E:/Maine Drive/Analysis/NestHabitatQuality/ag_NHQdata.RData")
NHQ.model <- nimbleModel(code = NHQ.code,
                         constants = NHQ.constants,
                         dimensions = NHQ.dimensions,
                         inits = NHQ.initial,
                         data = NHQ.data)
save(NHQ.model, file = "E:/Maine Drive/Analysis/NestHabitatQuality/BaseNimbleModel.RData")

### Run Model
#MCMC settings
ni <- 10000 #number of iterations
nt <- 8 #thinning
nb <- 2000 #burn in period
nc <- 1 #number of chains/parallel cores

### Run Model (Single Core)
#Multiple Line Invocation
require(nimble)
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                   "BA_", "HT_", "SW_",
                   "D2Edg_", "D2Rd_", "D2Rp_")
for(i in 1:length(covSelnames)){
  rm(list=setdiff(ls(),
                  c("NHQ.comp.MCMC",
                    "covname", "ni", 'nc', "nb", "nt", "covSelnames", "i")))
  gc()
  covname = covSelnames[i]
  
  
  load(file = paste("E:/Maine Drive/Analysis/NestHabitatQuality/", covname, "NHQdata.RData", sep = ""))
  load(file = "E:/Maine Drive/Analysis/NestHabitatQuality/BaseNimbleModel.RData")
  NHQ.model$setData(NHQ.data)
  NHQ.monitor <- c(NHQ.monitor, "err_PL", "err_L", "err_N")
  NHQ.comp.model <- compileNimble(NHQ.model)
  NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                                 monitors = NHQ.monitor,
                                 enableWAIC = T)
  NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
  NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
  rm(list=setdiff(ls(),
                  c("NHQ.comp.MCMC",
                    "covname", "ni", 'nc', "nb", "nt", 'covSelnames', 'i')))
  gc()
  NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                              niter = ni,
                              nburnin = nb,
                              nchain = nc,
                              thin = nt, 
                              summary = T,
                              WAIC = T)
  
  save(NHQ.samples.MCMC, 
       file = paste("C:/Users/matthew.gonnerman/Desktop/NestingHabitatQuality - NIMBLE MCMC Models/",covname, "MCMC.RData", sep = ""))
  write.csv(NHQ.samples.MCMC$summary, 
            paste("E:/Maine Drive/Analysis/NestHabitatQuality/", covname, "rawmodeloutput.csv", sep = ""))
  
  require(dplyr)
  require(stringr)
  NHQ.df <- as.data.frame(NHQ.samples.MCMC$summary) %>%
    mutate(Names = row.names(NHQ.samples.MCMC$summary)) %>%
    filter(grepl("NHQ", Names)) %>%
    mutate(ID = as.numeric(gsub("\\D", "", Names)))
  write.csv(NHQ.df, 
            paste("E:/Maine Drive/Analysis/NestHabitatQuality/", covname, "NHQ.csv", sep = ""))
}

### Create NHQ Value Rasters

