require(nimble)

### Data Prep

#Vectors for cycling through each model component and covariates
compnames <- c("PLSel", "LSel", "NSel", "NDSR")
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

### MCMC settings
ni <- 30000 #number of iterations
nt <- 1 #thinning
nb <- 10000 #burn in period
nc <- 1 #number of chains/parallel cores


### Data Preparation (Singular Components) ###
source(file = "NIMBLE IndComp Version/NestHabitatQuality - 1 IndComp NIMBLE Data Prep.R")


### Base Model Creation ###
#May be necessary to restart R to get completely clear RAM
require(nimble)
rm(list=setdiff(ls(),
                c("compnames","covSelnames", "ni", "nt", "nb", "nc")))
gc()

# Selection Base Models
for(i in 1:3){
  load(file = paste("./NIMBLE IndComp Version/", covSelnames[1], compnames[3], "_NHQdata.RData", sep = ""))
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
  save(NHQ.model, NHQ.comp.MCMC, file = paste("./NIMBLE IndComp Version/", compnames[i], "_BaseNimbleModel.RData", sep = ""))
}

#Nest Success Base Model
for(i in 4){
  load(file = paste("./NIMBLE IndComp Version/", covSelnames[1], compnames[i], "_NHQdata.RData", sep = ""))
  NHQ.model <- nimbleModel(code = NHQ.code,
                           constants = NHQ.constants,
                           inits = NHQ.initial,
                           data = NHQ.data)
  NHQ.comp.model <- compileNimble(NHQ.model)
  NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                                 monitors = NHQ.monitor,
                                 enableWAIC = T)
  NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
  NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
  save(NHQ.model, NHQ.comp.MCMC, file = paste("./NIMBLE IndComp Version/", compnames[i], "_BaseNimbleModel.RData", sep = ""))
}


### Perform BLISS Scale Selection ###


### Model Selection using best scale for each ###


### Final Model Run ###




### Collet WAIC Values into table
rm("waic_table")
waic_table <- data.frame(Component = NA,
                   Covariate = NA,
                   WAIC = NA)
for(i in 1:10){ #Number of covariates
    for(j in 4){ #Number of components
      load(file = paste("./NIMBLE IndComp Version/", covSelnames[i], compnames[j], "_MCMC.RData", sep = ""))
      waic_table[i,1] <- compnames[j]
      waic_table[i,2] <- covSelnames[i]
      waic_table[i,3] <- NHQ.samples.MCMC$WAIC
    }
}

require(dplyr)
waic_table %>% arrange(WAIC)

### Create NHQ Value Rasters

