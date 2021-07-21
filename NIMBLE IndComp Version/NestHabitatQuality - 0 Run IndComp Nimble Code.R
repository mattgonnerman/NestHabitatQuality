### Data Prep

#Vectors for cycling through each model component and covariates
compnames <- c("PLSel", "LSel", "NSel", "NDSR")
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")


############################################################################
### ONLY NEED TO RUN THIS SECTION ONCE TO SPEED UP PROCESS DOWN THE LINE ###
source(file = "NIMBLE IndComp Version/NestHabitatQuality - 1 IndComp NIMBLE Data Prep.R")


### Initial Model Prep
#May be necessary to restart R to get completely clear RAM
require(nimble)
rm(list=setdiff(ls(),
                c("compnames","covSelnames")))
gc()

for(i in 1:4){
  load(file = paste("./NIMBLE IndComp Version/ag_", compnames[i], "NHQdata.RData", sep = ""))
  NHQ.model <- nimbleModel(code = NHQ.code,
                           constants = NHQ.constants,
                           dimensions = NHQ.dimensions,
                           inits = NHQ.initial,
                           data = NHQ.data)
  save(NHQ.model, file = paste("./NIMBLE IndComp Version/", compnames[i], "BaseNimbleModel.RData", sep = ""))
}

### ONLY NEED TO RUN THIS SECTION ONCE TO SPEED UP PROCESS DOWN THE LINE ###
############################################################################


### Run NIMBLE Models ###
#MCMC settings
ni <- 30000 #number of iterations
nt <- 10 #thinning
nb <- 20000 #burn in period
nc <- 1 #number of chains/parallel cores

### Run Model (Single Core)
#Multiple Line Invocation
require(nimble)
# for(i in 1:10){ #Number of covariates
#   for(j in 1:4){ #Number of components
for(i in 1){ #Number of covariates
  for(j in 3){ #Number of components
    rm(list=setdiff(ls(),
                    c("ni", 'nc', "nb", "nt", "covSelnames", "compnames", "i", "j")))
    gc()
    covname = covSelnames[i]
    compname = compnames[j]
    
    load(file = "./NIMBLE IndComp Version/", compname,"_BaseNimbleModel.RData")
    load(file = paste("./NIMBLE IndComp Version/", covname, compnames, "_NHQdata.RData", sep = ""))
  
    NHQ.model$setData(NHQ.data)
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
                                samples = T,
                                WAIC = T)
    
    save(NHQ.samples.MCMC, 
         file = paste("./NIMBLE IndComp Version/",covname, compname, "_MCMC.RData", sep = ""))
    write.csv(NHQ.samples.MCMC$summary, 
              paste("./NIMBLE IndComp Version/", covname, compname, "_rawmodeloutput.csv", sep = ""))
  }
}

### Create NHQ Value Rasters

