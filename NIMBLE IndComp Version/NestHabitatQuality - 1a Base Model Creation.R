require(nimble)
rm(list=setdiff(ls(),
                c("compnames","covSelnames", "ni", "nt", "nb", "nc")))
gc()

# Selection Base Models
for(i in 1:3){
  load(file = paste("./NIMBLE IndComp Version/", covSelnames[1], compnames[i], "_NHQdata.RData", sep = ""))
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
  save(NHQ.model, NHQ.comp.MCMC, NHQ.MCMC, NHQ.conf.mcmc, NHQ.comp.model,
       file = paste("./NIMBLE IndComp Version/", compnames[i], "_BaseNimbleModel.RData", sep = ""))
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
  save(NHQ.model, NHQ.comp.MCMC, NHQ.MCMC, NHQ.conf.mcmc, NHQ.comp.model,
       file = paste("./NIMBLE IndComp Version/", compnames[i], "_BaseNimbleModel.RData", sep = ""))
}
