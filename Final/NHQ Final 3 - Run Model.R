lapply(c("dplyr", "ggplot2", "nimble"), require, character.only = T)

### Data Prep
source("./Final/NHQ Final 1 - Data Prep.R")

### Nimble Prep
source("./Final/NHQ Final 2 - rNimble Prep.R")

### Nimble Code
rm(list=setdiff(ls(),
                c("NHQ.code", "NHQ.constants", "NHQ.initial", "NHQ.data", "NHQ.monitor")))
gc()

NHQ.model <- nimbleModel(code = NHQ.code,
                         constants = NHQ.constants,
                         inits = NHQ.initial,
                         data = NHQ.data)

NHQ.comp.model <- compileNimble(NHQ.model)

NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                               monitors = NHQ.monitor)

NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)

NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)

rm(list=setdiff(ls(),
                c("NHQ.comp.MCMC")))
gc()

# MCMC settings
ni <- 40000 #number of iterations
nt <- 1 #thinning
nb <- 35000 #burn in period
nc <- 1 #number of chains/parallel cores

NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                            niter = ni,
                            nburnin = nb,
                            nchain = nc,
                            thin = nt,
                            summary = T,
                            samples = F)

write.csv(NHQ.samples.MCMC, "./Final/Final Model Summary 90m.csv")

