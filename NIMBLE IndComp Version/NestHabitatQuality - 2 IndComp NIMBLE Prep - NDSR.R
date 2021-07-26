####################
### NEST SUCCESS ###
####################

### Nimble Attempt
require(nimble)

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

  # BLISS
  weights[1:4] <- c(0.25,0.25,0.25,0.25)
  scale_NDSR ~ dcat(weights[1:4])
  scale_HDSR ~ dcat(weights[1:4])
  
  #Likelihood
  for(n in 1:NDSR_nvisit){
    #Linear Predictors
    cts[n] <- 1
    ctn[n] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    cth[n] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    
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
  ### Nest Success ###
  scale_NDSR = 1,
  beta_SC_NDSR = 0,  
  scale_HDSR = 1,
  beta_SC_HDSR = 0
)



### Parameters monitors
NHQ.monitor <- c(
  ### Nest Success ###
  "intercept_NDSR",
  "beta_SC_NDSR",
  "intercept_HDSR",
  "beta_SC_HDSR",
  "scale_NDSR",
  "scale_HDSR"
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code,
     file = paste("./NIMBLE IndComp Version/",covname,"NDSR_NHQdata.RData", sep = ""))