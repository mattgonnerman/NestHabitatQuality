### Nimble Attempt
require(nimble)

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({

  #############################################################################
  
  ### Nest/Hen Mort/Flush Daily Failure Risk ###
  ## Priors ##
  # Nest Only Failure
  intercept_NDSR ~ dnorm(0, 0.001)
  beta_SC_NDSR ~ dnorm(0, tau_NDSR)
  tau_NDSR <- 1/(pow(sigma_NDSR,2))
  sigma_NDSR ~ dunif(0,50)
  
  # Hen Mortality
  intercept_HDSR ~ dnorm(0, 0.001)
  beta_SC_HDSR ~ dnorm(0, tau_HDSR)
  tau_HDSR <- 1/(pow(sigma_HDSR,2))
  sigma_HDSR ~ dunif(0,50)

  # BLISS
  scale_NDSR ~ dcat(w_NDSR[1:4])
  w_NDSR[1:4] ~ ddirch(dirpw[1:4])
  scale_HDSR ~ dcat(w_HDSR[1:4])
  w_HDSR[1:4] ~ ddirch(dirpw[1:4])
  
  #Likelihood
  for(n in 1:NDSR_nvisit){
    #Linear Predictors
    cts[n] <- 1
    ctn[n] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    cth[n] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    # ctf[n] <- exp(intercept_FDSR + beta_SC_FDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    #Denominator
    den[n] <- cts[n] + ctn[n] + cth[n] #+ ctf[n]
    
    #Daily Survival Probability
    survp[n] <- cts[n]/den[n]
    
    #Interval Survival Probability
    p[n,1] <- pow(survp[n], NDSR_interval[n])
    
    #Interval Probabilities of Nest/Hen Loss
    p[n,2] <- ((ctn[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
    p[n,3] <- ((cth[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
    # p[n,4] <- ((ctf[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
    
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
  sigma_NDSR = 1,
  beta_SC_NDSR = 0,  
  scale_HDSR = 1,
  sigma_HDSR = 1,
  beta_SC_HDSR = 0
)



### Parameters monitors
NHQ.monitor <- c(
  ### Nest Success ###
  "intercept_NDSR",
  "beta_SC_NDSR",
  "sigma_NDSR",
  "intercept_HDSR",
  "beta_SC_HDSR",
  "sigma_HDSR",
  "scale_NDSR",
  "w_NDSR",
  "scale_HDSR",
  "w_HDSR"
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code,
     file = paste("./NIMBLE IndComp Version/",covname,"NDSR_NHQdata.RData", sep = ""))