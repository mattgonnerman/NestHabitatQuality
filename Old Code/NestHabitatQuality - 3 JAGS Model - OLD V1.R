function(){##############################################################################################

  ### Prelaying Habitat Selection
  for(i in 1:NInd_PLSel){
    alpha_PLSel[i] ~ dbeta(1,1)
    intercept_PLSel[i] <- logit(alpha_PLSel[i])
  }

  #Use BLISS for habitat covariates
  beta_SC_PLSel ~ dnorm(0, tau_PLSel)
  tau_PLSel <- 1/(pow(sigma_PLSel,2))
  sigma_PLSel ~ dunif(0,50)
  scale_PLSel ~ dcat(w_PLSel[1:4])
  w_PLSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  for(n in 1:N_PLSel){
    logit(piPL[n]) <- intercept_PLSel[Ind_PLSel[n]] + beta_SC_PLSel*cov_PLSel[n,scale_PLSel]
    yPL[n] ~ dbern(piPL[n]*wtPL[n])
  }

  ### Laying Habitat Selection
  for(i in 1:NInd_LSel){
    alpha_LSel[i] ~ dbeta(1,1)
    intercept_LSel[i] <- logit(alpha_LSel[i])
  }

  #Use BLISS for habitat covariates
  beta_SC_LSel ~ dnorm(0, tau_LSel)
  tau_LSel <- 1/(pow(sigma_LSel,2))
  sigma_LSel ~ dunif(0,50)
  scale_LSel ~ dcat(w_LSel[1:4])
  w_LSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  for(n in 1:N_LSel){
    logit(piL[n]) <- intercept_LSel[Ind_LSel[n]] + beta_SC_LSel*cov_LSel[n,scale_LSel]
    yL[n] ~ dbern(piL[n]*wtL[n])
  }

  ### Nesting Habitat Selection
  alpha_NSel ~ dbeta(1,1)
  intercept_NSel <- logit(alpha_NSel)

  #Use BLISS for habitat covariates
  beta_SC_NSel ~ dnorm(0, tau_NSel)
  tau_NSel <- 1/(pow(sigma_NSel,2))
  sigma_NSel ~ dunif(0,50) 
  scale_NSel ~ dcat(w_NSel[1:4])
  w_NSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))


  for(n in 1:N_NSel){
    logit(piN[n]) <- (intercept_NSel + beta_SC_NSel*cov_NSel[n,scale_NSel])*wtN[n]
    yN[n] ~ dbern(piN[n])
  }
  
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
  # Abandoned post flush
  intercept_FDSR ~ dnorm(0, 0.001)
  beta_SC_FDSR ~ dnorm(0, tau_FDSR)
  tau_FDSR <- 1/(pow(sigma_FDSR,2))
  sigma_FDSR ~ dunif(0,50)
  
  # BLISS
  scale_NDSR ~ dcat(w_NDSR[1:4])
  w_NDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  scale_HDSR ~ dcat(w_HDSR[1:4])
  w_HDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  scale_FDSR ~ dcat(w_FDSR[1:4])
  w_FDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  
  #Likelihood
  for(n in 1:NDSR_nvisit){
    #Linear Predictors
    cts[n] <- 1
    ctn[n] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    cth[n] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    ctf[n] <- exp(intercept_FDSR + beta_SC_FDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
    #Denominator
    den[n] <- cts[n] + ctn[n] + cth[n] + ctf[n]
    
    #Daily Survival Probability 
    survp[n] <- cts[n]/den[n]
    
    #Interval Probabilities of Nest/Hen Loss
    p[n,2] <- ((ctn[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n])) 
    p[n,3] <- ((cth[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n])) 
    p[n,4] <- ((ctf[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
    
    #Interval Survival Probability
    p[n,1] <- pow(survp[n], NDSR_interval[n]) 
    
    NDSR_succ[n,1:4] ~ dmulti(p[n,],1)
  }
  
  # Mean Success Probabilities
  mean_succ <- mean(survp)

  ### Habitat Quality ###
  # P(Selection) x P(Nest Success) x P(Hen Survival) = Quality
  for(i in 1:nNHQ){
    # Selection
    PLS[i] <- exp(beta_SC_PLSel*cov_NHQ[i,scale_PLSel])
    LS[i] <- exp(beta_SC_LSel*cov_NHQ[i,scale_LSel])
    NS[i] <- exp(beta_SC_NSel*cov_NHQ[i,scale_NSel])

    # Failure Risk
    PN[i] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NHQ[i,scale_NDSR])
    PH[i] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NHQ[i,scale_HDSR])
    PF[i] <- exp(intercept_FDSR + beta_SC_FDSR*cov_NHQ[i,scale_FDSR])
    SuccP[i] <- 1/(1 + PF[i] + PH[i] + PN[i]) 
    
    #Nesting Habitat Quality Metric
    NHQ[i] <- PLS[i]/max(PLS) * LS[i]/max(LS) * NS[i]/max(NS) * SuccP[i]/max(SuccP)
    # NHQ[i] <- NS[i]/max(NS) * SuccP[i]/max(SuccP)
  }
   
  
}