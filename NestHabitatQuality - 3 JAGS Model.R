function(){##############################################################################################
  ### Justification for using a conditional logistic regression for RSF
  ## Duchesne et al 2010 - Mixed conditional logsitic regression for habitat selection studies
  ### Weighted Conditional Logistic Regression
  ## Lee et al. 2019
  ### Conditional Logistic Regression Code From...
  ## Espino-Hernandez et al. 2011
  
  ### Prelaying Habitat Selection
  for(i in 1:NInd_PLSel){
    alpha_PLSel[i] ~ dbeta(1,1)
    intercept_PLSel[i] <- logit(alpha_PLSel[i])
  }
  beta_SC_PLSel ~ dnorm(0, tau_PLSel)
  tau_PLSel <- 1/(pow(sigma_PLSel,2))
  sigma_PLSel ~ dunif(0,50)
  scale_PLSel ~ dcat(w_PLSel[1:4])
  w_PLSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  for(i in 1:NInd_PLSel){
    yPL[i, 1:nPLLocs[i]] ~ dmulti(p_PL[i,1:nPLLocs[i]], 1)
    for(j in 1:nPLLocs[i]){
      p_PL[i,j] <- e_PL[i,j]/sum(wtPL[i,1:nPLLocs[i]]*e_PL[i,1:nPLLocs[i]])
      log(e_PL[i,j]) <- intercept_PLSel + beta_SC_PLSel*cov_PLSel[j,scale_PLSel,i]
    }
  }


    ### Laying Habitat Selection
  for(i in 1:NInd_LSel){
    alpha_LSel[i] ~ dbeta(1,1)
    intercept_LSel[i] <- logit(alpha_LSel[i])
  }
  beta_SC_LSel ~ dnorm(0, tau_LSel)
  tau_LSel <- 1/(pow(sigma_LSel,2))
  sigma_LSel ~ dunif(0,50)
  scale_LSel ~ dcat(w_LSel[1:4])
  w_LSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  
  for(i in 1:NInd_LSel){
    yL[i, 1:nLLocs[i]] ~ dmulti(p_L[i,1:nLLocs[i]], 1)
    for(j in 1:nLLocs[i]){
      p_L[i,j] <- e_L[i,j]/sum(wtL[i,1:nLLocs[i]]*e_L[i,1:nLLocs[i]])
      log(e_L[i,j]) <- intercept_LSel + beta_SC_LSel*cov_LSel[j,scale_LSel,i]
    }
  }

  
  ### Nesting Habitat Selection
  alpha_NSel ~ dbeta(1,1)
  intercept_NSel <- logit(alpha_NSel)
  beta_SC_NSel ~ dnorm(0, tau_NSel)
  tau_NSel <- 1/(pow(sigma_NSel,2))
  sigma_NSel ~ dunif(0,50) 
  scale_NSel ~ dcat(w_NSel[1:4])
  w_NSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))


  for(i in 1:NInd_NSel){
    yN[i, 1:nNLocs[i]] ~ dmulti(p_N[i,1:nNLocs[i]], 1)
    for(j in 1:nNLocs[i]){
      p_N[i,j] <- e_N[i,j]/sum(wtN[i,1:nNLocs[i]]*e_N[i,1:nNLocs[i]])
      log(e_N[i,j]) <- intercept_NSel + beta_SC_NSel*cov_NSel[j,scale_NSel,i]
    }
  }
  
  # ### Nest/Hen Mort/Flush Daily Failure Risk ###
  # ## Priors ##
  # # Nest Only Failure
  # intercept_NDSR ~ dnorm(0, 0.001)
  # beta_SC_NDSR ~ dnorm(0, tau_NDSR)
  # tau_NDSR <- 1/(pow(sigma_NDSR,2))
  # sigma_NDSR ~ dunif(0,50)  
  # # Hen Mortality
  # intercept_HDSR ~ dnorm(0, 0.001)
  # beta_SC_HDSR ~ dnorm(0, tau_HDSR)
  # tau_HDSR <- 1/(pow(sigma_HDSR,2))
  # sigma_HDSR ~ dunif(0,50) 
  # # Abandoned post flush
  # intercept_FDSR ~ dnorm(0, 0.001)
  # beta_SC_FDSR ~ dnorm(0, tau_FDSR)
  # tau_FDSR <- 1/(pow(sigma_FDSR,2))
  # sigma_FDSR ~ dunif(0,50)
  # 
  # # BLISS
  # scale_NDSR ~ dcat(w_NDSR[1:4])
  # w_NDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  # scale_HDSR ~ dcat(w_HDSR[1:4])
  # w_HDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  # scale_FDSR ~ dcat(w_FDSR[1:4])
  # w_FDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  # 
  # #Likelihood
  # for(n in 1:NDSR_nvisit){
  #   #Linear Predictors
  #   cts[n] <- 1
  #   ctn[n] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
  #   cth[n] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
  #   ctf[n] <- exp(intercept_FDSR + beta_SC_FDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
  #   #Denominator
  #   den[n] <- cts[n] + ctn[n] + cth[n] + ctf[n]
  #   
  #   #Daily Survival Probability 
  #   survp[n] <- cts[n]/den[n]
  #   
  #   #Interval Probabilities of Nest/Hen Loss
  #   p[n,2] <- ((ctn[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n])) 
  #   p[n,3] <- ((cth[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n])) 
  #   p[n,4] <- ((ctf[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
  #   
  #   #Interval Survival Probability
  #   p[n,1] <- pow(survp[n], NDSR_interval[n]) 
  #   
  #   NDSR_succ[n,1:4] ~ dmulti(p[n,],1)
  # }
  # 
  # # Mean Success Probabilities
  # mean_succ <- mean(survp)
  # 
  ### Habitat Quality ###
  # P(Selection) x P(Nest Success) x P(Hen Survival) = Quality
  # for(i in 1:nNHQ){
  #   # Selection
  #   PLS[i] <- exp(beta_SC_PLSel*cov_NHQ[i,scale_PLSel])
  #   LS[i] <- exp(beta_SC_LSel*cov_NHQ[i,scale_LSel])
  #   NS[i] <- exp(beta_SC_NSel*cov_NHQ[i,scale_NSel])
  # 
  #   # Failure Risk
  #   PN[i] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NHQ[i,scale_NDSR])
  #   PH[i] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NHQ[i,scale_HDSR])
  #   PF[i] <- exp(intercept_FDSR + beta_SC_FDSR*cov_NHQ[i,scale_FDSR])
  #   SuccP[i] <- 1/(1 + PF[i] + PH[i] + PN[i])
  # 
  #   #Nesting Habitat Quality Metric
  #   NHQ[i] <- PLS[i]/max(PLS) * LS[i]/max(LS) * NS[i]/max(NS) * SuccP[i]/max(SuccP)
  #   # NHQ[i] <- NS[i]/max(NS) * SuccP[i]/max(SuccP)
  # }
   
  
}