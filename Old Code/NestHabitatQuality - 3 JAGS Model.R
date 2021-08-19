function(){##############################################################################################
  ### Justification for using a conditional logistic regression for RSF
  ## Duchesne et al 2010 - Mixed conditional logsitic regression for habitat selection studies
  ### Infinitely Weighted Logistic Regression 
  ## Fithian and Hastie 2013
  ### Weighted Conditional Logistic Regression Code...
  ## Lee et al. 2019
  ### Conditional Logistic Regression Code From...
  ## Espino-Hernandez et al. 2011
  ### Multinomila Logistic Nest Fate From...
  ## Darrah et al 2018

  
  ### Prelaying Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_PLSel ~ dnorm(0, tau_PLSel) 
  tau_PLSel <- 1/(pow(sigma_PLSel,2))
  sigma_PLSel ~ dunif(0,50)
  
  # BLISS 
  scale_PLSel ~ dcat(w_PLSel[1:4])
  w_PLSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  # Individual Random Effect (Intercept)
  alpha_PL_Int[1:NNest_PLSel] ~ dmnorm(mu_PL_I[1:NNest_PLSel], omega_PL_I[1:NNest_PLSel,1:NNest_PLSel])
  omega_PL_I[1:NNest_PLSel,1:NNest_PLSel] ~ dwish(R_PL_I[,],NNest_PLSel)
  for(i in 1:NNest_PLSel){
    mu_PL_I[i] <- 0
    for(j in 1:NNest_PLSel){
      R_PL_I[i,j] <- ifelse(i==j, 1, 0)
    }
  }
  
  # Individual Random Effect (Slope)
  alpha_PL_Slp[1:NNest_PLSel] ~ dmnorm(mu_PL_S[1:NNest_PLSel], omega_PL_S[1:NNest_PLSel,1:NNest_PLSel])
  omega_PL_S[1:NNest_PLSel,1:NNest_PLSel] ~ dwish(R_PL_S[,],NNest_PLSel)
  for(i in 1:NNest_PLSel){
    mu_PL_S[i] <- 0
    for(j in 1:NNest_PLSel){
      R_PL_S[i,j] <- ifelse(i==j, 1, 0)
    }
  }

  # Random Error 
  for(i in 1:NGrp_PLSel){  
    for(j in 1:11){
      err_PL[i,j] ~ dnorm(0, tau_PLerr) 
    }
  }
  tau_PLerr <- 1/(pow(sigma_PLerr,2))
  sigma_PLerr ~ dunif(0,50)
  
  ## Likelihood
  for(i in 1:NGrp_PLSel){
    y_PL[i, 1:11] ~ dmulti(p_PL[i,1:11], 1)
    for(j in 1:11){
      p_PL[i,j] <- e_PL[i,j]/sum(wt_PL[i,1:11]*e_PL[i,1:11])
      log(e_PL[i,j]) <- alpha_PL_Int[NestID_PL[i]] + alpha_PL_Slp[NestID_PL[i]]*cov_PLSel[j,scale_PLSel,i] + beta_SC_PLSel*cov_PLSel[j,scale_PLSel,i] + err_PL[i,j]
    }
  }

  #############################################################################
  
  ### Laying Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_LSel ~ dnorm(0, tau_LSel) 
  tau_LSel <- 1/(pow(sigma_LSel,2))
  sigma_LSel ~ dunif(0,50)
  
  # BLISS 
  scale_LSel ~ dcat(w_LSel[1:4])
  w_LSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  
  # Individual Random Effect (Intercept)
  alpha_L_Int[1:NNest_LSel] ~ dmnorm(mu_L_I[1:NNest_LSel], omega_L_I[1:NNest_LSel,1:NNest_LSel])
  omega_L_I[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_I[,],NNest_LSel)
  for(i in 1:NNest_LSel){
    mu_L_I[i] <- 0
    for(j in 1:NNest_LSel){
      R_L_I[i,j] <- ifelse(i==j, 1, 0)
    }
  }
  
  # Individual Random Effect (Slope)
  alpha_L_Slp[1:NNest_LSel] ~ dmnorm(mu_L_S[1:NNest_LSel], omega_L_S[1:NNest_LSel,1:NNest_LSel])
  omega_L_S[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_S[,],NNest_LSel)
  for(i in 1:NNest_LSel){
    mu_L_S[i] <- 0
    for(j in 1:NNest_LSel){
      R_L_S[i,j] <- ifelse(i==j, 1, 0)
    }
  }
  
  
  # Random Error 
  for(i in 1:NGrp_LSel){  
    for(j in 1:11){
      err_L[i,j] ~ dnorm(0, tau_Lerr) 
    }
  }
  tau_Lerr <- 1/(pow(sigma_Lerr,2))
  sigma_Lerr ~ dunif(0,50)
  
  ## Likelihood
  for(i in 1:NGrp_LSel){
    y_L[i, 1:11] ~ dmulti(p_L[i,1:11], 1)
    for(j in 1:11){
      p_L[i,j] <- e_L[i,j]/sum(wt_L[i,1:11]*e_L[i,1:11])
      log(e_L[i,j]) <- alpha_L_Int[NestID_L[i]] + alpha_L_Slp[NestID_L[i]]*cov_LSel[j,scale_LSel,i] + beta_SC_LSel*cov_LSel[j,scale_LSel,i] + err_L[i,j]
    }
  }
  
  #############################################################################
  
  ### Nesting Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_NSel ~ dnorm(0, tau_NSel) 
  tau_NSel <- 1/(pow(sigma_NSel,2))
  sigma_NSel ~ dunif(0,50)
  
  # BLISS 
  scale_NSel ~ dcat(w_NSel[1:4])
  w_NSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  # Random Error 
  for(i in 1:NGrp_NSel){  
    for(j in 1:11){
      err_N[i,j] ~ dnorm(0, tau_Nerr) 
    }
  }
  tau_Nerr <- 1/(pow(sigma_Nerr,2))
  sigma_Nerr ~ dunif(0,50)
  
  ## Likelihood
  for(i in 1:NGrp_NSel){
    y_N[i, 1:11] ~ dmulti(p_N[i,1:11], 1)
    for(j in 1:11){
      p_N[i,j] <- e_N[i,j]/sum(wt_N[i,1:11]*e_N[i,1:11])
      log(e_N[i,j]) <- beta_SC_NSel*cov_NSel[j,scale_NSel,i] + err_N[i,j]
    }
  }
  
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
  
  # # Abandoned post flush
  # intercept_FDSR ~ dnorm(0, 0.001)
  # beta_SC_FDSR ~ dnorm(0, tau_FDSR)
  # tau_FDSR <- 1/(pow(sigma_FDSR,2))
  # sigma_FDSR ~ dunif(0,50)

  # BLISS
  scale_NDSR ~ dcat(w_NDSR[1:4])
  w_NDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  scale_HDSR ~ dcat(w_HDSR[1:4])
  w_HDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))
  # scale_FDSR ~ dcat(w_FDSR[1:4])
  # w_FDSR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

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
    NDSR_succ[n,1:3] ~ dmulti(p[n,],1)
  }

  ## Habitat Quality ###
  for(i in 1:nNHQ){
    # Selection
    PLS[i] <- exp(beta_SC_PLSel*cov_NHQ[i,scale_PLSel])
    LS[i] <- exp(beta_SC_LSel*cov_NHQ[i,scale_LSel])
    NS[i] <- exp(beta_SC_NSel*cov_NHQ[i,scale_NSel])

    # Failure Risk
    PN[i] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NHQ[i,scale_NDSR])
    PH[i] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NHQ[i,scale_HDSR])
    # PF[i] <- exp(intercept_FDSR + beta_SC_FDSR*cov_NHQ[i,scale_FDSR])
    SuccP[i] <- 1/(1 + PH[i] + PN[i]) #+ PF[i])

    #Nesting Habitat Quality Metric
    NHQ[i] <- PLS[i]/max(PLS) * LS[i]/max(LS) * NS[i]/max(NS) * SuccP[i]/max(SuccP)
  }
   
  
}