### Nimble Attempt
require(nimble)

NHQ.code <- nimbleCode({
  #Model 
  ### Prelaying Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_PLSel ~ dnorm(0, tau_PLSel) 
  tau_PLSel <- 1/(pow(sigma_PLSel,2))
  sigma_PLSel ~ dunif(0,50)
  
  # BLISS 
  dirpw <- c(0.25,0.25,0.25,0.25)
  scale_PLSel ~ dcat(w_PLSel[1:4])
  w_PLSel[1:4] ~ ddirch(dirpw)
  
  # Individual Random Effect (Intercept)
  alpha_PL_Int[1:NNest_PLSel] ~ dmnorm(mu_PL_I[1:NNest_PLSel], omega_PL_I[1:NNest_PLSel,1:NNest_PLSel])
  omega_PL_I[1:NNest_PLSel,1:NNest_PLSel] ~ dwish(R_PL_I[1:NNest_PLSel,1:NNest_PLSel],NNest_PLSel)
  for(i in 1:NNest_PLSel){
    mu_PL_I[i] <- 0
    for(j in 1:NNest_PLSel){
      R_PL_I[i,j] <- ifelse(i==j, 1, 0)
    }
  }
  
  # Individual Random Effect (Slope)
  alpha_PL_Slp[1:NNest_PLSel] ~ dmnorm(mu_PL_S[1:NNest_PLSel], omega_PL_S[1:NNest_PLSel,1:NNest_PLSel])
  omega_PL_S[1:NNest_PLSel,1:NNest_PLSel] ~ dwish(R_PL_S[1:NNest_PLSel,1:NNest_PLSel],NNest_PLSel)
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
  w_LSel[1:4] ~ ddirch(dirpw)
  
  # Individual Random Effect (Intercept)
  alpha_L_Int[1:NNest_LSel] ~ dmnorm(mu_L_I[1:NNest_LSel], omega_L_I[1:NNest_LSel,1:NNest_LSel])
  omega_L_I[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_I[1:NNest_LSel,1:NNest_LSel],NNest_LSel)
  for(i in 1:NNest_LSel){
    mu_L_I[i] <- 0
    for(j in 1:NNest_LSel){
      R_L_I[i,j] <- ifelse(i==j, 1, 0)
    }
  }
  
  # Individual Random Effect (Slope)
  alpha_L_Slp[1:NNest_LSel] ~ dmnorm(mu_L_S[1:NNest_LSel], omega_L_S[1:NNest_LSel,1:NNest_LSel])
  omega_L_S[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_S[1:NNest_LSel,1:NNest_LSel],NNest_LSel)
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
  w_NSel[1:4] ~ ddirch(dirpw)
  
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

  # BLISS
  scale_NDSR ~ dcat(w_NDSR[1:4])
  w_NDSR[1:4] ~ ddirch(dirpw)
  scale_HDSR ~ dcat(w_HDSR[1:4])
  w_HDSR[1:4] ~ ddirch(dirpw)
  
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
    NHQ[i] <- PLS[i]/max(PLS[1:nNHQ]) * LS[i]/max(LS[1:nNHQ]) * NS[i]/max(NS[1:nNHQ]) * SuccP[i]/max(SuccP[1:nNHQ])
    }
})

#Data for NIMBLE
NHQ.data <- list(
  ### PreLaying Selection ###
  y_PL = y_PL, # Used/Available Specifications
  wt_PL = weightsPL, #Weights for IWLR
  cov_PLSel = cov_PLSel, # Spatial Covariates (3Dim Array)
  
  ### Laying Selection ###
  y_L = y_L, # Used/Available Specifications
  wt_L = weightsL, #Weights for IWLR
  cov_LSel = cov_LSel, # Spatial Covariates (3Dim Array)
  
  ### Nesting Selection ###
  y_N = y_N, # Used/Available Specifications
  wt_N = weightsN, #Weights for IWLR
  cov_NSel = cov_NSel, # Spatial Covariates (3Dim Array)
  
  ### Nest Success ###
  NDSR_succ = ns_succ.mat,
  NDSR_interval = ns_interval,
  cov_NDSR = cov_NDSR,
  
  ### Nesting Habitat Quality Metric ###
  cov_NHQ = cov_NHQ
)

#Constants for NIMBLE
NHQ.constants <- list(
  ### PreLaying Selection ###
  NestID_PL = Ind_PLSel, # Numeric Nest ID
  NGrp_PLSel = NInd_PLSel, # Count of Used/Available Groups
  NNest_PLSel = N_PLSel, # Count of Individual Nests
  
  ### Laying Selection ###
  NestID_L = Ind_LSel, # Numeric Nest ID
  NGrp_LSel = NInd_LSel, # Count of Used/Available Groups
  NNest_LSel = N_LSel, # Count of Individual Nests
  
  ### Nesting Selection ###
  NestID_N = Ind_NSel, # Numeric Nest ID
  NGrp_NSel = NInd_NSel, # Count of Used/Available Groups
  NNest_NSel = N_NSel, # Count of Individual Nests
  
  ### Nest Success ###
  NDSR_nvisit = length(ns_succ),
  NDSR_ID = ns_ID,
  
  ### Nesting Habitat Quality Metric ###
  nNHQ = nrow(cov_NHQ)
)

NHQ.model <- nimbleModel(code = NHQ.code,
                         name = paste(covname, "NIMBLE", sep = ""),
                         constants = NHQ.constants,
                         data = NHQ.data)
