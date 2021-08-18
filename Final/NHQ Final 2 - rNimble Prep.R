### Nimble Code Prep ###
require(nimble)

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({
  
  #############################################################################
  
  ### Prelaying Selection
  # Habitat Coefficient
  for(i in 1:10){
    beta_SC_PLSel[i] ~ dnorm(0, 0.001)
  }
  
  ## Likelihood
  for(i in 1:NGrp_PLSel){
    y_PL[i, 1:11] ~ dmulti(p_PL[i,1:11], 1)
    for(j in 1:11){
      p_PL[i,j] <- e_PL[i,j]/inprod(wt_PL[i,1:11],e_PL[i,1:11])
      log(e_PL[i,j]) <- beta_SC_PLSel[1]*cov_PLSel[j,1,i] +
        beta_SC_PLSel[2]*cov_PLSel[j,2,i] +
        beta_SC_PLSel[3]*cov_PLSel[j,3,i] +
        beta_SC_PLSel[4]*cov_PLSel[j,4,i] +
        beta_SC_PLSel[5]*cov_PLSel[j,5,i] +
        beta_SC_PLSel[6]*cov_PLSel[j,6,i] +
        beta_SC_PLSel[7]*cov_PLSel[j,7,i] +
        beta_SC_PLSel[8]*cov_PLSel[j,8,i] +
        beta_SC_PLSel[9]*cov_PLSel[j,9,i] +
        beta_SC_PLSel[10]*cov_PLSel[j,10,i]
        
    }
  }
  
  #############################################################################
  
  ### Laying Selection
  # Habitat Coefficient
  for(i in 1:10){
    beta_SC_LSel[i] ~ dnorm(0, 0.001)
  }
  
  ## Likelihood
  for(i in 1:NGrp_LSel){
    y_L[i, 1:11] ~ dmulti(p_L[i,1:11], 1)
    for(j in 1:11){
      p_L[i,j] <- e_L[i,j]/inprod(wt_L[i,1:11],e_L[i,1:11])
      log(e_L[i,j]) <- beta_SC_LSel[1]*cov_LSel[j,1,i] +
        beta_SC_LSel[2]*cov_LSel[j,2,i] +
        beta_SC_LSel[3]*cov_LSel[j,3,i] +
        beta_SC_LSel[4]*cov_LSel[j,4,i] +
        beta_SC_LSel[5]*cov_LSel[j,5,i] +
        beta_SC_LSel[6]*cov_LSel[j,6,i] +
        beta_SC_LSel[7]*cov_LSel[j,7,i] +
        beta_SC_LSel[8]*cov_LSel[j,8,i] +
        beta_SC_LSel[9]*cov_LSel[j,9,i] +
        beta_SC_LSel[10]*cov_LSel[j,10,i]
      
    }
  }
  
  #############################################################################
  
  ### Nest Site Selection
  # Habitat Coefficient
  for(i in 1:10){
    beta_SC_NSel[i] ~ dnorm(0, 0.001)
  }
  
  ## Likelihood
  for(i in 1:NNest_NSel){
    y_N[i, 1:11] ~ dmulti(p_N[i,1:11], 1)
    for(j in 1:11){
      p_N[i,j] <- e_N[i,j]/inprod(wt_N[i,1:11],e_N[i,1:11])
      log(e_N[i,j]) <- beta_SC_NSel[1]*cov_NSel[j,1,i] +
        beta_SC_NSel[2]*cov_NSel[j,2,i] +
        beta_SC_NSel[3]*cov_NSel[j,3,i] +
        beta_SC_NSel[4]*cov_NSel[j,4,i] +
        beta_SC_NSel[5]*cov_NSel[j,5,i] +
        beta_SC_NSel[6]*cov_NSel[j,6,i] +
        beta_SC_NSel[7]*cov_NSel[j,7,i] +
        beta_SC_NSel[8]*cov_NSel[j,8,i] +
        beta_SC_NSel[9]*cov_NSel[j,9,i] +
        beta_SC_NSel[10]*cov_NSel[j,10,i]
      
    }
  }
  
  #############################################################################
  
  ### Nest/Hen Mort/Flush Daily Failure Risk ###
  ## Priors ##
  # Nest Only Failure
  intercept_NDSR ~ dnorm(0, 0.001)
  for(i in 1:10){
    beta_SC_NDSR[i] ~ dnorm(0, 0.001)
  }
  
  # Hen Mortality
  intercept_HDSR ~ dnorm(0, 0.001)
  for(i in 1:10){
    beta_SC_HDSR[i] ~ dnorm(0, 0.001)
  }

  #Likelihood
  for(n in 1:NDSR_nvisit){
    #Linear Predictors
    cts[n] <- 1
    ctn[n] <- exp(intercept_NDSR + beta_SC_NDSR[1]*cov_NDSR[NDSR_ID[n],1] + beta_SC_NDSR[2]*cov_NDSR[NDSR_ID[n],2] +
                    beta_SC_NDSR[3]*cov_NDSR[NDSR_ID[n],3] + beta_SC_NDSR[4]*cov_NDSR[NDSR_ID[n],4] + 
                    beta_SC_NDSR[5]*cov_NDSR[NDSR_ID[n],5] + beta_SC_NDSR[6]*cov_NDSR[NDSR_ID[n],6] + 
                    beta_SC_NDSR[7]*cov_NDSR[NDSR_ID[n],7] + beta_SC_NDSR[8]*cov_NDSR[NDSR_ID[n],8] + 
                    beta_SC_NDSR[9]*cov_NDSR[NDSR_ID[n],9] + beta_SC_NDSR[10]*cov_NDSR[NDSR_ID[n],10])
    cth[n] <- exp(intercept_HDSR + beta_SC_HDSR[1]*cov_HDSR[NDSR_ID[n],1] + beta_SC_HDSR[2]*cov_HDSR[NDSR_ID[n],2] +
                    beta_SC_HDSR[3]*cov_HDSR[NDSR_ID[n],3] + beta_SC_HDSR[4]*cov_HDSR[NDSR_ID[n],4] + 
                    beta_SC_HDSR[5]*cov_HDSR[NDSR_ID[n],5] + beta_SC_HDSR[6]*cov_HDSR[NDSR_ID[n],6] + 
                    beta_SC_HDSR[7]*cov_HDSR[NDSR_ID[n],7] + beta_SC_HDSR[8]*cov_HDSR[NDSR_ID[n],8] + 
                    beta_SC_HDSR[9]*cov_HDSR[NDSR_ID[n],9] + beta_SC_HDSR[10]*cov_HDSR[NDSR_ID[n],10])
    
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
  
  ### Habitat Quality ###
  PLS_max <- max(exp(beta_SC_PLSel[1]*cov_NHQ[1:nNHQ,1] +
                       beta_SC_PLSel[2]*cov_NHQ[1:nNHQ,2] +
                       beta_SC_PLSel[3]*cov_NHQ[1:nNHQ,3] +
                       beta_SC_PLSel[4]*cov_NHQ[1:nNHQ,4] +
                       beta_SC_PLSel[5]*cov_NHQ[1:nNHQ,5] +
                       beta_SC_PLSel[6]*cov_NHQ[1:nNHQ,6] +
                       beta_SC_PLSel[7]*cov_NHQ[1:nNHQ,7] +
                       beta_SC_PLSel[8]*cov_NHQ[1:nNHQ,8] +
                       beta_SC_PLSel[9]*cov_NHQ[1:nNHQ,9] +
                       beta_SC_PLSel[10]*cov_NHQ[1:nNHQ,10]))

  LS_max <- max(exp(beta_SC_LSel[1]*cov_NHQ[1:nNHQ,11] +
                      beta_SC_LSel[2]*cov_NHQ[1:nNHQ,12] +
                      beta_SC_LSel[3]*cov_NHQ[1:nNHQ,13] +
                      beta_SC_LSel[4]*cov_NHQ[1:nNHQ,14] +
                      beta_SC_LSel[5]*cov_NHQ[1:nNHQ,15] +
                      beta_SC_LSel[6]*cov_NHQ[1:nNHQ,16] +
                      beta_SC_LSel[7]*cov_NHQ[1:nNHQ,17] +
                      beta_SC_LSel[8]*cov_NHQ[1:nNHQ,18] +
                      beta_SC_LSel[9]*cov_NHQ[1:nNHQ,19] +
                      beta_SC_LSel[10]*cov_NHQ[1:nNHQ,20]))

  NS_max <- max(exp(beta_SC_NSel[1]*cov_NHQ[1:nNHQ,21] +
                      beta_SC_NSel[2]*cov_NHQ[1:nNHQ,22] +
                      beta_SC_NSel[3]*cov_NHQ[1:nNHQ,23] +
                      beta_SC_NSel[4]*cov_NHQ[1:nNHQ,24] +
                      beta_SC_NSel[5]*cov_NHQ[1:nNHQ,25] +
                      beta_SC_NSel[6]*cov_NHQ[1:nNHQ,26] +
                      beta_SC_NSel[7]*cov_NHQ[1:nNHQ,27] +
                      beta_SC_NSel[8]*cov_NHQ[1:nNHQ,28] +
                      beta_SC_NSel[9]*cov_NHQ[1:nNHQ,29] +
                      beta_SC_NSel[10]*cov_NHQ[1:nNHQ,30]))
    
  NSucc_max <- max((1/(1 + exp(intercept_NDSR +
                                 beta_SC_NDSR[1]*cov_NHQ[1:nNHQ,31] +
                                 beta_SC_NDSR[2]*cov_NHQ[1:nNHQ,32] +
                                 beta_SC_NDSR[3]*cov_NHQ[1:nNHQ,33] +
                                 beta_SC_NDSR[4]*cov_NHQ[1:nNHQ,34] +
                                 beta_SC_NDSR[5]*cov_NHQ[1:nNHQ,35] +
                                 beta_SC_NDSR[6]*cov_NHQ[1:nNHQ,36] +
                                 beta_SC_NDSR[7]*cov_NHQ[1:nNHQ,37] +
                                 beta_SC_NDSR[8]*cov_NHQ[1:nNHQ,38] +
                                 beta_SC_NDSR[9]*cov_NHQ[1:nNHQ,39] +
                                 beta_SC_NDSR[10]*cov_NHQ[1:nNHQ,40]
                                 ) +
                         exp(intercept_HDSR +
                               beta_SC_HDSR[1]*cov_NHQ[1:nNHQ,41] +
                               beta_SC_HDSR[2]*cov_NHQ[1:nNHQ,42] +
                               beta_SC_HDSR[3]*cov_NHQ[1:nNHQ,43] +
                               beta_SC_HDSR[4]*cov_NHQ[1:nNHQ,44] +
                               beta_SC_HDSR[5]*cov_NHQ[1:nNHQ,45] +
                               beta_SC_HDSR[6]*cov_NHQ[1:nNHQ,46] +
                               beta_SC_HDSR[7]*cov_NHQ[1:nNHQ,47] +
                               beta_SC_HDSR[8]*cov_NHQ[1:nNHQ,48] +
                               beta_SC_HDSR[9]*cov_NHQ[1:nNHQ,49] +
                               beta_SC_HDSR[10]*cov_NHQ[1:nNHQ,50]
                         ))))^40
  
  for(i in 1:nNHQ){
    #Nesting Habitat Quality Metric
    NHQ[i] <- (exp(beta_SC_PLSel[1]*cov_NHQ[i,1] +
                     beta_SC_PLSel[2]*cov_NHQ[i,2] +
                     beta_SC_PLSel[3]*cov_NHQ[i,3] +
                     beta_SC_PLSel[4]*cov_NHQ[i,4] +
                     beta_SC_PLSel[5]*cov_NHQ[i,5] +
                     beta_SC_PLSel[6]*cov_NHQ[i,6] +
                     beta_SC_PLSel[7]*cov_NHQ[i,7] +
                     beta_SC_PLSel[8]*cov_NHQ[i,8] +
                     beta_SC_PLSel[9]*cov_NHQ[i,9] +
                     beta_SC_PLSel[10]*cov_NHQ[i,10])/PLS_max) *
      
      (exp(beta_SC_LSel[1]*cov_NHQ[i,11] +
             beta_SC_LSel[2]*cov_NHQ[i,12] +
             beta_SC_LSel[3]*cov_NHQ[i,13] +
             beta_SC_LSel[4]*cov_NHQ[i,14] +
             beta_SC_LSel[5]*cov_NHQ[i,15] +
             beta_SC_LSel[6]*cov_NHQ[i,16] +
             beta_SC_LSel[7]*cov_NHQ[i,17] +
             beta_SC_LSel[8]*cov_NHQ[i,18] +
             beta_SC_LSel[9]*cov_NHQ[i,19] +
             beta_SC_LSel[10]*cov_NHQ[i,20])/LS_max) *
      
      (exp(beta_SC_NSel[1]*cov_NHQ[i,21] +
             beta_SC_NSel[2]*cov_NHQ[i,22] +
             beta_SC_NSel[3]*cov_NHQ[i,23] +
             beta_SC_NSel[4]*cov_NHQ[i,24] +
             beta_SC_NSel[5]*cov_NHQ[i,25] +
             beta_SC_NSel[6]*cov_NHQ[i,26] +
             beta_SC_NSel[7]*cov_NHQ[i,27] +
             beta_SC_NSel[8]*cov_NHQ[i,28] +
             beta_SC_NSel[9]*cov_NHQ[i,29] +
             beta_SC_NSel[10]*cov_NHQ[i,30])/NS_max) * 
      
      (((1/(1 + exp(intercept_NDSR +
                     beta_SC_NDSR[1]*cov_NHQ[i,31] +
                     beta_SC_NDSR[2]*cov_NHQ[i,32] +
                     beta_SC_NDSR[3]*cov_NHQ[i,33] +
                     beta_SC_NDSR[4]*cov_NHQ[i,34] +
                     beta_SC_NDSR[5]*cov_NHQ[i,35] +
                     beta_SC_NDSR[6]*cov_NHQ[i,36] +
                     beta_SC_NDSR[7]*cov_NHQ[i,37] +
                     beta_SC_NDSR[8]*cov_NHQ[i,38] +
                     beta_SC_NDSR[9]*cov_NHQ[i,39] +
                     beta_SC_NDSR[10]*cov_NHQ[i,40]) +
             exp(intercept_HDSR +
                   beta_SC_HDSR[1]*cov_NHQ[i,41] +
                   beta_SC_HDSR[2]*cov_NHQ[i,42] +
                   beta_SC_HDSR[3]*cov_NHQ[i,43] +
                   beta_SC_HDSR[4]*cov_NHQ[i,44] +
                   beta_SC_HDSR[5]*cov_NHQ[i,45] +
                   beta_SC_HDSR[6]*cov_NHQ[i,46] +
                   beta_SC_HDSR[7]*cov_NHQ[i,47] +
                   beta_SC_HDSR[8]*cov_NHQ[i,48] +
                   beta_SC_HDSR[9]*cov_NHQ[i,49] +
                   beta_SC_HDSR[10]*cov_NHQ[i,50])))^40)/NSucc_max)
  }
})


NHQ.data <- list(
  ### PreLaying Selection ###
  y_PL = y_PL, # Used/Available Specifications
  wt_PL = weightsPL, #Weights for IWLR
  cov_PLSel = cov_PLSel, # Spatial Covariates (3Dim Array)
  
  ### Laying Selection ###
  y_L = y_L, # Used/Available Specifications
  wt_L = weightsL, #Weights for IWLR
  cov_LSel = cov_LSel, # Spatial Covariates (3Dim Array)
  
  ### Nest Site Selection ###
  y_N = y_N, # Used/Available Specifications
  wt_N = weightsN, #Weights for IWLR
  cov_NSel = cov_NSel, # Spatial Covariates (3Dim Array)
  
  ### Nest Success ###
  NDSR_succ = ns_succ.mat,
  NDSR_interval = ns_interval,
  cov_NDSR = cov_NDSR,
  cov_HDSR = cov_HDSR,
  
  ### Nesting Habitat Quality ###
  cov_NHQ = cov_NHQ
)


NHQ.constants <- list(
  ### PreLaying Selection ###
  NestID_PL = Ind_PLSel, # Numeric Nest ID
  NGrp_PLSel = NInd_PLSel, # Count of Used/Available Groups
  
  ### PreLaying Selection ###
  NestID_L = Ind_LSel, # Numeric Nest ID
  NGrp_LSel = NInd_LSel, # Count of Used/Available Groups
  
  ### PreLaying Selection ###
  NNest_NSel = NInd_NSel, # Number of Nests
  
  ### Nest Success ###
  NDSR_nvisit = length(ns_succ),
  NDSR_ID = ns_ID,
  
  ### Nesting Habitat Quality ###
  nNHQ = nNHQ
)


NHQ.initial <- list(
  beta_SC_PLSel = rep(0, 10),
  beta_SC_LSel = rep(0, 10),
  beta_SC_NSel = rep(0, 10),
  beta_SC_NDSR = rep(0, 10),  
  beta_SC_HDSR = rep(0, 10),
  intercept_HDSR = 0,
  intercept_NDSR = 0
)


NHQ.monitor <- c(
  "beta_SC_PLSel",
  "beta_SC_LSel",
  "beta_SC_NSel",
  "intercept_NDSR",
  "beta_SC_NDSR",
  "intercept_HDSR",
  "beta_SC_HDSR",
  "NHQ"
)


NHQ.dimensions <- list(
  cov_PLSel = dim(NHQ.data$cov_PLSel),
  cov_LSel = dim(NHQ.data$cov_LSel),
  cov_NSel = dim(NHQ.data$cov_NSel),
  cov_NHQ = dim(NHQ.data$cov_NHQ)
)