### Nimble Attempt
require(nimble)

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({

  #############################################################################
  
  ### Laying Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_LSel ~ dnorm(0, tau_LSel) 
  tau_LSel <- 1/(pow(sigma_LSel,2))
  sigma_LSel ~ dunif(0,50)
  
  # BLISS 
  scale_LSel ~ dcat(w_LSel[1:4])
  w_LSel[1:4] ~ ddirch(dirpw[1:4])
  
  # Individual Random Effect (Intercept)
  alpha_L_Int[1:NNest_LSel] ~ dmnorm(mu_L_I[1:NNest_LSel], omega_L_I[1:NNest_LSel,1:NNest_LSel])
  omega_L_I[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_I[1:NNest_LSel,1:NNest_LSel],NNest_LSel)
  for(i in 1:NNest_LSel){
    mu_L_I[i] <- 0
    for(j in 1:NNest_LSel){
      R_L_I[i,j] <- ifelse(i==j, 1, 0.1)
    }
  }
  
  # Individual Random Effect (Slope)
  alpha_L_Slp[1:NNest_LSel] ~ dmnorm(mu_L_S[1:NNest_LSel], omega_L_S[1:NNest_LSel,1:NNest_LSel])
  omega_L_S[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_S[1:NNest_LSel,1:NNest_LSel],NNest_LSel)
  for(i in 1:NNest_LSel){
    mu_L_S[i] <- 0
    for(j in 1:NNest_LSel){
      R_L_S[i,j] <- ifelse(i==j, 1, 0.1)
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
  
})

### Data for NIMBLE
NHQ.data <- list(
  ### Laying Selection ###
  y_L = y_L, # Used/Available Specifications
  wt_L = weightsL, #Weights for IWLR
  cov_LSel = cov_LSel # Spatial Covariates (3Dim Array)
)


### Constants for NIMBLE
NHQ.constants <- list(
  ### Laying Selection ###
  NestID_L = Ind_LSel, # Numeric Nest ID
  NGrp_LSel = NInd_LSel, # Count of Used/Available Groups
  NNest_LSel = N_LSel # Count of Individual Nests
)

NHQ.initial <- list(
  ### Laying Selection ###
  scale_LSel = 1,
  sigma_LSel = 1,
  beta_SC_LSel = 0
)



### Parameters monitors
NHQ.monitor <- c(
  ### Laying Selection ###
  "beta_SC_LSel",
  "sigma_LSel",
  "scale_LSel",
  "w_LSel",
  "err_L"
)

NHQ.dimensions <- list(
  cov_LSel = dim(NHQ.data$cov_LSel)
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code, NHQ.dimensions, 
     file = paste("./NIMBLE IndComp Version/",covname, "NSel_NHQdata.RData", sep = ""))