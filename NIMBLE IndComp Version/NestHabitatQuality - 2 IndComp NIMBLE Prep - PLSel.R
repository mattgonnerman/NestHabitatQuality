### Nimble Attempt
require(nimble)

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({
  
  #############################################################################
  ### Prelaying Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_PLSel ~ dnorm(0, tau_PLSel) 
  tau_PLSel <- 1/(pow(sigma_PLSel,2))
  sigma_PLSel ~ dunif(0,50)
  
  # BLISS 
  dirpw[1:4] <- c(0.25,0.25,0.25,0.25)
  scale_PLSel ~ dcat(w_PLSel[1:4])
  w_PLSel[1:4] ~ ddirch(dirpw[1:4])
  
  # Individual Random Effect (Intercept)
  alpha_PL_Int[1:NNest_PLSel] ~ dmnorm(mu_PL_I[1:NNest_PLSel], omega_PL_I[1:NNest_PLSel,1:NNest_PLSel])
  omega_PL_I[1:NNest_PLSel,1:NNest_PLSel] ~ dwish(R_PL_I[1:NNest_PLSel,1:NNest_PLSel],NNest_PLSel)
  for(i in 1:NNest_PLSel){
    mu_PL_I[i] <- 0
    for(j in 1:NNest_PLSel){
      R_PL_I[i,j] <- ifelse(i==j, 1, 0.1)
    }
  }
  
  # Individual Random Effect (Slope)
  alpha_PL_Slp[1:NNest_PLSel] ~ dmnorm(mu_PL_S[1:NNest_PLSel], omega_PL_S[1:NNest_PLSel,1:NNest_PLSel])
  omega_PL_S[1:NNest_PLSel,1:NNest_PLSel] ~ dwish(R_PL_S[1:NNest_PLSel,1:NNest_PLSel],NNest_PLSel)
  for(i in 1:NNest_PLSel){
    mu_PL_S[i] <- 0
    for(j in 1:NNest_PLSel){
      R_PL_S[i,j] <- ifelse(i==j, 1, 0.1)
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
  
})

### Data for NIMBLE
NHQ.data <- list(
  ### PreLaying Selection ###
  y_PL = y_PL, # Used/Available Specifications
  wt_PL = weightsPL, #Weights for IWLR
  cov_PLSel = cov_PLSel # Spatial Covariates (3Dim Array)
)


### Constants for NIMBLE
NHQ.constants <- list(
  ### PreLaying Selection ###
  NestID_PL = Ind_PLSel, # Numeric Nest ID
  NGrp_PLSel = NInd_PLSel, # Count of Used/Available Groups
  NNest_PLSel = N_PLSel # Count of Individual Nests
)

NHQ.initial <- list(
  ### PreLaying Selection ###
  scale_PLSel = 1,
  sigma_PLSel = 1,
  beta_SC_PLSel = 0
)



### Parameters monitors
NHQ.monitor <- c(
  ### PreLaying Selection ###
  "beta_SC_PLSel",
  "sigma_PLSel",
  "scale_PLSel",
  "w_PLSel",
  "err_PL"
)

NHQ.dimensions <- list(
  cov_PLSel = dim(NHQ.data$cov_PLSel)
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code, NHQ.dimensions, 
     file = paste("./NIMBLE IndComp Version/",covname, "PLSel_NHQdata.RData", sep = ""))