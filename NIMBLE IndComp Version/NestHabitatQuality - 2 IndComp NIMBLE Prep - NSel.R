###########################
### NEST SITE SELECTION ###
###########################

### Make sure r-NIMBLE is loaded
require(nimble)

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({
  #############################################################################
  
  ### Nesting Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_NSel ~ dnorm(0, tau_NSel) 
  tau_NSel <- 1/(pow(sigma_NSel,2))
  sigma_NSel ~ dunif(0,50)
  
  # BLISS 
  scale_NSel ~ dcat(w_NSel[1:4])
  w_NSel[1:4] ~ ddirch(dirpw[1:4])
  
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
})

### Data for NIMBLE
NHQ.data <- list(
  ### Nesting Selection ###
  y_N = y_N, # Used/Available Specifications
  wt_N = weightsN, #Weights for IWLR
  cov_NSel = cov_NSel # Spatial Covariates (3Dim Array)
)


### Constants for NIMBLE
NHQ.constants <- list(
  ### Nesting Selection ###
  NestID_N = Ind_NSel, # Numeric Nest ID
  NGrp_NSel = NInd_NSel, # Count of Used/Available Groups
  NNest_NSel = N_NSel # Count of Individual Nests
)

NHQ.initial <- list(
  ### Nesting Selection ###
  scale_NSel = 1,
  sigma_NSel = 1,
  beta_SC_NSel = 0
)



### Parameters monitors
NHQ.monitor <- c(
  ### Nest Selection ###
  "beta_SC_NSel",
  "sigma_NSel",
  "scale_NSel",
  "w_NSel",
  "err_N"
)

NHQ.dimensions <- list(
  cov_NSel = dim(NHQ.data$cov_NSel)
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code, NHQ.dimensions, 
     file = paste("./NIMBLE IndComp Version/",covname, "NSel_NHQdata.RData", sep = ""))