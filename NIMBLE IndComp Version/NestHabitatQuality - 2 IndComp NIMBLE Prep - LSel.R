########################
### LAYING SELECTION ###
########################

### Nimble Attempt
require(nimble)

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({

  #############################################################################
  
  ### Laying Habitat Selection
  ## Priors
  # Habitat Coefficient
  beta_SC_LSel ~ dnorm(0, 0.001)
  
  # BLISS 
  weights[1:4] <- c(0.25,0.25,0.25,0.25)
  scale_LSel ~ dcat(weights[1:4])
  
  # Individual Random Effect (Slope)
  alpha_L_Slp[1:NNest_LSel] ~ dmnorm(mu_L_S[1:NNest_LSel], omega_L_S[1:NNest_LSel,1:NNest_LSel])
  omega_L_S[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_S[1:NNest_LSel,1:NNest_LSel],NNest_LSel)
  for(i in 1:NNest_LSel){
    mu_L_S[i] <- 0
    for(j in 1:NNest_LSel){
      R_L_S[i,j] <- ifelse(i==j, 1, 0.1)
    }
  }
  
  ## Likelihood
  for(i in 1:NGrp_LSel){
    y_L[i, 1:11] ~ dmulti(p_L[i,1:11], 1)
    for(j in 1:11){
      p_L[i,j] <- e_L[i,j]/inprod(wt_L[i,1:11],e_L[i,1:11])
      log(e_L[i,j]) <- alpha_L_Slp[NestID_L[i]]*cov_LSel[j,scale_LSel,i] + beta_SC_LSel*cov_LSel[j,scale_LSel,i]
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
  beta_SC_LSel = 0
)



### Parameters monitors
NHQ.monitor <- c(
  ### Laying Selection ###
  "beta_SC_LSel",
  "scale_LSel"
)

NHQ.dimensions <- list(
  cov_LSel = dim(NHQ.data$cov_LSel)
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code, NHQ.dimensions, 
     file = paste("./NIMBLE IndComp Version/",covname, "LSel_NHQdata.RData", sep = ""))