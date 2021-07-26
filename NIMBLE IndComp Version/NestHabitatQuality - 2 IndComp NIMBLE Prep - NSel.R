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
  beta_SC_NSel ~ dnorm(0, 0.001) 
  
  # BLISS 
  weights[1:4] <- c(0.25,0.25,0.25,0.25)
  scale_NSel ~ dcat(weights[1:4])
  
  ## Likelihood
  for(i in 1:NNest_NSel){
    y_N[i, 1:11] ~ dmulti(p_N[i,1:11], 1)
    for(j in 1:11){
      p_N[i,j] <- e_N[i,j]/inprod(wt_N[i,1:11],e_N[i,1:11])
      log(e_N[i,j]) <- beta_SC_NSel*cov_NSel[j,scale_NSel,i] #+ err_N[i,j]
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
  # NestID_N = Ind_NSel, # Numeric Nest ID
  NNest_NSel = NInd_NSel # Number of Nests
)

NHQ.initial <- list(
  ### Nesting Selection ###
  scale_NSel = 1,
  beta_SC_NSel = 0
)



### Parameters monitors
NHQ.monitor <- c(
  ### Nest Selection ###
  "beta_SC_NSel",
  "scale_NSel"
)

NHQ.dimensions <- list(
  cov_NSel = dim(NHQ.data$cov_NSel)
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code, NHQ.dimensions, 
     file = paste("./NIMBLE IndComp Version/",covname, "NSel_NHQdata.RData", sep = ""))