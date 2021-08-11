########################
### LAYING SELECTION ###
########################
### Justification for using a conditional logistic regression for RSF
## Duchesne et al 2010 - Mixed conditional logsitic regression for habitat selection studies
### Infinitely Weighted Logistic Regression 
## Fithian and Hastie 2013
### Weighted Conditional Logistic Regression Code...
## Lee et al. 2019
### Conditional Logistic Regression Code From...
## Espino-Hernandez et al. 2011

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
  for(i in 1:NNest_PLSel){
    alpha_L_Slp[i] ~ dunif(-10,10)
  }
  # alpha_L_Slp[1:NNest_LSel] ~ dmnorm(mu_L[1:NNest_LSel], omega_L_S[1:NNest_LSel,1:NNest_LSel])
  # mu_L[1:NNest_LSel] <- rep(0, NNest_LSel)
  # omega_L_S[1:NNest_LSel,1:NNest_LSel] ~ dwish(R_L_S[1:NNest_LSel,1:NNest_LSel],NNest_LSel)
  # R_L_S[1:NNest_LSel,1:NNest_LSel] <- diag(rep(1, NNest_LSel))
   
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
  beta_SC_LSel = 0,
  alpha_L_Slp = rep(0, N_LSel),
  omega_L_S = diag(rep(1, N_LSel))
)



### Parameters monitors
NHQ.monitor <- c(
  ### Laying Selection ###
  "beta_SC_LSel",
  "scale_LSel",
  "alpha_L_Slp"
)

NHQ.dimensions <- list(
  cov_LSel = dim(NHQ.data$cov_LSel)
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code, NHQ.dimensions, 
     file = paste("./NIMBLE IndComp Version/",covname, "LSel_NHQdata.RData", sep = ""))