###########################
### PRELAYING SELECTION ###
###########################
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
  
  # Habitat Coefficient
  beta_SC_PLSel ~ dnorm(0, 0.001)
  
  # BLISS 
  weights[1:4] <- c(0.25,0.25,0.25,0.25)
  scale_PLSel ~ dcat(weights[1:4])
  
  # Individual Random Effect (Slope)
  # for(i in 1:NNest_PLSel){
  #   alpha_PL_Slp[i] ~ dunif(-10,10)
  # }
  # alpha_PL_Slp[1:NNest_PLSel] ~ dmnorm(mu_PL[1:NNest_PLSel], omega_PL_S[1:NNest_PLSel,1:NNest_PLSel])
  # mu_PL[1:NNest_PLSel] <- rep(0, NNest_PLSel)
  # omega_PL_S[1:NNest_PLSel,1:NNest_PLSel] ~ dwish(R_PL_S[1:NNest_PLSel,1:NNest_PLSel], NNest_PLSel)
  # R_PL_S[1:NNest_PLSel,1:NNest_PLSel] <- diag(rep(0.1, NNest_PLSel))

  ## Likelihood
  for(i in 1:NGrp_PLSel){
    y_PL[i, 1:11] ~ dmulti(p_PL[i,1:11], 1)
    for(j in 1:11){
      p_PL[i,j] <- e_PL[i,j]/inprod(wt_PL[i,1:11],e_PL[i,1:11])
      # log(e_PL[i,j]) <- alpha_PL_Slp[NestID_PL[i]]*cov_PLSel[j,scale_PLSel,i] + beta_SC_PLSel*cov_PLSel[j,scale_PLSel,i]
      log(e_PL[i,j]) <- beta_SC_PLSel*cov_PLSel[j,scale_PLSel,i]
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
  NGrp_PLSel = NInd_PLSel # Count of Used/Available Groups
  # NNest_PLSel = N_PLSel # Count of Individual Nests
)

NHQ.initial <- list(
  ### PreLaying Selection ###
  scale_PLSel = 1,
  beta_SC_PLSel = 0
  # alpha_PL_Slp = rep(0, N_PLSel),
  # omega_PL_S = diag(rep(1, N_PLSel))
)



### Parameters monitors
NHQ.monitor <- c(
  ### PreLaying Selection ###
  "beta_SC_PLSel",
  "scale_PLSel"
  # "alpha_PL_Slp"
  
)

NHQ.dimensions <- list(
  cov_PLSel = dim(NHQ.data$cov_PLSel)
)

save(NHQ.data, NHQ.constants, NHQ.initial, NHQ.monitor, NHQ.code, NHQ.dimensions, 
     file = paste("./NIMBLE IndComp Version/",covname, "PLSel_NHQdata.RData", sep = ""))