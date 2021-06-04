require(R2jags)

### Data needed for MCMC
dat <- list(
  ### PreLaying Selection ###
  y_PL = yPL, # Used/Available Specifications
  wt_PL = weightsPL, #Weights for IWLR
  cov_PLSel = cov_PLSel, # Spatial Covariates (3Dim Array)
  NestID_PL = Ind_PLSel, # Numeric Nest ID
  NGrp_PLSel = NInd_PLSel, # Count of Used/Available Groups
  NNest_PLSel = N_PLSel, # Count of Individual Nests

  ### Laying Selection ###
  y_L = yL, # Used/Available Specifications
  wt_L = weightsL, #Weights for IWLR
  cov_LSel = cov_LSel, # Spatial Covariates (3Dim Array)
  NestID_L = Ind_LSel, # Numeric Nest ID
  NGrp_LSel = NInd_LSel, # Count of Used/Available Groups
  NNest_LSel = N_LSel, # Count of Individual Nests

  ### Nesting Selection ###
  y_N = yN, # Used/Available Specifications
  wt_N = weightsN, #Weights for IWLR
  cov_NSel = cov_NSel, # Spatial Covariates (3Dim Array)
  NestID_N = Ind_NSel, # Numeric Nest ID
  NGrp_NSel = NInd_NSel, # Count of Used/Available Groups
  NNest_NSel = N_NSel, # Count of Individual Nests
  
  ### Nest Success ###
  NDSR_succ = ns_succ.mat,
  NDSR_interval = ns_interval,
  NDSR_nvisit = length(ns_succ),
  cov_NDSR = cov_NDSR,
  NDSR_ID = ns_ID,

  ### Nesting Habitat Quality Metric ###
  cov_NHQ = cov_NHQ,
  nNHQ = nrow(cov_NHQ)
)


### Parameters monitors
par.monitor <- c(
  ### PreLaying Selection ###
  "intercept_PLSel",
  "beta_SC_PLSel",
  "sigma_PLSel",
  "scale_PLSel",
  "w_PLSel",

  ### Laying Selection ###
  "intercept_LSel",
  "beta_SC_LSel",
  "sigma_LSel",
  "scale_LSel",
  "w_LSel",

  ### Nest Selection ###
  "intercept_NSel",
  "beta_SC_NSel",
  "sigma_NSel",
  "scale_NSel",
  "w_NSel",
  
  ### Nest Success ###
  "intercept_NDSR",
  "beta_SC_NDSR",
  "sigma_NDSR",
  "intercept_HDSR",
  "beta_SC_HDSR",
  "sigma_HDSR",
  "intercept_FDSR",
  "beta_SC_FDSR",
  "sigma_FDSR",
  "scale_NDSR",
  "w_NDSR",
  "scale_HDSR",
  "w_HDSR",
  "scale_FDSR",
  "w_FDSR",

  ### Nesting Habitat Quality Metric ###
  "NHQ"
)


### Initial Values for select parameters

inits <- function(){
  list(
    )
}

### Object names needed for parallel cores
names_for_parallel <- c("nb", 
                        "nt", 
                        "nc",
                        "ni")


### Model for JAGS
NHQ_model <- source(file = "NestHabitatQuality - 3 JAGS Model.R")$value


### Run model in parallel
NHQ_output <- jags.parallel(data = dat,
                                 parameters.to.save = par.monitor,
                                 # inits = inits,
                                 model.file = NHQ_model,
                                 n.iter = ni,
                                 n.burnin = nb,
                                 n.thin = nt,
                                 n.chains = nc,
                                 export_obj_names = names_for_parallel)

# ### Run model normally
# NHQ_output <- jags(data = dat,
#                             parameters.to.save = par.monitor,
#                             # inits = inits,
#                             model.file = NHQ_model,
#                             n.iter = ni,
#                             n.burnin = nb,
#                             n.thin = nt,
#                             n.chains = 3
#                    ) 
