require(R2jags)

### Data needed for MCMC
dat <- list(
  # ### PreLaying Selection ###
  # yPL = yPL,
  # wtPL = weightsPL,
  # cov_PLSel = cov_PLSel,
  # Ind_PLSel = Ind_PLSel,
  # NInd_PLSel = NInd_PLSel,
  # N_PLSel = N_PLSel,
  # 
  # ### Laying Selection ###
  # yL = yL,
  # wtL = weightsL,
  # cov_LSel = cov_LSel,
  # Ind_LSel = Ind_LSel,
  # NInd_LSel = NInd_LSel,
  # N_LSel = N_LSel,

  ### Nesting Selection ###
  yN = yN,
  wtN = weightsN,
  cov_NSel = cov_NSel,
  Ind_NSel = Ind_NSel,
  NInd_NSel = NInd_NSel,
  N_NSel = N_NSel,
  
  ### Nest Success ###
  NDSR_succ = ns_succ,
  NDSR_interval = ns_interval,
  NDSR_nvisit = length(ns_succ),
  cov_NDSR = cov_NDSR,
  NDSR_ID = ns_ID,
  
  ### Hen Survival ###
  
  
  ### Nesting Habitat Quality Metric ###
  cov_NHQ = cov_NHQ,
  nNHQ = nrow(cov_NHQ)
)


### Parameters monitors
par.monitor <- c(
  # ### PreLaying Selection ###
  # "intercept_PLSel",
  # "beta_PLSel",
  # "scale_PLSel",
  # "w_PLSel",
  # 
  # ### Laying Selection ###
  # "intercept_LSel",
  # "beta_LSel",
  # "scale_LSel",
  # "w_LSel",

  ### Nest Selection ###
  "intercept_NSel",
  "beta_NSel",
  "scale_NSel",
  "w_NSel",
  
  ### Nest Success ###
  "intercept_NDSR",
  "beta_NDSR",
  "scale_NDSR",
  "w_NDSR",
  
  ### Hen Survival ###
  
  
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
