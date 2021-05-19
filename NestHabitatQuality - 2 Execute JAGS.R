require(R2jags)

### Data needed for MCMC
dat <- list(
  ### PreLaying Selection ###
  yPL = yPL, 
  cov_PLSel = cov_PLSel,
  Ind_PLSel = Ind_PLSel,
  NInd_PLSel = NInd_PLSel,
  N_PLSel = N_PLSel,
  
  ### Laying Selection ###
  yL = yL, 
  cov_LSel = cov_LSel,
  Ind_LSel = Ind_LSel,
  NInd_LSel = NInd_LSel,
  N_LSel = N_LSel,
  
  ### Nesting Selection ###
  yN = yN, 
  cov_NSel = cov_NSel,
  Ind_NSel = Ind_NSel,
  NInd_NSel = NInd_NSel,
  N_NSel = N_NSel
)


### Parameters monitors
par.monitor <- c(
  ### PreLaying Selection ###
  "intercept_PLSel",
  "beta_PLSel",
  "scale_PLSel",
  
  ### PreLaying Selection ###
  "intercept_LSel",
  "beta_LSel",
  "scale_LSel",
  
  ### PreLaying Selection ###
  "intercept_NSel",
  "beta_NSel",
  "scale_NSel"
)


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
                                 # inits = inits.null,
                                 model.file = NHQ_model,
                                 n.iter = ni,
                                 n.burnin = nb,
                                 n.thin = nt,
                                 n.chains = nc,
                                 export_obj_names = names_for_parallel) 