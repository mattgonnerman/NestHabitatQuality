require(R2jags)

### Data needed for MCMC
dat <- list(
  ### PreLaying Selection ###
  yPL = yPL, 
  cov_PLSel = cov_PLSel,
  Ind_PLSel = Ind_PLSel,
  NInd_PLSel = NInd_PLSel
)


### Parameters monitors
parameters.null <- c(
  ### PreLaying Selection ###
  "intercept_PLSel",
  "beta_PLSel",
  "scale_PLSel"
)


### Object names needed for parallel cores
names_for_parallel <- c()


### Model for JAGS
NHQ_model <- source(file = "NestHabitatQuality - 3 JAGS Model.R")$value


### Run model in parallel
NHQ_output <- jags.parallel(data = dat,
                                 parameters.to.save = parameters.null,
                                 inits = inits.null,
                                 model.file = NHQ_model,
                                 n.iter = ni,
                                 n.burnin = nb,
                                 n.thin = nt,
                                 n.chains = nc,
                                 export_obj_names = names_for_parallel) 