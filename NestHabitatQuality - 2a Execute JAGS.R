require(R2jags)

### Data needed for MCMC
dat <- list(
  ### Daily Survival Rate ###
  dsr_succ = dsr_succ,
  dsr_interval = dsr_interval,
  dsr_nvisit = length(dsr_succ),
  dsr_adult = dsr_adult, #Adult = 1, Juv = 0
)


### Parameters monitors
parameters.null <- c(
  ### Daily Survival Rate ###
  "intercept_DHM",
  "beta_A_DHM"
)


### Object names needed for parallel cores
names_for_parallel <- c()


### Model for JAGS
NHQ_model <- source(file = "NestHabitatQuality - 3a JAGS Model.R")$value


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