###
setwd("E:/GitHub/NestHabitatQuality")

### Data Preperation
# # Nest Site Selection
# source(file = "NestHabitatQuality - 1a Selection Data.R")
# 
# # Nest Success
source(file = "NestHabitatQuality - 1b Nest Success Data.R")
# 
# # Hen Survival
# source(file = "NestHabitatQuality - 1c Hen Survival Data.R")
# 
# # Habitat Covariates
# source(file = "NestHabitatQuality - 1d Habitat Covariates Data.R")


### Run JAGS Model
#MCMC settings
ni <- 1000 #number of iterations
nt <- 1 #thinning
nb <- 100 #burn in period
nc <- 5 #number of chains/parallel cores

#Spatial Covariate Column Prefixes
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
  "BA_", "HT_", "SW_",
  "D2Edg_", "D2Rd_", "D2Rp_")

NHQ_output_list <- list()
NHQ_waic <- list()
# for(i in 1:length(covSelnames)){
for(i in 1){
  #Specify covariate of interest
  covname <- covSelnames[i]
  
  print(paste("Run", covname, "Start Time:", Sys.time(), sep = " "))
  
  # Prep Data
  source(file = "NestHabitatQuality - 1 JAGS Data Prep.R")
  
  #Run JAGS Model
  source(file = "NestHabitatQuality - 2 Execute JAGS.R")
  
  #Save Model to list
  NHQ_output_list[[i]] <- NHQ_output
  
  # # compute wAIC for model with intercept only
  # NHQ_samples <- jags.samples(NHQ_output$model, 
  #                            c("WAIC","deviance"), 
  #                            type = "mean", 
  #                            n.iter = ni,
  #                            n.burnin = nb,
  #                            n.thin = nt)
  # NHQ_samples$p_waic <- NHQ_samples$WAIC
  # NHQ_samples$waic <- NHQ_samples$deviance + NHQ_samples$p_waic
  # tmp <- sapply(NHQ_samples, sum)
  # NHQ_waic[[i]] <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
  
  print(paste("Run", covname, "End Time:", Sys.time(), sep = " "))
}


### Examine Model Outputs
#Create NHQ Raster from Outputs
createNHQraster <- function(df){
  rawdf <- as.data.frame(df$BUGSoutput$summary)
  NHQest.df <- rawdf %>% 
    mutate(Name = rownames(rawdf)) %>%
    filter(grepl(x = Name, pattern = "NHQ"))
  NHQest <- NHQest.df$mean
  NHQ.points <- NHQ.covs %>%
    dplyr::select(geometry)
  NHQ.points$NHQ <- NHQest
  NHQ.raster <- raster(NHQ.points, crs = crs(NHQ.points), vals = 0, resolution = 1000, ext = extend(extent(NHQ.points), 1000))
  NHQ.raster <- shift(NHQ.raster, dx = 500, dy = 500)
  NHQ.raster <- rasterize(st_coordinates(NHQ.points)[,1:2], NHQ.raster, field = NHQ.points$NHQ)
  NHQ.raster
}

NHQ.rasters.list <- lapply(NHQ_output_list, createNHQraster)
plot(NHQ.rasters.list[[1]])
