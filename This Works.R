### Nimble Attempt
require(nimble)
require(dplyr)
require(ggplot2)
require(tidyr)

# Vectors for cycling through each model component and covariates
compnames <- c("PLSel", "LSel", "NSel", "NDSR")
covSelnames <- c("ag_", "dev_", "shrub_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

### MCMC settings
ni <- 20000 #number of iterations
nt <- 1 #thinning
nb <- 10000 #burn in period
nc <- 1 #number of chains/parallel cores

### Model in BUGS code adjusted for NIMBLE
NHQ.code <- nimbleCode({
  
  #############################################################################
  
  # Habitat Coefficient
  beta_SC_PLSel ~ dnorm(0, 0.001)
  
  # BLISS 
  weights[1:4] <- c(0.25,0.25,0.25,0.25)
  scale_PLSel ~ dcat(weights[1:4])
  
  # Individual Random Effect (Slope)
  for(i in 1:NNest_PLSel){
    alpha_PL_Slp[i] ~ dunif(-10,10)
  }
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
      log(e_PL[i,j]) <- alpha_PL_Slp[NestID_PL[i]] + beta_SC_PLSel*cov_PLSel[j,scale_PLSel,i]
      # log(e_PL[i,j]) <- beta_SC_PLSel*cov_PLSel[j,scale_PLSel,i]
    }
  }
  
  #############################################################################
  
})

covname <- covSelnames[1]

#Pre-Laying Selection
cov_PLSel1 <- st_drop_geometry(pl.covs[,c(1,which(grepl(covname, colnames(pl.covs))), ncol(pl.covs))])
cov_PLSel2 <- merge(pl.df %>% dplyr::select(NestID, PairID, MergeID, Used), cov_PLSel1, by = c("NestID", "MergeID")) %>%
  arrange(NestID, PairID, desc(Used), MergeID) %>%
  mutate(ID = as.factor(paste(NestID, PairID, sep = "_"))) %>%
  group_by(ID) %>%
  dplyr::select(-NestID, -PairID, -Used, -MergeID)

#Z - Scale
cov_PLSel2[,1] <- scale(cov_PLSel2[,1], center = TRUE, scale = TRUE)
cov_PLSel2[,2] <- scale(cov_PLSel2[,2], center = TRUE, scale = TRUE)
cov_PLSel2[,3] <- scale(cov_PLSel2[,3], center = TRUE, scale = TRUE)
cov_PLSel2[,4] <- scale(cov_PLSel2[,4], center = TRUE, scale = TRUE)

cov_PLSel3 <- split(cov_PLSel2[,1:4], cov_PLSel2$ID)
cov_PLSel <- array(as.numeric(unlist(cov_PLSel3)), dim=c(11, 4, length(cov_PLSel3)))

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
  NGrp_PLSel = NInd_PLSel, # Count of Used/Available Groups
  NNest_PLSel = N_PLSel # Count of Individual Nests
)

NHQ.initial <- list(
  ### PreLaying Selection ###
  scale_PLSel = 1,
  beta_SC_PLSel = 0,
  alpha_PL_Slp = rep(0, N_PLSel),
  omega_PL_S = diag(rep(1, N_PLSel))
)

### Parameters monitors
NHQ.monitor <- c(
  ### PreLaying Selection ###
  "beta_SC_PLSel",
  "scale_PLSel",
  "alpha_PL_Slp"
)

NHQ.dimensions <- list(
  cov_PLSel = dim(NHQ.data$cov_PLSel)
)


NHQ.model <- nimbleModel(code = NHQ.code,
                         constants = NHQ.constants,
                         dimensions = NHQ.dimensions,
                         inits = NHQ.initial,
                         data = NHQ.data)

for(i in 1){ #Number of covariates
  for(j in 1){
    rm(list=setdiff(ls(),
                    c("ni", 'nc', "nb", "nt", "covSelnames", "compnames", "i", "j", "NHQ.model")))
    gc()
    covname = covSelnames[i]
    compname = compnames[j]
    
    load(file = paste("./NIMBLE IndComp Version/", covname, compname, "_NHQdata.RData", sep = ""))
    NHQ.monitor <- c(
      ### PreLaying Selection ###
      "beta_SC_PLSel",
      "scale_PLSel",
      "alpha_PL_Slp"
    )
    NHQ.model$setData(NHQ.data)
    NHQ.comp.model <- compileNimble(NHQ.model)
    NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                                   monitors = NHQ.monitor,
                                   enableWAIC = T)
    NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
    NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
    
    # rm(list=setdiff(ls(),
    #                 c("NHQ.comp.MCMC", "NHQ.model",
    #                   "covname", "compname", "ni", 'nc', "nb", "nt", 'covSelnames', "compnames", 'i', 'j')))
    # gc()
    NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                                niter = ni,
                                nburnin = nb,
                                nchain = nc,
                                thin = nt,
                                summary = T,
                                samples = T,
                                WAIC = TRUE)
    
    # save(NHQ.samples.MCMC, file = paste("./NIMBLE IndComp Version/",covname, compname, "_MCMC_.RData", sep = ""))
    
    ### PreLaying Selection ###
    if(j == 1){
      convergence.check <- as.data.frame(NHQ.samples.MCMC$samples) 
      
      PLSel_t <- table(convergence.check$scale_PLSel)
      
      bestscales <- convergence.check %>%
        group_by(scale_PLSel) %>%
        mutate(as.factor(scale_PLSel)) %>%
        mutate(Order = row_number())
      
      convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_PLSel, group = scale_PLSel)) +
        geom_line() +
        facet_wrap(~scale_PLSel)
      # ggsave(convergence.plot, filename = paste(covname, compname, "_ConvPlot.jpeg", sep = ""),
      #        path = "./NIMBLE IndComp Version/", device = "jpeg")
      
      scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "beta_SC_PLSel",
                                 S1 = PLSel_t[1], S2 = PLSel_t[2], S3 = PLSel_t[3], S4 = PLSel_t[4],
                                 TopScale = which.max(PLSel_t))
      # write.table( scaleoutputs,  
      #              file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
      #              append = T, 
      #              sep=',', 
      #              row.names=F, 
      #              col.names=F )
    }
  }
}
