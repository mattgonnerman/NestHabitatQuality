require(nimble)

### Data Prep

#Vectors for cycling through each model component and covariates
compnames <- c("PLSel", "LSel", "NSel", "NDSR")
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")


############################################################################
### ONLY NEED TO RUN THIS SECTION ONCE TO SPEED UP PROCESS DOWN THE LINE ###
# source(file = "NIMBLE IndComp Version/NestHabitatQuality - 1 IndComp NIMBLE Data Prep.R")


### Initial Model Prep
#May be necessary to restart R to get completely clear RAM
# require(nimble)
# rm(list=setdiff(ls(),
#                 c("compnames","covSelnames")))
# gc()
# 
# for(i in 1:4){
#   load(file = paste("./NIMBLE IndComp Version/ag_", compnames[i], "_NHQdata.RData", sep = ""))
  # NHQ.model <- nimbleModel(code = NHQ.code,
  #                          constants = NHQ.constants,
  #                          dimensions = NHQ.dimensions,
  #                          inits = NHQ.initial,
  #                          data = NHQ.data)
#   save(NHQ.model, file = paste("./NIMBLE IndComp Version/", compnames[i], "_BaseNimbleModel.RData", sep = ""))
# }

### ONLY NEED TO RUN THIS SECTION ONCE TO SPEED UP PROCESS DOWN THE LINE ###
############################################################################


### Run NIMBLE Models ###
#MCMC settings
ni <- 30000 #number of iterations
nt <- 1 #thinning
nb <- 10000 #burn in period
nc <- 1 #number of chains/parallel cores

#######################
### SCALE SELECTION ###
#######################
### Nest Success
for(i in 1:10){ #Number of covariates
# for(i in 3){ #Number of covariates
  for(j in 4){ #Number of components
    rm(list=setdiff(ls(),
                    c("ni", 'nc', "nb", "nt", "covSelnames", "compnames", "i", "j")))
    gc()
    covname = covSelnames[i]
    compname = compnames[j]
    
    # load(file = paste("./NIMBLE IndComp Version/", compname,"_BaseNimbleModel.RData", sep = ""))
    load(file = paste("./NIMBLE IndComp Version/", covname, compname, "_NHQdata.RData", sep = ""))
  
    
    ### Parameters monitors
    NHQ.monitor <- c(
      ### Nest Success ###
      "intercept_NDSR",
      "beta_SC_NDSR",
      "intercept_HDSR",
      "beta_SC_HDSR",
      "scale_NDSR",
      "scale_HDSR"
    )

    NHQ.initial <- list(
      ### Nest Success ###
      scale_NDSR = 1,
      beta_SC_NDSR = 0,  
      scale_HDSR = 1,
      beta_SC_HDSR = 0
    )

    ### Model in BUGS code adjusted for NIMBLE
    NHQ.code <- nimbleCode({
      
      #############################################################################
      
      ### Nest/Hen Mort/Flush Daily Failure Risk ###
      ## Priors ##
      # Nest Only Failure
      intercept_NDSR ~ dnorm(0, 0.001)
      beta_SC_NDSR ~ dnorm(0, 0.001)
      
      # Hen Mortality
      intercept_HDSR ~ dnorm(0, 0.001)
      beta_SC_HDSR ~ dnorm(0, 0.001)
      
      # BLISS
      weights[1:4] <- c(0.25,0.25,0.25,0.25)
      scale_NDSR ~ dcat(weights[1:4])
      scale_HDSR ~ dcat(weights[1:4])
      
      #Likelihood
      for(n in 1:NDSR_nvisit){
        #Linear Predictors
        cts[n] <- 1
        ctn[n] <- exp(intercept_NDSR + beta_SC_NDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
        cth[n] <- exp(intercept_HDSR + beta_SC_HDSR*cov_NDSR[NDSR_ID[n],scale_NDSR])
        
        #Denominator
        den[n] <- cts[n] + ctn[n] + cth[n] #+ ctf[n]
        
        #Daily Survival Probability
        survp[n] <- cts[n]/den[n]
        
        #Interval Survival Probability
        p[n,1] <- pow(survp[n], NDSR_interval[n])
        
        #Interval Probabilities of Nest/Hen Loss
        p[n,2] <- ((ctn[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
        p[n,3] <- ((cth[n]/den[n])/(1 - survp[n]))*(1 - pow(survp[n], NDSR_interval[n]))
        
        #Multinomial likelihood
        NDSR_succ[n,1:3] ~ dmulti(p[n,1:3],1)
      }
      
      #############################################################################
      
    })
    
    NHQ.model <- nimbleModel(code = NHQ.code,
                             constants = NHQ.constants,
                             inits = NHQ.initial,
                             data = NHQ.data)
    
    NHQ.model$setData(NHQ.data)
    NHQ.comp.model <- compileNimble(NHQ.model)
    NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                                   monitors = NHQ.monitor,
                                   enableWAIC = T)
    NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
    NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
    rm(list=setdiff(ls(),
                    c("NHQ.comp.MCMC",
                      "covname", "compname", "ni", 'nc', "nb", "nt", 'covSelnames', "compnames", 'i')))
    gc()
    NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                                niter = ni,
                                nburnin = nb,
                                nchain = nc,
                                thin = nt, 
                                summary = T,
                                samples = T,
                                WAIC = T)
    
    save(NHQ.samples.MCMC, 
         file = paste("./NIMBLE IndComp Version/",covname, compname, "_MCMC.RData", sep = ""))
    write.csv(NHQ.samples.MCMC$samples, 
              paste("./NIMBLE IndComp Version/", covname, compname, "_modelsamples.csv", sep = ""))
    
    convergence.check <- as.data.frame(NHQ.samples.MCMC$samples) 
    
    HDSR_t <- table(convergence.check$scale_HDSR)
    NDSR_t <- table(convergence.check$scale_NDSR)
    
    bestscales <- convergence.check%>%
      filter(scale_HDSR == which.max(HDSR_t)) %>%
      filter(scale_NDSR == which.max(NDSR_t)) %>%
      mutate(Order = row_number())
  
    convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_HDSR)) +
      geom_line()
    ggsave(convergence.plot, filename = paste(covname, "HDSR","_ConvPlot.jpeg", sep = ""),
           path = "./NIMBLE IndComp Version/", device = "jpeg")
    convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_NDSR)) +
      geom_line()
    ggsave(convergence.plot, filename = paste(covname, "NDSR", "_ConvPlot.jpeg", sep = ""),
           path = "./NIMBLE IndComp Version/", device = "jpeg")
  }
}

### Collet WAIC Values into table
rm("waic_table")
waic_table <- data.frame(Component = NA,
                   Covariate = NA,
                   WAIC = NA)
for(i in 1:10){ #Number of covariates
    for(j in 4){ #Number of components
      load(file = paste("./NIMBLE IndComp Version/", covSelnames[i], compnames[j], "_MCMC.RData", sep = ""))
      waic_table[i,1] <- compnames[j]
      waic_table[i,2] <- covSelnames[i]
      waic_table[i,3] <- NHQ.samples.MCMC$WAIC
    }
}

require(dplyr)
waic_table %>% arrange(WAIC)

### Create NHQ Value Rasters

