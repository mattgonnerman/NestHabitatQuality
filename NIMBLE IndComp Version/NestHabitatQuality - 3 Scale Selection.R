#######################
### SCALE SELECTION ###
#######################

# Load Packages
require(nimble)
require(dplyr)
require(ggplot2)

# Vectors for cycling through each model component and covariates
compnames <- c("PLSel", "LSel", "NSel", "NDSR")
covSelnames <- c("ag_", "dev_", "shrb_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

scaleoutputs <- data.frame(Component = character(), Covariate = character(), ID = character(),
                           S1 = numeric(), S2 = numeric(), S3 = numeric(), S4 = numeric(), TopScale = numeric())
write.csv(scaleoutputs, file = "./NIMBLE IndComp Version/Scale Selection Outputs.csv", row.names = F)
### Run Models ###
for(i in 1:10){ #Number of covariates
  for(j in 1:4){
    rm(list=setdiff(ls(),
                    c("ni", 'nc', "nb", "nt", "covSelnames", "compnames", "i", "j")))
    gc()
    covname = covSelnames[i]
    compname = compnames[j]
    
    # load(file = paste("./NIMBLE IndComp Version/", compname,"_BaseNimbleModel.RData", sep = ""))
    load(file = paste("./NIMBLE IndComp Version/", covname, compname, "_NHQdata.RData", sep = ""))
    load(file = paste("./NIMBLE IndComp Version/", compname, "_BaseNimbleModel.RData", sep = ""))
    
    NHQ.model$setData(NHQ.data)
    rm(list=setdiff(ls(),
                    c("NHQ.comp.MCMC", "NHQ.model",
                      "covname", "compname", "ni", 'nc', "nb", "nt", 'covSelnames', "compnames", 'i')))
    gc()
    NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
                                niter = ni,
                                nburnin = nb,
                                nchain = nc,
                                thin = nt,
                                samples = T)
    
    save(NHQ.samples.MCMC, 
         file = paste("./NIMBLE IndComp Version/",covname, compname, "_MCMC_.RData", sep = ""))
    
    ### Nest Site Selection ###
    if(j == 3){
      convergence.check <- as.data.frame(NHQ.samples.MCMC$samples) 
      
      NSel_t <- table(convergence.check$scale_NSel)
      
      bestscales <- convergence.check %>%
        filter(scale_HDSR == which.max(NSel_t)) %>%
        mutate(Order = row_number())
      
      convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_HDSR)) +
        geom_line()
      ggsave(convergence.plot, filename = paste(covname, "HDSR","_ConvPlot.jpeg", sep = ""),
             path = "./NIMBLE IndComp Version/", device = "jpeg")
      convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_NDSR)) +
        geom_line()
      ggsave(convergence.plot, filename = paste(covname, "NDSR", "_ConvPlot.jpeg", sep = ""),
             path = "./NIMBLE IndComp Version/", device = "jpeg")
      
      scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "HDSR",
                                 S1 = HDSR_t[1,1], S2 = HDSR_t[1,2], S3 = HDSR_t[1,3], S4 = HDSR_t[1,4],
                                 TopScale = which.max(HDSR_t))
      write.table( scaleoutputs1,  
                   file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
                   append = T, 
                   sep=',', 
                   row.names=F, 
                   col.names=F )
      
      scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "HDSR",
                                 S1 = NDSR_t[1,1], S2 = NDSR_t[1,2], S3 = NDSR_t[1,3], S4 = NDSR_t[1,4],
                                 TopScale = which.max(NDSR_t))
      write.table( scaleoutputs1,  
                   file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
                   append = T, 
                   sep=',', 
                   row.names=F, 
                   col.names=F )
    }
    
    ### Nest Success ###
    if(j == 4){
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
    
    scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "HDSR",
                                S1 = HDSR_t[1,1], S2 = HDSR_t[1,2], S3 = HDSR_t[1,3], S4 = HDSR_t[1,4],
                                TopScale = which.max(HDSR_t))
    write.table( scaleoutputs1,  
                 file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
                 append = T, 
                 sep=',', 
                 row.names=F, 
                 col.names=F )
    
    scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "HDSR",
                                S1 = NDSR_t[1,1], S2 = NDSR_t[1,2], S3 = NDSR_t[1,3], S4 = NDSR_t[1,4],
                                TopScale = which.max(NDSR_t))
    write.table( scaleoutputs1,  
                 file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
                 append = T, 
                 sep=',', 
                 row.names=F, 
                 col.names=F )
    }
  }
}

scaleoutputs1 <- data.frame(Component = 1, Covariate = 2, ID = 3, S1 = 4, S2 = 5, S3 = 6, S4 = 7, TopScale = 8)

