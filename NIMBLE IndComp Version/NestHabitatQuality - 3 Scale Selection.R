#######################
### SCALE SELECTION ###
#######################

# Load Packages
require(nimble)
require(dplyr)
require(ggplot2)
require(tidyr)

# Vectors for cycling through each model component and covariates
compnames <- c("PLSel", "LSel", "NSel", "NDSR")
covSelnames <- c("ag_", "dev_", "shrub_", "hrb_",
                 "BA_", "HT_", "SW_",
                 "D2Edg_", "D2Rd_", "D2Rp_")

# scaleoutputs <- data.frame(Component = character(), Covariate = character(), ID = character(),
#                            S1 = numeric(), S2 = numeric(), S3 = numeric(), S4 = numeric(), TopScale = numeric())
# write.csv(scaleoutputs, file = "./NIMBLE IndComp Version/Scale Selection Outputs.csv", row.names = F)
# waicoutputs <- data.frame(Component = character(), Covariate = character(), WAIC = numeric())
# write.csv(waicoutputs, file = "./NIMBLE IndComp Version/Scale Selection WAIC Outputs.csv", row.names = F)

### MCMC settings
ni <- 30000 #number of iterations
nt <- 1 #thinning
nb <- 20000 #burn in period
nc <- 1 #number of chains/parallel cores
        
### Run Models ###
# 
# for(j in 1:3){
#   rm(list=setdiff(ls(),
#                   c("ni", 'nc', "nb", "nt", "covSelnames", "compnames", "j")))
#   gc()
#   
#   #Build Base Model
#   load(file = paste("./NIMBLE IndComp Version/", covSelnames[1], compnames[j], "_NHQdata.RData", sep = ""))
#   
#   NHQ.model <- nimbleModel(code = NHQ.code,
#                            constants = NHQ.constants,
#                            dimensions = NHQ.dimensions,
#                            inits = NHQ.initial,
#                            data = NHQ.data)
#   
#   for(i in 1:10){ #Number of covariates
#     rm(list=setdiff(ls(),
#                     c("ni", 'nc', "nb", "nt", "covSelnames", "compnames", "i", "j", "NHQ.model")))
#     gc()
#     covname = covSelnames[i]
#     compname = compnames[j]
#     
#     load(file = paste("./NIMBLE IndComp Version/", covname, compname, "_NHQdata.RData", sep = ""))
#     
#     NHQ.model$setData(NHQ.data)
#     NHQ.comp.model <- compileNimble(NHQ.model)
#     NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
#                                    monitors = NHQ.monitor,
#                                    enableWAIC = T)
#     NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
#     NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
#     
#     # rm(list=setdiff(ls(),
#     #                 c("NHQ.comp.MCMC", "NHQ.model",
#     #                   "covname", "compname", "ni", 'nc', "nb", "nt", 'covSelnames', "compnames", 'i', 'j')))
#     # gc()
#     NHQ.samples.MCMC <- runMCMC(NHQ.comp.MCMC,
#                                 niter = ni,
#                                 nburnin = nb,
#                                 nchain = nc,
#                                 thin = nt,
#                                 summary = T,
#                                 samples = T,
#                                 WAIC = TRUE)
#     
#     save(NHQ.samples.MCMC, 
#          file = paste("./NIMBLE IndComp Version/",covname, compname, "_MCMC_.RData", sep = ""))
#     
#     ### PreLaying Selection ###
#     if(j == 1){
#       convergence.check <- as.data.frame(NHQ.samples.MCMC$samples) 
#       
#       # t1 <- as.data.frame(NHQ.samples.MCMC$samples[[1]]) %>% mutate(Chain = 1)
#       # t2 <- as.data.frame(NHQ.samples.MCMC$samples[[2]]) %>% mutate(Chain = 2)
#       # t3 <- as.data.frame(NHQ.samples.MCMC$samples[[3]]) %>% mutate(Chain = 3)
#       # convergence.check <- rbind(t1, t2, t3)
#       # bestscales <- convergence.check %>%
#       #   group_by(Chain,scale_PLSel) %>%
#       #   mutate(Order = row_number())
#       # ggplot(data = bestscales, aes(x = Order, y = beta_SC_PLSel, group = Chain)) +
#       #   geom_line(aes(color = Chain)) +
#       #   facet_wrap(~scale_PLSel)
#       
#       PLSel_t <- table(convergence.check$scale_PLSel)
#       
#       bestscales <- convergence.check %>%
#         group_by(scale_PLSel) %>%
#         mutate(as.factor(scale_PLSel)) %>%
#         mutate(Order = row_number())
#       
#       convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_PLSel, group = scale_PLSel)) +
#         geom_line() +
#         facet_wrap(~scale_PLSel)
#       ggsave(convergence.plot, filename = paste(covname, compname, "_ConvPlot.jpeg", sep = ""),
#              path = "./NIMBLE IndComp Version/", device = "jpeg")
#       
#       scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "beta_SC_PLSel",
#                                  S1 = PLSel_t[1], S2 = PLSel_t[2], S3 = PLSel_t[3], S4 = PLSel_t[4],
#                                  TopScale = which.max(PLSel_t))
#       write.table( scaleoutputs,  
#                    file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
#                    append = T, 
#                    sep=',', 
#                    row.names=F, 
#                    col.names=F )
#     }
#     
#     ### Nest Site Selection ###
#     if(j == 2){
#       convergence.check <- as.data.frame(NHQ.samples.MCMC$samples) 
#       
#       LSel_t <- table(convergence.check$scale_LSel)
#       
#       bestscales <- convergence.check %>%
#         group_by(scale_LSel) %>%
#         mutate(as.factor(scale_LSel)) %>%
#         mutate(Order = row_number())
#       
#       convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_LSel, group = scale_LSel)) +
#         geom_line() +
#         facet_wrap(~scale_LSel)
#       ggsave(convergence.plot, filename = paste(covname, compname, "_ConvPlot.jpeg", sep = ""),
#              path = "./NIMBLE IndComp Version/", device = "jpeg")
#       
#       scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "beta_SC_LSel",
#                                  S1 = LSel_t[1], S2 = LSel_t[2], S3 = LSel_t[3], S4 = LSel_t[4],
#                                  TopScale = which.max(LSel_t))
#       write.table( scaleoutputs,  
#                    file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
#                    append = T, 
#                    sep=',', 
#                    row.names=F, 
#                    col.names=F )
#     }
#     
#     ### Nest Site Selection ###
#     if(j == 3){
#       
#       # #for multiple chains
#       # t1 <- as.data.frame(NHQ.samples.MCMC$samples[[1]]) %>% mutate(Chain = 1)
#       # t2 <- as.data.frame(NHQ.samples.MCMC$samples[[2]]) %>% mutate(Chain = 2)
#       # t3 <- as.data.frame(NHQ.samples.MCMC$samples[[3]]) %>% mutate(Chain = 3)
#       # convergence.check <- rbind(t1, t2, t3)
#       # 
#       # bestscales <- convergence.check %>%
#       #   group_by(Chain, scale_PLSel) %>%
#       #   mutate(Order = row_number())
#       # 
#       # convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_PLSel, group = scale_PLSel)) +
#       #   geom_line(aes(color = Chain)) +
#       #   facet_wrap(~scale_PLSel)
#       # ggsave(convergence.plot, filename = paste(covname, compname, "_ConvPlot.jpeg", sep = ""),
#       #        path = "./NIMBLE IndComp Version/", device = "jpeg")
#       
#       
#       convergence.check <- as.data.frame(NHQ.samples.MCMC$samples) 
#       
#       NSel_t <- table(convergence.check$scale_NSel)
#       
#       bestscales <- convergence.check %>%
#         group_by(scale_NSel) %>%
#         mutate(as.factor(scale_NSel)) %>%
#         mutate(Order = row_number())
#       
#       convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_NSel, group = scale_NSel)) +
#         geom_line() +
#         facet_wrap(~scale_NSel)
#       ggsave(convergence.plot, filename = paste(covname, compname, "_ConvPlot.jpeg", sep = ""),
#              path = "./NIMBLE IndComp Version/", device = "jpeg")
#       
#       scaleoutputs <- data.frame(Component = compname, Covariate = covname, ID = "beta_SC_NSel",
#                                  S1 = NSel_t[1], S2 = NSel_t[2], S3 = NSel_t[3], S4 = NSel_t[4],
#                                  TopScale = which.max(NSel_t))
#       write.table( scaleoutputs,  
#                    file="./NIMBLE IndComp Version/Scale Selection Outputs.csv", 
#                    append = T, 
#                    sep=',', 
#                    row.names=F, 
#                    col.names=F )
#     }
#     
#     waicoutputs <- data.frame(Component = compname, Covariate = covname, WAIC = NHQ.samples.MCMC$WAIC)
#     write.table( waicoutputs,  
#                  file="./NIMBLE IndComp Version/Scale Selection WAIC Outputs.csv", 
#                  append = T, 
#                  sep=',', 
#                  row.names=F, 
#                  col.names=F )
#   }
# }


for(j in 4){
  #Build Base Model
  load(file = paste("./NIMBLE IndComp Version/", covSelnames[1], compnames[j], "_NHQdata.RData", sep = ""))

  NHQ.model <- nimbleModel(code = NHQ.code,
                           constants = NHQ.constants,
                           inits = NHQ.initial,
                           data = NHQ.data)


  for(i in 1:10){ #Number of covariates
    rm(list=setdiff(ls(),
                    c("ni", 'nc', "nb", "nt", "covSelnames", "compnames", "i", "j", "NHQ.model")))
    gc()
    covname = covSelnames[i]
    compname = compnames[j]

    # load(file = paste("./NIMBLE IndComp Version/", compname,"_BaseNimbleModel.RData", sep = ""))
    load(file = paste("./NIMBLE IndComp Version/", covname, compname, "_NHQdata.RData", sep = ""))
    # load(file = paste("./NIMBLE IndComp Version/", compname, "_BaseNimbleModel.RData", sep = ""))

    NHQ.model$setData(NHQ.data)
    NHQ.comp.model <- compileNimble(NHQ.model)
    NHQ.conf.mcmc <- configureMCMC(model = NHQ.comp.model,
                                   monitors = NHQ.monitor,
                                   enableWAIC = T)
    NHQ.MCMC <- buildMCMC(NHQ.conf.mcmc)
    NHQ.comp.MCMC <- compileNimble(NHQ.MCMC)
    rm(list=setdiff(ls(),
                    c("NHQ.comp.MCMC", "NHQ.model",
                      "covname", "compname", "ni", 'nc', "nb", "nt", 'covSelnames', "compnames", 'i', 'j')))
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
         file = paste("./NIMBLE IndComp Version/",covname, compname, "_MCMC_.RData", sep = ""))

    ### Nest Success ###
    if(j == 4){
      convergence.check <- as.data.frame(NHQ.samples.MCMC$samples)

      HDSR_t <- table(convergence.check$scale_HDSR)
      NDSR_t <- table(convergence.check$scale_NDSR)

      bestscales1 <- convergence.check %>%
        group_by(scale_HDSR, scale_NDSR) %>%
        summarize(Total = n()) %>%
        pivot_wider(id_cols = scale_HDSR, values_from = Total,
                    names_from = scale_NDSR)

      bestscales <- convergence.check %>%
        group_by(scale_NDSR, scale_HDSR) %>%
        mutate(Order = row_number())


      convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_HDSR)) +
        geom_line() +
        facet_wrap(~scale_NDSR + scale_HDSR)
      ggsave(convergence.plot, filename = paste(covname, "HDSR" ,"_ConvPlot.jpeg", sep = ""),
             path = "./NIMBLE IndComp Version/", device = "jpeg")
      convergence.plot <- ggplot(data = bestscales, aes(x = Order, y = beta_SC_NDSR)) +
        geom_line() +
        facet_wrap(~scale_NDSR + scale_HDSR)
      ggsave(convergence.plot, filename = paste(covname, "NDSR" ,"_ConvPlot.jpeg", sep = ""),
             path = "./NIMBLE IndComp Version/", device = "jpeg")



      scaleoutputs <- data.frame(Component = rep(compname,4), Covariate = rep(covname,4), ID = rep("HDSR(rows) by NDSR(columns)", 4),
                                 S1 = c(bestscales1[,2]), S2 = bestscales1[,3], S3 = bestscales1[,4], S4 = bestscales1[,5],
                                 TopScale = NA)
      write.table( scaleoutputs,
                   file="./NIMBLE IndComp Version/Scale Selection Outputs.csv",
                   append = T,
                   sep=',',
                   row.names=F,
                   col.names=F )
    }

    waicoutputs <- data.frame(Component = compname, Covariate = covname, WAIC = NHQ.samples.MCMC$WAIC)
    write.table( waicoutputs,
                 file="./NIMBLE IndComp Version/Scale Selection WAIC Outputs.csv",
                 append = T,
                 sep=',',
                 row.names=F,
                 col.names=F )
  }
}