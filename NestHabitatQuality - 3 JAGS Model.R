function(){##############################################################################################

  ### Prelaying Habitat Selection
  for(i in 1:NInd_PLSel){
    alpha_PLSel[i] ~ dbeta(1,1)
    intercept_PLSel[i] <- logit(alpha_PLSel[i])
  }
  
  beta_PLSel ~ dnorm(0, 0.01)
  scale_PLSel ~ dcat(c(0.25, 0.25, 0.25, 0.25))
  
  for(n in 1:N_PLSel){
    logit(piPL[n]) <- intercept_PLSel[Ind_PLSel[n]] + beta_PLSel*cov_PLSel[n,scale_PLSel]
    yPL[n] ~ dbern(piPL[n])
  }
  
  ### Laying Habitat Selection
  for(i in 1:NInd_LSel){
    alpha_LSel[i] ~ dbeta(1,1)
    intercept_LSel[i] <- logit(alpha_LSel[i])
  }
  
  beta_LSel ~ dnorm(0, 0.01)
  scale_LSel ~ dcat(c(0.25, 0.25, 0.25, 0.25))
  
  for(n in 1:N_LSel){
    logit(piL[n]) <- intercept_LSel[Ind_LSel[n]] + beta_LSel*cov_LSel[n,scale_LSel]
    yL[n] ~ dbern(piL[n])
  }
  
  ### Nesting Habitat Selection
  for(i in 1:NInd_NSel){
    alpha_NSel[i] ~ dbeta(1,1)
    intercept_NSel[i] <- logit(alpha_NSel[i])
  }
  
  beta_NSel ~ dnorm(0, 0.01)
  scale_NSel ~ dcat(c(0.25, 0.25, 0.25, 0.25))
  
  for(n in 1:N_NSel){
    logit(piN[n]) <- intercept_NSel[Ind_NSel[n]] + beta_NSel*cov_NSel[n,scale_NSel]
    yN[n] ~ dbern(piN[n])
  }
  
  # #3rd Order Selection
  # alpha_RSF3 ~ dbeta(1,1)
  # intercept_RSF3 <- logit(alpha_RSF3)
  # 
  # for(n in 1:NInd_RSF){
  #   for(j in 1:NLoc_RSF3){
  #     logit(pi3[n,j]) <- intercept_RSF3 + beta_X_RSF3*RSF3_X[n,j]
  #     y3[n,j] ~ dbern(pi3[n,j])
  #   }
  # }
  # 
  # 
  # #4th Order Selection
  # alpha_RSF4 ~ dbeta(1,1)
  # intercept_RSF4 <- logit(alpha_RSF4)
  # 
  # for(n in 1:NInd_RSF){
  #   for(j in 1:NLoc_RSF4){
  #     logit(pi4[n,j]) <- intercept_RSF4 + beta_X_RSF4*RSF4_X[n,j]
  #     y4[n,j] ~ dbern(pi4[n,j])
  #   }
  # }
  # 
  # ### Nest Daily Survival Rate ###
  # #Use BLISS for habitat covariates
  # alpha_NDSR ~ dbeta(1,1)
  # intercept_NDSR <- cloglog(alpha_NDSR)
  # beta_A_NDSR ~ dnorm(0,.01) #Effect of age on NDSR (1 = Adult, 0 = Juv)
  # 
  # for(i in 1:NDSR_nvisit){
  #   NDSR_eta[i] <- intercept_NDSR + beta_A_NDSR*NDSR_age[i] 
  #   cloglog(NDSR_phi[i]) <- NDSR_eta[i]  #anti-logit to determine the daily survival rate
  #   NDSR_mu[i] <- exp(-(NDSR_interval[i]*NDSR_phi[i]))  #period survival is DSR raised to the interval
  #   NDSR_succ[i] ~ dbern(NDSR_mu[i])  #the data is distributed as bernoulli with period survival as the mean
  # }
  # 
  # ### Daily Hen Mortality Risk ###
  # #Use BLISS for habitat covariates
  # alpha_DHM ~ dbeta(1,1)
  # intercept_DHM <- cloglog(alpha_DHM)
  # beta_A_DHM ~ dnorm(0,.01) #Effect of age on DSR (1 = Adult, 0 = Juv)
  # 
  # for(i in 1:DHM_nvisit){
  #   DHM_eta[i] <- intercept_DHM + beta_A_DHM*DHM_age[i] 
  #   cloglog(DHM_phi[i]) <- DHM_eta[i]  #anti-logit to determine the daily survival rate
  #   DHM_mu[i] <- exp(-(DHM_interval[i]*DHM_phi[i]))  #period survival is DSR raised to the interval
  #   DHM_succ[i] ~ dbern(DHM_mu[i])  #the data is distributed as bernoulli with period survival as the mean
  # }


  ### Habitat Quality ###
  # P(Selection) x P(Nest Success) x P(Hen Survival) = Quality
  
  
}