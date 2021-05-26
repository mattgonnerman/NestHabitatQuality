function(){##############################################################################################

  ### Prelaying Habitat Selection
  for(i in 1:NInd_PLSel){
    alpha_PLSel[i] ~ dbeta(1,1)
    intercept_PLSel[i] <- logit(alpha_PLSel[i])
  }

  beta_SC_PLSel ~ dnorm(0, 0.01/wtPL)
  scale_PLSel ~ dcat(w_PLSel[1:4])
  w_PLSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  for(n in 1:N_PLSel){
    logit(piPL[n]) <- intercept_PLSel[Ind_PLSel[n]] + beta_SC_PLSel*cov_PLSel[n,scale_PLSel]
    yPL[n] ~ dbern(piPL[n])
  }

  ### Laying Habitat Selection
  for(i in 1:NInd_LSel){
    alpha_LSel[i] ~ dbeta(1,1)
    intercept_LSel[i] <- logit(alpha_LSel[i])
  }

  beta_SC_LSel ~ dnorm(0, 0.01/wtL)
  scale_LSel ~ dcat(w_LSel[1:4])
  w_LSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  for(n in 1:N_LSel){
    logit(piL[n]) <- intercept_LSel[Ind_LSel[n]] + beta_SC_LSel*cov_LSel[n,scale_LSel]
    yL[n] ~ dbern(piL[n])
  }

  ### Nesting Habitat Selection

  alpha_NSel ~ dbeta(1,1)
  intercept_NSel <- logit(alpha_NSel)

  beta_SC_NSel ~ dnorm(0, 0.01/wtN)
  scale_NSel ~ dcat(w_NSel[1:4])
  w_NSel[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))


  for(n in 1:N_NSel){
    logit(piN[n]) <- intercept_NSel + beta_SC_NSel*cov_NSel[n,scale_NSel]
    yN[n] ~ dbern(piN[n])
  }
  
  ### Nest Daily Mortality Risk ###
  #Use BLISS for habitat covariates
  alpha_NDMR ~ dbeta(1,1)
  intercept_NDMR <- cloglog(alpha_NDMR)
  beta_A_NDMR ~ dnorm(0,.01) #Effect of age on DSR (1 = Adult, 0 = Juv)
  beta_SC_NDMR ~ dnorm(0, 0.01)
  scale_NDMR ~ dcat(w_NDMR[1:4])
  w_NDMR[1:4] ~ ddirch(c(0.25,0.25,0.25,0.25))

  for(n in 1:NDMR_nvisit){
    NDMR_eta[n] <- intercept_NDMR + beta_A_NDMR*NDMR_age[i] + beta_SC_NDMR*cov_NDMR[NDMR_ID[n],scale_NDMR]
    cloglog(NDMR_phi[n]) <- NDMR_eta[n]  #anti-logit to determine the daily survival rate
    NDMR_mu[n] <- exp(-(NDMR_interval[n]*NDMR_phi[n]))  #period survival is DSR raised to the interval
    NDMR_succ[n] ~ dbern(NDMR_mu[n])  #the data is distributed as bernoulli with period survival as the mean
  }

  ### Habitat Quality ###
  # P(Selection) x P(Nest Success) x P(Hen Survival) = Quality
  for(i in 1:nNHQ){
    PLS[i] <- exp(beta_SC_PLSel*cov_NHQ[i,scale_PLSel])
    LS[i] <- exp(beta_SC_LSel*cov_NHQ[i,scale_LSel])
    NS[i] <- exp(beta_SC_NSel*cov_NHQ[i,scale_NSel])
    cloglog(NDMR[i]) <- mean(intercept_NDMR) + beta_SC_NDMR*cov_NHQ[i,scale_NDMR]
    # cloglog(HDMR[i]) <- mean(intercept_HDMR) + beta_SC_HDMR*cov_NHQ[i,scale_HDMR]
    NHQ[i] <- PLS[i]/max(PLS) * LS[i]/max(LS) * NS[i]/max(NS) * NDMR[i]/max(NDMR)
  }
   
  
}