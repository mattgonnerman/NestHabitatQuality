require(nimble)

### Data Preparation (Singular Components) ###
source(file = "NIMBLE IndComp Version/NestHabitatQuality - 1 IndComp NIMBLE Data Prep.R")


### Base Model Creation ###
# May be necessary to restart R to get completely clear RAM
# source(file = "NIMBLE IndComp Version/NestHabitatQuality - 1a Base Model Creation.R")

### Perform BLISS Scale Selection ###
source(file = "NIMBLE IndComp Version/NestHabitatQuality - 3 Scale Selection.R")

### Model Selection ###
#https://docs.pymc.io/notebooks/model_averaging.html
waicscores <- read.csv("Scale Selection WAIC Outputs.csv") %>%
  group_by(Component) %>%
  mutate(DeltaWAIC = (exp(-.5*WAIC))) %>%
  mutate(Weight = DeltaWAIC/sum(DeltaWAIC)) %>%
  arrange(Component, WAIC) %>%
  mutate(CumulWeight = cumsum(Weight)) %>%
  ungroup() 
waicscores2 <- read.csv("Scale Selection WAIC Outputs.csv") %>%
  group_by(Component) %>%
  mutate(WAIC = WAIC/1000) %>%
  mutate(DeltaWAIC = (exp(-.5*WAIC))) %>%
  mutate(Weight = DeltaWAIC/sum(DeltaWAIC)) %>%
  arrange(Component, WAIC) %>%
  mutate(CumulWeight = cumsum(Weight)) %>%
  ungroup() 
waicscores3 <- read.csv("Scale Selection WAIC Outputs.csv")

for(i in 1:10){
  waicscores3$Weights[i] <- exp(-.5*waicscores3$WAIC[i])/sum(exp(-.5*waicscores3$WAIC[1:10]))
}

### Final Model Run ###


### Create NHQ Value Rasters

