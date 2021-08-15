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

### Final Model Run ###


### Create NHQ Value Rasters

