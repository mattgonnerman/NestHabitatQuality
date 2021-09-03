### Load Necessary Packages
lapply(c('dplyr', 'lubridate', 'reshape2'), require, character.only = T)

### Load relevant databases
trap.raw <- read.csv("Trapping - Data.csv")
nestfate.raw <- read.csv("Nest Monitoring - Nest Info.csv") %>%
  mutate(Alum.Band.ID = as.character(Alum.Band.ID)) %>%
  filter(!is.na(NestLat))
nestmonitor.raw <- read.csv("Nest Monitoring - Nest Status Checks.csv") %>%
  filter(NestID %in% unique(nestfate.raw$NestID))

### Number Nests Lost Prior to Flushing
vhf.nests <- trap.raw %>%
  filter(Trans.Type %in% c("VHF_Neck", "VHF_Back"))

no.flush.vhf <- nestfate.raw %>%
  filter(Alum.Band.ID %in% vhf.nests$AlumBand) %>%
  filter(Nest.Attempt == 1) %>%
  filter(is.na(Date.Counted))

nrow(no.flush.vhf)

