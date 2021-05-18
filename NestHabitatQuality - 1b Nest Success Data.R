### Load Necessary Packages
lapply(c('dplyr', 'lubridate', 'reshape2'), require, character.only = T)

### Load relevant databases
trap.raw <- read.csv("Trapping - Data.csv")
nestfate.raw <- read.csv("Nest Monitoring - Nest Info.csv") %>%
  mutate(Alum.Band.ID = as.character(Alum.Band.ID)) %>%
  filter(!is.na(NestLat))
nestmonitor.raw <- read.csv("Nest Monitoring - Nest Status Checks.csv") %>%
  filter(NestID %in% unique(nestfate.raw$NestID))


### Clean up databases for EH construction
## Get relevant bird information
# Subset to relevant time period
# Remove males
bird.info <- trap.raw %>%
  dplyr::select(BirdID = AlumBand, BirdID2 = Rivet.Band, Age, Sex, Trans.Type, Trap.Date = Date) %>%
  filter(Sex == "F") %>%
  filter(!is.na(Trans.Type) & Trans.Type != "") %>%
  mutate(Trap.Date = as.POSIXct(Trap.Date, format = "%m/%d/%Y"),
         Trap.Year = year(Trap.Date))

##Get relevant nest monitoring data
nest.info <- nestfate.raw %>%
  dplyr::select(NestID, BirdID = Alum.Band.ID, Nest.Attempt, Nest.Year = Year,
                NestLat, NestLong, Est.Laying.Init = Est.Laying.Initiation, Est.Hatch = EstimatedHatch, Fate) %>%
  filter(!is.na(NestLat))

## Get relevant nest monitoring data
# Group by NestID
monitoring.info <- nestmonitor.raw %>% 
  dplyr::select(NestID, Date, Status) %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  filter(Status == 1 | Status == 0) %>%
  mutate(Status = ifelse(Status == 0, 1, 0)) %>%
  mutate(Init = "N")

nestinit.info <- nest.info %>%
  dplyr::select(NestID, Date = Est.Laying.Init) %>%
  mutate(Status = 1) %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(Init = "Y")

monitoring.comp <- rbind(nestinit.info, monitoring.info) %>%
  arrange(NestID, Date)

#Identify which nests have more than one off-nest status (0)
multioff <- monitoring.comp %>%
  filter(Status == 0) %>%
  group_by(NestID) %>%
  summarize(Total = n()) %>%
  filter(Total > 1)

#Ensure Nest Initiation Is the first date
firstinit <- monitoring.comp %>%
  group_by(NestID) %>%
  slice(1L) %>%
  filter(Init == "N")

# Clean up dataframe
monitoring.clean <- monitoring.comp %>%
  dplyr::select(-Init) %>%
  distinct() %>%
  group_by(NestID) %>%
  mutate(Day1 = min(Date)) %>%
  mutate(RefDate = 1+yday(Date) - yday(Day1)) %>%
  ungroup()

#Transform into EH format
ns_eh_matrix <- monitoring.clean %>% dcast(NestID ~ RefDate, value.var = "Status")


########################
### Prep EH For JAGS ###
########################
### Vectorize encounter histories ###
# Translate visitation history to exposure length history
ns_eh_matrix_edit <- ns_eh_matrix[,-c(1)]
ns_n_ind <- nrow(ns_eh_matrix)
ns_exposure <- ncol(ns_eh_matrix)-1

#Vectorize encounter histories
ns_succ1 <- as.vector(t(ns_eh_matrix_edit))
ns_succ <- ns_succ1[!is.na(ns_succ1)]
ns_ID <- matrix(1:ns_n_ind, nrow = ns_n_ind, ncol = ns_exposure)
ns_ID <- as.vector(t(ns_ID))
ns_ID <- ns_ID[!is.na(ns_succ1)]      # ID marker

# Interval length between visits
ns_visit_ind <- matrix(NA, ncol = ncol(ns_eh_matrix), nrow = nrow(ns_eh_matrix))
get.last <- function(x) max(which(!is.na(x)))

for(i in 1:ns_n_ind){
  for(j in 2:(ns_exposure+1)){
    if(is.na(ns_eh_matrix[i,j]) == FALSE){
      ns_visit_ind[i,j] <- j - get.last(ns_eh_matrix[i,1:(j-1)])
    }
  }
}

ns_interval <- as.vector(t(ns_visit_ind))
ns_interval <- ns_interval[!is.na(ns_interval)]
