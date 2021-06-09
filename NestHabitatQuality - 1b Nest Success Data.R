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


### Hen Mortality

#Get mortality dates for relevant birds
telem.raw <- read.csv("Telemetry_Data - Telemetry.csv")

death.dates <- telem.raw %>% dplyr::select(BirdID = AlumBand, Date, Fate) %>% 
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(Year = year(Date)) %>%
  filter(Fate == "D" ) %>%
  group_by(BirdID) %>%
  arrange(Date) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(NestID = paste(BirdID, Year, 1, sep = "-")) %>%
  filter(NestID %in% nest.info$NestID)  %>%
  mutate(HenMort = 1)
  
#Get First off nest
monitoring.firstoff <- monitoring.clean %>% 
  filter(Status == 0) %>%
  group_by(NestID) %>%
  arrange(Date) %>%
  slice(1L)

killed.on.nest <- merge(monitoring.firstoff, death.dates, by = c("NestID", "Date")) %>%
  arrange(NestID, Date)

### Nest Abandoned post Flushing
#Flush Count Date
flush.date <- nestfate.raw %>%
  dplyr::select(NestID, Date = Date.Counted) %>%
  filter(!is.na(Date) & Date != "") %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(Flushed = 1)

# Last Day On Nest
monitoring.lastdayon <- monitoring.clean %>% 
  filter(Status == 1) %>%
  group_by(NestID) %>%
  arrange(desc(Date)) %>%
  slice(1L)

flushed.aband <- merge(monitoring.lastdayon, flush.date, by = c("NestID", "Date"))

#Check If Hens are in both flush and death lists
which(killed.on.nest$NestID %in% flushed.aband$NestID)
which(flushed.aband$NestID %in% killed.on.nest$NestID)
killed.on.nest <- killed.on.nest[-which(killed.on.nest$NestID %in% flushed.aband$NestID),]
  
#Transform into EH format
ns_eh_matrix <- monitoring.clean %>% dcast(NestID ~ RefDate, value.var = "Status")

ns_eh_failnomort <- ns_eh_matrix[2:ncol(ns_eh_matrix)]

#Flip Success history in prep for making failure type histories
ns_eh_failnomort <- ifelse(ns_eh_failnomort == 1, 0, ifelse(ns_eh_failnomort == 0, 1, NA))
ns_eh_failmort <- ifelse(ns_eh_failnomort == 1, 0, ifelse(ns_eh_failnomort == 0, 0, NA))

row.mort <- which(ns_eh_matrix$NestID %in% killed.on.nest$NestID)
row.flush <- which(ns_eh_matrix$NestID %in% flushed.aband$NestID)

for(i in 1:nrow(ns_eh_failnomort)){
  for(j in 1:ncol(ns_eh_failnomort)){

    ns_eh_failmort[i,j] <- ifelse(i %in% row.mort & ns_eh_failnomort[i,j] == 1, 1,
                                  ifelse(i %in% row.flush & ns_eh_failnomort[i,j] == 1, 0, ns_eh_failmort[i,j]))
    ns_eh_matrix[i,j+1] <- ifelse(i %in% row.flush & ns_eh_matrix[i,j+1] == 0, NA, ns_eh_matrix[i,j+1])
    ns_eh_failnomort[i,j] <- ifelse(i %in% row.mort & ns_eh_failnomort[i,j] == 1, 0,
                                    ifelse(i %in% row.flush & ns_eh_failnomort[i,j] == 1, 0, ns_eh_failnomort[i,j]))
    
  } #j
} #i



########################
### Prep EH For JAGS ###
########################
### Vectorize encounter histories ###
# Translate visitation history to exposure length history
ns_eh_matrix_edit <- ns_eh_matrix[,-c(1)]
ns_eh_failmort[is.na(ns_eh_matrix_edit)] <- NA
ns_eh_failnomort[is.na(ns_eh_matrix_edit)] <- NA
ns_n_ind <- nrow(ns_eh_matrix)
ns_exposure <- ncol(ns_eh_matrix)-1

#Vectorize encounter histories
ns_succ1 <- as.vector(t(ns_eh_matrix_edit))
ns_succ <- ns_succ1[!is.na(ns_succ1)]

ns_failnomort1 <- as.vector(t(ns_eh_failnomort))
ns_failnomort <- ns_failnomort1[!is.na(ns_failnomort1)]

ns_failmort1 <- as.vector(t(ns_eh_failmort))
ns_failmort <- ns_failmort1[!is.na(ns_failmort1)]

# ns_failflush1 <- as.vector(t(ns_eh_failflush))
# ns_failflush <- ns_failflush1[!is.na(ns_failflush1)]

#Format as a matrix for JAGS model
# ns_succ.mat <- matrix(c(ns_succ, ns_failnomort, ns_failmort, ns_failflush), ncol = 4, nrow = length(ns_failnomort), byrow = F)
ns_succ.mat <- matrix(c(ns_succ, ns_failnomort, ns_failmort), ncol = 3, nrow = length(ns_failnomort), byrow = F)

#Double check that all rows sum to 1 and the columns sum = length of number in each group
rowSums(ns_succ.mat)
colSums(ns_succ.mat)

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
