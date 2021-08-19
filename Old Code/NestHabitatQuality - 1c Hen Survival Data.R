### Load Necessary Packages
require(dplyr)
require(lubridate)
require(reshape2)

### Load relevant databases
trap.raw <- read.csv("Trapping - Data.csv")
telem.raw <- read.csv("Telemetry_Data - Telemetry.csv")


### Clean up databases for EH construction
## Get relevant bird information
# Subset to relevant time period
# Remove males
# Hen X Year ID
bird.info <- trap.raw %>%
  dplyr::select(BirdID = AlumBand, BirdID2 = Rivet.Band, Age, Sex, Trans.Type, Trap.Date = Date) %>%
  filter(Sex == "F") %>%
  filter(!is.na(Trans.Type) & Trans.Type != "") %>%
  mutate(Trap.Date = as.POSIXct(Trap.Date, format = "%m/%d/%Y"),
         Trap.Year = year(Trap.Date))

## Get relevant telemetry data
# Group by HenID X Year
telemetry_reduced <- telem.raw %>% dplyr::select(BirdID = AlumBand, Date, Fate) %>% 
  filter(BirdID %in% unique(bird.info$BirdID)) %>%
  filter(!is.na(Fate) & Fate != "" & Fate != "?" & Fate != "MIA") %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  filter(month(Date) %in% 3:7) %>%
  mutate(Year = year(Date)) %>%
  mutate(EH_ID = paste(BirdID, Year, sep = "_"))

telem_trap_comb <- merge(telemetry_reduced, bird.info, by = "BirdID", all.x = T) %>% 
  arrange(Year, BirdID, Date) %>%
  dplyr::select(-BirdID2, -Sex)

# Subset to relevant time period
telem_trap_comb



### Set each date in reference to when the bird transitioned to prenesting


###################################################################
###PLACE HOLDER DATE FOR WHEN YOU GET THE CORRECT DATES FROM HMM###
###################################################################
trans.dates <- data.frame(EH_ID = unique(telem_trap_comb$EH_ID),
                          TransDate = c(rep(as.POSIXct("2018-03-15"),50), rep(as.POSIXct("2019-03-15"),103), rep(as.POSIXct("2020-03-15"),79)))
###################################################################
###################################################################

 
telemetry_eh <- merge(telem_trap_comb, trans.dates, by = "EH_ID", all.x = T) %>%
  mutate(RefDate = as.integer(difftime(Date, TransDate, unit = "days")+1))

trans.dates1 <- trans.dates %>%
  mutate(BirdID = sub("\\_.*", "", EH_ID)) %>%
  mutate(Year = sub(".*\\_", "", EH_ID)) %>%
  mutate(Fate = "L") %>%
  mutate(Date = TransDate) %>%
  mutate(RefDate = 1)

trans.dates2 <- merge(trans.dates1, bird.info %>% dplyr::select(BirdID, Age, Trans.Type, Trap.Date, Trap.Year), all.x = T) 

telemetry_eh1 <- rbind(telemetry_eh, trans.dates2) %>% 
  arrange(Year, BirdID, Date) %>%
  filter(RefDate > 0) %>%
  mutate(Fate = as.numeric(ifelse(Fate == "L", 1, 0)),
         Year = as.factor(Year), 
         Age = ifelse(Trap.Year == Year, Age, "A"),
         Age = ifelse(Age == "A", 1, 0)) %>%
  distinct()

#Need to make sure you only have one D for each bird. 
#This returns the number of D for each bird in database
#Manually changing database to assess each situation
#Usually left first D as the known D and moved the subsequent Fates to Notes
checkD <- telemetry_eh1 %>% 
  filter(Fate == 0) %>% 
  group_by(BirdID) %>% 
  dplyr::summarize(Total = n()) %>% 
  filter( Total > 1)

#Check for occassions where there is a death recorded but bird was later alive
#Check and change manually in database
deathdate <- telemetry_eh1 %>%
  filter(Fate == 0) %>%
  dplyr::select(BirdID, DeathDate = Date)
alivedate <- telemetry_eh1 %>%
  filter(Fate == 1) %>%
  group_by(BirdID) %>%
  top_n(1, Date) %>%
  ungroup() %>%
  dplyr::select(BirdID, LastAlive = Date)
comparedates <- merge(deathdate, alivedate, all.x = T, by = "BirdID") %>%
  filter(LastAlive > DeathDate)
# comparedates

#Transform into EH format
dailysurvival_eh <- telemetry_eh1 %>% dcast(EH_ID ~ RefDate, value.var = "Fate")

### Final EH format ###
dailysurvival_eh_nocap <- dailysurvival_eh[,-c(1)]
dsr_eh_matrix <- data.matrix(dailysurvival_eh_nocap)


########################
### Prep EH For JAGS ###
########################
### Vectorize encounter histories ###
# Translate visitation history to exposure length history
dsr_eh_matrix_edit <- dsr_eh_matrix[,-c(1)]
dsr_n_ind <- nrow(dsr_eh_matrix)
dsr_exposure <- ncol(dsr_eh_matrix)-1

#Vectorize encounter histories
dsr_succ1 <- as.vector(t(dsr_eh_matrix_edit))
dsr_succ <- dsr_succ1[!is.na(dsr_succ1)]
dsr_ID <- matrix(1:dsr_n_ind, nrow = dsr_n_ind, ncol = dsr_exposure)
dsr_ID <- as.vector(t(dsr_ID))
dsr_ID <- dsr_ID[!is.na(dsr_succ1)]      # ID marker

# Interval length between visits
dsr_visit_ind <- matrix(NA, ncol = ncol(dsr_eh_matrix), nrow = nrow(dsr_eh_matrix))
get.last <- function(x) max(which(!is.na(x)))

for(i in 1:dsr_n_ind){
  for(j in 2:(dsr_exposure+1)){
    if(is.na(dsr_eh_matrix[i,j]) == FALSE){
      dsr_visit_ind[i,j] <- j - get.last(dsr_eh_matrix[i,1:(j-1)])
    }
  }
}

dsr_interval <- as.vector(t(dsr_visit_ind))
dsr_interval <- dsr_interval[!is.na(dsr_interval)]


######################################
### Individual Specific Covariates ###
######################################
DSR_Age <- dailysurvival_eh
telemetry_age <- telemetry_eh1 %>%
  dplyr::select(EH_ID, Age) %>%
  distinct()
DSR_Age1 <- merge(DSR_Age, telemetry_age, by = "EH_ID")


for(i in 1:nrow(DSR_Age1)){
  for(j in 2:(ncol(DSR_Age1)-1)){
    DSR_Age1[i,j] <- ifelse(!is.na(DSR_Age1[i,j]), DSR_Age1[i,2], NA)
  }
}
DSR_Age2 <- DSR_Age1 %>% dplyr::select(-EH_ID, -Age, -1)
dsr_adult1 <- as.vector(t(DSR_Age2))
dsr_adult <- dsr_adult1[!is.na(dsr_adult1)]
