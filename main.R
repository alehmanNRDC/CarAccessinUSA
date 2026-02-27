# Calculations and data for the GA dashboard map data 
# By ALehman
# Published Feb 2026

rm(list = ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets working directory to active document

# Package upload ---- 
packages <- c(
  'quarto',
  'sf',
  'tidyverse',
  'httr',
  'CDCPLACES',
  'dplyr',
  'rjson',  
  'jsonlite',
  'tigris',
  'tidycensus',
  'data.table',
  'viridis',
  'readxl',
  'fs',
  'here',
  'osmdata',
  'sfnetwork',
  'ggplot2',
  "readr", 
  "osmdata",
  'dodgr'
)

if(!require(pacman)) install.packages('pacman')
pacman::p_load(packages,character.only = T)

# set the census state number
statenumber = 13 

# --- process data ----
# the following scripts, all of which live in this folder, prepare the data inputs 

source("census-inputs.R")
source("NHTS-inputs.R")
source("PLACES-input.R")


###  Binomial probability to determine age of household members----
# we know the household size and vehicle count but we don't know who is a child or driving age in the household. i use binomial probabilities to determine this 

household_types <- c("P2V0","P2V1", "P2V2","P2V3","P2V4",
                     "P3V0","P3V1", "P3V2","P3V3","P3V4",
                     "P4V0","P4V1", "P4V2","P4V3","P4V4")


# Loop through each household type
for (ht in household_types) {
  n_people <- as.numeric(substr(ht, 2, 2))# e.g. 3 from P3V2
  n_nonhh <- n_people - 1       # subtract householder, who is definetly an adult or of driving age 
  
  # For each possible number of kids (0 to n_nonhh), calculate binomial probability
  for (k in 0:n_nonhh) {
    prob_expr <- paste0("dbinom(", k, ", ", n_nonhh, ", pct_kid_nonhh)")
    col_name <- paste0(ht, "_", k, "kids")
    workingDF <- workingDF %>%
      mutate(!!col_name := !!rlang::parse_expr(paste0(ht, " * ", prob_expr)))
  }
}



## i. Degrees of access -----
# examining at households based on their ratio of adults to cars and people to cars 
workingDF <- workingDF %>%
  mutate(
    oneMoreAdults = 
      # 2 adults, 1 vehicle
      2 * P2V1_0kids +
      2 * P3V1_1kids +
      2 * P4V1_2kids+
      # 3 adults, 2 vehicles
      3 * P3V2_0kids+
      3 * P4V2_1kids+
      #4 adults, 3 vehicles 
      4 * P4V3_0kids,  
    
    twoMorePlusAdults =
      # 3 adults, 1 vehicle → 2 more adults than cars
      3 * P3V1_0kids+
      3 * P4V1_1kids+
      # 4 adults, 2 vehicles
      4* P4V2_0kids, 
    
    carSafeAdults = 
      # single households with car
      P1V1 +P1V2+P1V3+P1V4+
      
      # 1 adult, 1 vehicle
      1 * P2V1_1kids +
      1 * P3V1_2kids +
      1 * P4V1_3kids + 
      # 1 adult, 2 vehicle
      1 * P2V2_1kids +
      1 * P3V2_2kids +
      1 * P4V2_3kids +
      # 1 adult, 3 vehicle
      1 * P2V3_1kids +
      1 * P3V3_2kids +
      1 * P4V3_3kids + 
      # 1 adult, 1 vehicle
      1 * P2V4_1kids +
      1 * P3V4_2kids +
      1 * P4V4_3kids + 
      
      # 2 adults, 2 cars
      2 * P3V2_1kids +
      2 * P2V2_0kids + 
      2 * P4V2_2kids+
      # 2 adults, 3 cars 
      2* P2V3_0kids +
      2* P3V3_1kids +
      2* P4V3_1kids+
      # 2 adults, 4 cars 
      2* P2V4_0kids +
      2* P3V4_1kids +
      2* P4V4_1kids+
      
      # 3 adults, 3 cars
      3* P3V3_0kids +
      3* P4V3_1kids +
      #3 adults, 4 cars
      3* P3V4_0kids+
      3* P4V3_1kids+
      
      # 4 adults, 4 cars 
      4* P4V4_0kids, 
    
    zeroCarAdults=
      P1V0+
      2* P2V0_0kids+
      1* P2V0_1kids+
      
      3*P3V0_0kids+
      2*P3V0_1kids+
      1*P3V0_2kids+
      
      4*P4V0_0kids+
      3*P4V0_1kids+
      2*P4V0_2kids+
      1*P4V0_3kids,
    zeroHH=
      P1V0+P2V0+P3V0+P4V0,
    allZeroCar=
      4*P4V0+
      3*P3V0+
      2*P2V0+
      P1V0)


## ii. Rural area flag -----
ruralCensus2020 <- ruralCensus2020 %>%
  mutate(rural_flag = ifelse(pRural > 0.5, 1, 0)) %>%
  select(GEOID, rural_flag)

# Join into workingDF and the rural flag to compute rural car access
workingDF<- merge(workingDF,ruralCensus2020, by="GEOID", all.x=TRUE)%>%
  # use the rural flag to calculate rural nonDrivers
  mutate(RURALallZeroCar=allZeroCar*rural_flag)%>%
  mutate(RURALzeroCarHH=zeroHH*rural_flag)

rm(ruralCensus2020)

## iii. people with disabilities that are in cared household mostly drive-----
# count those with a "medical condition" that make a majority of their trips by car, have a car, and are 18-75
qualified_ids <- peopletrip %>%
  filter(MEDCOND == "1" &
           peopletrip$R_AGE.x > 18 &
           peopletrip$R_AGE.x< 75  & 
           peopletrip$HHVEHCNT.x>0)%>%                   
  group_by(ID) %>%
  summarise(
    total_trips = n(),
    qualifying_trips = sum(TRPTRANS %in% c(1, 2, 3, 4, 6, 7) & WHODROVE == 1)
  ) %>%
  filter(qualifying_trips > total_trips / 2) %>%  # Keep if majority of trips qualify
  pull(ID)


total_disability_majorityDriving <- peopletrip %>%
  filter(ID %in% qualified_ids) %>%
  distinct(ID, .keep_all = TRUE) %>%  # Ensure one row per person
  summarise(total_weight = sum(WTPERFIN, na.rm = TRUE)) %>%
  pull(total_weight)

# medical condition and over 18 
TLD=sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$MEDCOND == "1"&
    NHTS_personReport$R_AGE > 18], 
  na.rm = TRUE)

# the percent of people with disabilities that consistently drive and have access to a car, we will use the inverse of this 
pdrivingdis<-total_disability_majorityDriving/TLD


qualified_ids_dash <- peopletrip %>%
  filter(MEDCOND == "1" &
           peopletrip$R_AGE.x > 18 )%>%      # now i dont care if they are  75 or in a cared hh             
  group_by(ID) %>%
  summarise(
    total_trips = n(),
    qualifying_trips = sum(TRPTRANS %in% c(1, 2, 3, 4, 6, 7) & WHODROVE == 1)
  ) %>%
  filter(qualifying_trips > total_trips / 2) %>%  # Keep if majority of trips qualify
  pull(ID)


total_disability_majorityDriving <- peopletrip %>%
  filter(ID %in% qualified_ids_dash) %>%
  distinct(ID, .keep_all = TRUE) %>%  # Ensure one row per person
  summarise(total_weight = sum(WTPERFIN, na.rm = TRUE)) %>%
  pull(total_weight)


# the percent of people with disabilities that consistently drive and have access to a car, we will use the inverse of this 
# this is the data point in the dashboard 
1-total_disability_majorityDriving/TLD


# remove cognitive disabilities from the total, an explanation can be found in the methodology 
PLACES_census <- PLACES_census %>%
  mutate(
    disabilities= (`Any disability among adults` - `Cognitive disability among adults`)/100,
    disability_constraints = (1 - pdrivingdis) * disabilities 
  )%>%
  select(1,5)

## iv. People 75+ and driving status  ----
drivers_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$R_AGE >= 75], 
  na.rm = TRUE)
all_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$R_AGE >= 75], 
  na.rm = TRUE)


pDrivingover75 = drivers_over75 /all_over75 

workingDF <- workingDF%>%
  mutate(elderlyDrivers=pDrivingover75 * over75,
       elderlyNotDriving= (1-pDrivingover75)* over75)


# driving limitation
drivers_over75_night = 
  sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$CONDNIGH == 1 &
    NHTS_personReport$R_AGE >= 75], 
  na.rm = TRUE)

# no disability, HH car and elderly constrained 
driverandowner_over75_night = 
  sum(NHTS_personReport$WTPERFIN[
    NHTS_personReport$DRIVER == "1" & 
      NHTS_personReport$MEDCOND != "1" &
      NHTS_personReport$CONDNIGH == 1 &
      NHTS_personReport$R_AGE >= 75 & 
      NHTS_personReport$HHVEHCNT >= 1], 
    na.rm = TRUE)
pover75constrained=driverandowner_over75_night/all_over75
pover75constrained



#### Add geometries ####
# use the api to pull a shapefile of census tracts
tractGEO <- get_decennial(
  geography = "tract",
  variables = "P1_001N",   # dummy variable
  year = 2020,
  sumfile = "pl",
  geometry = TRUE,
  keep_geo_vars = TRUE,
  state = statenumber 
) 


# select workingDF1 fields
workingDF1<-workingDF%>%
  select(-c(4:29,31:46,51:53,56:101,108,110))%>%
  mutate(across(where(is.numeric), round))%>%
  mutate(COUNTY_NAME = str_replace(NAME,pattern = ".*;([^-]*);.*", replacement = "\\1"))%>%
  merge(PLACES_census, by.x="GEOID" , by.y="locationname" )


rm(workingDF)  


workingDF1$NAME<-gsub(".*;","",workingDF1$NAME)
workingDF1$NAME <- sub(" ", "", workingDF1$NAME)

# add the census shapefile 
geofile <- tractGEO %>%
  select(GEOID, geometry)%>%
  right_join(workingDF1, by = "GEOID")


# add the H&T data 
source("chaufferingandcosts.R")
export<- merge(geofile,HandTdata1, by.x="GEOID", by.y="tract")


# topline total ----
export<-export  %>%
  mutate(PzeroCarAdults=100*zeroCarAdults/TotalPopulationover10)%>%
  mutate(P10to15=100*young10to15/TotalPopulationover10)%>%
  mutate(PcarConstrainedAdults=100*twoMorePlusAdults/TotalPopulationover10)%>%
  mutate(Pover75Constrained=100*over75*pover75constrained/TotalPopulationover10)%>%
  mutate(Pdisability=100*disability_constraints/TotalPopulationover10)%>%
  mutate(Ptotal=P10to15+PcarConstrainedAdults+PzeroCarAdults+Pover75Constrained+Pdisability)

#write_sf(export1,r"(C:\Users\alehman\Downloads\GA6.geojson)")
write_sf(export,here("outputs/GAbasecalc.geojson)"))

