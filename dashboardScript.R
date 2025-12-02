# Calculations and data for the dashboard map data 
# By ALehman
# Published December 2025

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


# US. Census Inputs ####
## use census API to pull three types of data 
## i. Rural data  -----
# Using 2020 census rural and urban population. If 50% or more of the population in a census tract is rural, it is flagged as a rural tract 
## upload 2020 Census (Demographic and Housing Characteristics File) with rural and urban population
state_codes <- unique(fips_codes$state_code)[1:51] 

decennialCensus2020 <- get_decennial(
  geography = "tract", 
  state = state_codes,    
  geometry = FALSE,
  variables = c(
    urban = "P2_002N",
    rural = "P2_003N"
  ),
  year = 2020,
  sumfile = "dhc"
)

ruralCensus2020<- 
  pivot_wider(
    select(decennialCensus2020,NAME, GEOID, variable, value),
    names_from = 'variable',
    values_from = 'value'
  )%>%
  mutate(NAME = sub(".*,\\s*.*,\\s*", "", NAME))%>%
  select(GEOID,urban,rural)%>%
  mutate(pRural=rural/(urban+rural))


## ii. Household car ownership data  ----
vars = load_variables( 2020,'acs5') |> 
  as.data.table()

B08201 <- vars %>% 
  filter(grepl('B08201', name)) %>% 
  slice(7:30) %>%  
  pull(name) # Creates a list of all the B08201 (household members & number of vehicles) sub variables 

# add, B01003_001, total population
B08201 <- c(B08201, "B01003_001")

# rename: P= the number of people in a household and V= the number of vehicles
variable_names <- set_names(
  B08201,
  c('TP1', 'P1V0', 'P1V1', 'P1V2', 'P1V3', 'P1V4', 
    'TP2', 'P2V0', 'P2V1', 'P2V2', 'P2V3', 'P2V4', 
    'TP3', 'P3V0', 'P3V1', 'P3V2', 'P3V3', 'P3V4', 
    'TP4', 'P4V0', 'P4V1', 'P4V2', 'P4V3', 'P4V4', 
    'TotalPopulation')
)


# Loop through the states, pulling data at the census tract level from census API 
pop <- get_acs(
  geography = "tract", 
  variables = variable_names,  
  state = state_codes,         # Must specify states
  geometry = FALSE             
)

ACS2021 = 
  pivot_wider(
    select(pop,NAME, GEOID, variable, estimate),
    names_from = 'variable',
    values_from = 'estimate'
  )%>%
  mutate(NAME = sub(".*,\\s*.*,\\s*", "", NAME))


# Develop factor to account for the fact that 4+ people households might have more than 4 people 
hh_size<- ACS2021%>% 
  mutate(pop_3orless=TP1 + 2*TP2+ 3*TP3)%>% # ppl in 4 or more p hh = [average size] * [# 4+ p hh] = state pop - [# 1p hh] - 2* [# 2 p hh] - 3* [# 3 p hh] 
  mutate(pop_4andmore= TotalPopulation - pop_3orless)%>%
  mutate(avgsize4and= pop_4andmore/  TP4)%>%
  select(GEOID, avgsize4and)

ACS2021<-merge(ACS2021,hh_size, by="GEOID") #add the adjustment factor into ACS dataframe 
ACS2021$avgsize4and <- ifelse(is.na(ACS2021$avgsize4and) | 
                                ACS2021$avgsize4and < 4 | 
                                ACS2021$avgsize4and > 10 | 
                                is.infinite(ACS2021$avgsize4and), 
                              4, 
                              ACS2021$avgsize4and)  # Replace values in avgsize4and that are less than 4, greater than 10, or invalid (NA/Inf)

# Rescale population
# the reporting at the household level for ACS (the source of the car ownership data) is different than the census, so there is a slight difference in populations. 
# I use the ratio of ACS to Census total population to scale up all calculations done on household outputs
ACS2021<- ACS2021%>%
  mutate(calculatedPop=TP1+
           2*(TP2)+
           3*(TP3)+
           avgsize4and*(TP4))%>%
  filter(calculatedPop> 0)%>%
  mutate(calculatedHH=TP1+
           TP2+
           TP3+
           TP4)

ACS2021$avgsize4and <- ifelse(ACS2021$calculatedPop == 0 , 
                              0, 
                              ACS2021$avgsize4and)

ACS2021$scaleFactor<-ACS2021$TotalPopulation/ ACS2021$calculatedPop
ACS2021[, 4:27] <- ACS2021[, 4:27] * ACS2021$scaleFactor

## iii. Age data  -----
# ACS B01001, B11005_001, and B11005_002 are age variables 
# B11005_001 and B11005_002 are the number of households and number of households with kids, respectively 

age <-  #ACS reports age by gender so we have to pull age groups for "men" and "women" then consolidate
  get_acs(geography = "tract", 
          variables = 	c("B01001_003","B01001_027",
                         "B01001_004","B01001_028", 
                         "B01001_005", "B01001_029", 
                         "B01001_023", "B01001_024",
                         "B01001_047", "B01001_048",
                         "B01001_025", "B01001_049", 
                         "B01001_001", "B11005_001",
                         "B11005_002", 	
                         "B25046_001"),
          state = state_codes,
          geometry = FALSE)

ACSage =
  pivot_wider(
    select(age,GEOID, variable, estimate),
    names_from = 'variable',
    values_from = 'estimate'
  )%>% 
  rename(hh_totalcount = 15, hh_w_kids = 16, cars=17)%>%
  mutate(young10to15=B01001_005+ B01001_029)%>%
  mutate(under10=B01001_003+B01001_004+B01001_028+B01001_027)%>%
  mutate(over75=B01001_023+B01001_047 +B01001_025+ B01001_049+ B01001_024+ B01001_048)%>%mutate(pHHwKids=100*(hh_w_kids/hh_totalcount))

### Combines ACS data into one DF ----
workingDF<-merge(ACS2021,ACSage, by="GEOID")%>%
  mutate(pkid=(young10to15+under10)/B01001_001)%>%
  mutate(p10to15=young10to15/(B01001_001-under10))%>%
  mutate(TotalPopulationover10=B01001_001-under10)%>%
  mutate(adultTotal=B01001_001-under10-young10to15)%>%
  mutate(pct_kid_nonhh=((under10+young10to15))/(B01001_001-hh_totalcount))

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


# NHTS data inputs ####
# remove unneeded variables
rm(decennialCensus2020,household_types,ACSage,age,vars,pop,ACS2021, hh_size, B08201, state_codes,variable_names)
# NHTS doesn't have an api so I use a function and links to the zip files to download locally
download_and_load_zip <- function(url, outdir = here("inputs", "nhts_2016")) {
  
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  zip_path <- file.path(outdir, "data.zip")
  download.file(url, zip_path, mode = "wb")
  
  unzip(zip_path, exdir = outdir)
  
  csv_files <- list.files(outdir, pattern = "\\.csv$", full.names = TRUE)
  
  data_list <- lapply(csv_files, read.csv)
  
  names(data_list) <- tools::file_path_sans_ext(basename(csv_files))
  
  return(data_list)
}

#### nhts links and function 
nhts2022 <- download_and_load_zip("https://nhts.ornl.gov/media/2022/download/csv.zip")
nhts2017 <- download_and_load_zip("https://nhts.ornl.gov/media/2016/download/csv.zip")

NHTS_personReport<-nhts2022$perv2pub
NHTS_tripReport<-nhts2022$tripv2pub
NHTS_hhReport<-nhts2022$hhv2pub
NHTS_personReport2017<-nhts2017$perv2pub
rm(nhts2022,nhts2017)

# create df with trip and person reports by matching the person ID
NHTS_tripReport$ID<-paste(NHTS_tripReport$HOUSEID,NHTS_tripReport$PERSONID)
NHTS_personReport$ID<-paste(NHTS_personReport$HOUSEID,NHTS_personReport$PERSONID)
peopletrip<-merge(NHTS_tripReport, NHTS_personReport, by="ID")



# Calculations ####
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


# PLACES from CDC is used for data related to disability prevalence
# CDC places API: get county-level data for the USA (release 2024)
PLACES <- get_places(geography = "county",
                 release = 2024,
                 geometry = FALSE,
                 measure = c("DISABILITY","COGNITION"))

# average across the counties, to find a state average 
PLACES_state <- PLACES %>% 
  filter(datavaluetypeid=="CrdPrv")%>% # PLACES reports prevalence as a percent
  mutate(
    data_value = as.numeric(data_value),
    totalpop18plus = as.numeric(totalpop18plus)
  ) %>%
  group_by(statedesc, measure) %>%
  summarise(affected = weighted.mean(data_value, totalpop18plus) )%>%
  pivot_wider(
    id_cols = statedesc,
    names_from = measure,
    values_from = affected)


# remove cognitive disabilities from the total, an explanation can be found in the methodology 
PLACES_state <- PLACES_state %>%
  mutate(
    disabilities          = `Any disability among adults` - `Cognitive disability among adults`,
    disability_constraints = (1 - pdrivingdis) * disabilities
  )%>%
  select(1,5)

rm(PLACES)

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


#### Add geometries ####
state_codes <- unique(fips_codes$state_code)[1:51]
# use the api to pull a shapefile of census tracts
tracts_usa <- get_decennial(
  geography = "tract",
  variables = "P1_001N",   # dummy variable
  year = 2020,
  sumfile = "pl",
  geometry = TRUE,
  keep_geo_vars = TRUE,
  state = state_codes 
) 

# use the api to pull a shapefile of congressional districts
cd_usa <- get_acs(
  geography = "congressional district",
  variables = "B01003_001",   # dummy variable
  year = 2022,
  geometry = TRUE,
  state = state_codes 
) 

# select workingDF1 fields
workingDF1<-workingDF%>%
  select(-c(4:29,31:46,51:53,56:101,108,110))%>%
  mutate(across(where(is.numeric), round))
rm(workingDF)  
workingDF1$NAME<-gsub(".*;","",workingDF1$NAME)
workingDF1$NAME <- sub(" ", "", workingDF1$NAME)

# add the census shapefile 
geofile <- tracts_usa %>%
  select(GEOID, geometry)%>%
  right_join(workingDF1, by = "GEOID")

## convert to congressional districts 
cd_usa$CD119FP = substr(cd_usa$GEOID, 3,4)
cd_usa<-cd_usa %>%
  select(1,6,7)%>%
  st_as_sf()%>%
  st_transform(st_crs(geofile))%>%
  st_make_valid()


# Compute centroids of geofile and join with the congressional district geometries
geofile_with_cd <- geofile %>%
  st_make_valid() %>%
  st_centroid()%>%
  st_join(cd_usa, join = st_within) # Join centroids to congressional districts

geofile_clean<- geofile_with_cd %>%
  st_drop_geometry() %>%  
  group_by(GEOID.y,NAME) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  merge(cd_usa, by.y = "GEOID", by.x="GEOID.y", all.x=TRUE)%>%
  st_as_sf()


names(geofile_clean)[names(geofile_clean) == "cars"] <- "cars_"
write_sf(geofile_clean,here("outputs/baseCalc.geojson)"))

