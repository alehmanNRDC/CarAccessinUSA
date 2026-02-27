# Calculations and data for US census data inputs

# by Ali Lehman
# Nov 2025

## i. Rural data  -----
# Using 2020 census rural and urban population. If 50% or more of the population in a census tract is rural, it is flagged as a rural tract 
## upload 2020 Census (Demographic and Housing Characteristics File) with rural and urban population
state_codes <- unique(fips_codes$state_code)[1:51] 

decennialCensus2020 <- get_decennial(
  geography = "tract", 
  state = statenumber,    
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
  state = statenumber,         # Must specify states
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
          state = statenumber,
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

### Combines ACS data into one DF 
workingDF<-merge(ACS2021,ACSage, by="GEOID")%>%
  mutate(pkid=(young10to15+under10)/B01001_001)%>%
  mutate(p10to15=young10to15/(B01001_001-under10))%>%
  mutate(TotalPopulationover10=B01001_001-under10)%>%
  mutate(adultTotal=B01001_001-under10-young10to15)%>%
  mutate(pct_kid_nonhh=((under10+young10to15))/(B01001_001-hh_totalcount))
