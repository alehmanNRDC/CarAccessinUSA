# Calculations and data for charts and data points in the report
## first run the dashboard script to download the NHTS data 
# by Ali Lehman
# December 2025

rm(list = ls()) # Clear the work space
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets working directory to active document

# package upload ---- 
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


# 75+ basics ----
drivers_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$R_AGE >= 75], 
  na.rm = TRUE)
all_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$R_AGE >= 75], 
  na.rm = TRUE)

# 75+ can drive but not at night ----
drivers_over75_noNight = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$R_AGE >= 75 &
    NHTS_personReport$CONDNIGH == "1"], 
  na.rm = TRUE)
# 75+ doesn't drive at night, has HH car, drives not disabled ----
drivers_over75_constrainedORCArHH <- sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & # driver
    NHTS_personReport$R_AGE >= 75 & # over 75
    NHTS_personReport$HHVEHCNT >0 & # more than one household car
    NHTS_personReport$MEDCOND == "2"& # no travel limiting disability
    (
      (NHTS_personReport$CONDTRAV == "1" ) |
        (NHTS_personReport$CONDNIGH == "0")) # doesnt drive at night OR does have travel limitations
  
], na.rm = TRUE)
# 75 + doesn't drive at night, and has a HH car----
# this is the "constrained" data point from the dashboard 
drivers_over75_constrained <- sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$HHVEHCNT >0 & # 1+ household car
    NHTS_personReport$R_AGE >= 75 &
    (
      (NHTS_personReport$CONDTRAV == "1" ) |
        (NHTS_personReport$CONDNIGH == "0"))
  
], na.rm = TRUE)

pover75constrained = drivers_over75_constrained/all_over75
pover75NoNight = drivers_over75_noNight/drivers_over75


# Women vs men drivers 75+ ----
women_drivers_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$R_AGE >= 75&
    NHTS_personReport$R_SEX == "2"], 
  na.rm = TRUE)
women_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$R_AGE >= 75&
    NHTS_personReport$R_SEX == "2"], 
  na.rm = TRUE)
men_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$R_AGE >= 75&
    NHTS_personReport$R_SEX == "1"], 
  na.rm = TRUE)
men_drivers_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$R_AGE >= 75&
    NHTS_personReport$R_SEX == "1"], 
  na.rm = TRUE)

pWomenOver75Driver= women_drivers_over75/women_over75
pmenOver75Driver= men_drivers_over75/men_over75

1-pWomenOver75Driver
1-pmenOver75Driver

rm(women_drivers_over75,women_over75,men_over75,men_drivers_over75)

# mode choice ----
# this uses NHTS to find people who take more than half of their trips using nonAuto mode
IDs_peopleWhoTakeNonAuto <- peopletrip %>%
  filter(peopletrip$R_AGE.x > 10) %>%                  
  group_by(ID) %>%
  summarise(
    total_trips = n(),
    qualifying_trips = sum(TRPTRANS %in% c(8, 9, 10, 11, 17,18, 19,20))
  ) %>%
  filter(qualifying_trips >0) %>% # Keep if majority of trips qualify
  pull(ID)


NHTS_tripReport %>%
  mutate(mode = case_when(
    TRPTRANS %in% c(1, 2, 3, 4, 6, 7) ~ "Vehicle",
    TRPTRANS %in% c(8, 9, 10, 11) ~ "Public Transit",
    TRPTRANS == 17        ~ "Paratransit",
    TRPTRANS %in% c(15, 16) ~ "Ride Share",
    TRPTRANS %in% c(18, 19) ~ "Bike",
    TRPTRANS == 20   ~ "Walk",
    TRPTRANS %in% c(-9,14)                   ~ NA_character_,  # Omit no responses and airplane trips)
    TRUE             ~ "Other"
  )) %>%
  filter(!is.na(mode)) %>%
  group_by(mode) %>%
  summarise(total_trips = sum(WTTRDFIN , na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate percentage
  mutate(percent = total_trips / sum(total_trips) * 100) %>%
  ggplot(aes(x = reorder(mode, -percent), y = percent, fill = mode)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent of Total Trips",
       x = "Travel Mode",
       y = "Percent of Total Trips") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5)
# mode rural ----
NHTS_tripReport %>%
  filter(URBRUR==2)%>%
  mutate(mode = case_when(
    TRPTRANS %in% c(8, 9, 10, 11) ~ "Public Transit",
    TRPTRANS == 17        ~ "Paratransit",
    TRPTRANS %in% c(15, 16) ~ "Ride Share",
    TRPTRANS %in% c(18, 19) ~ "Bike",
    TRPTRANS == 20   ~ "Walk",
    TRPTRANS %in% c(-9,14,1, 2, 3, 4, 6, 7)  ~ NA_character_,  # Omit no responses and airplane trips)
    TRUE             ~ "Other"
  )) %>%
  filter(!is.na(mode)) %>%
  group_by(mode) %>%
  summarise(total_trips = sum(WTTRDFIN , na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate percentage
  mutate(percent = total_trips / sum(total_trips) * 100) %>%
  ggplot(aes(x = reorder(mode, -percent), y = percent, fill = mode)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent of Total Trips",
       x = "Travel Mode",
       y = "Percent of Total Trips") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5)

# mode school trips (children) ----
# NHTS Person Report query for how k-12 public and private school students get to school 

NHTS_schoolTrips<- NHTS_personReport2017 %>%
  filter(SCHTYP==1) %>% # not a home schooled student
  filter(R_AGE<14)%>%
  mutate(mode = case_when(
    SCHTRN1 %in% c(5, 6, 3, 4, 7,8,9) ~ "Driven by Caretaker",
    SCHTRN1 %in% c( 12,11,14,15.16,20)~ "Public Transit",
    SCHTRN1 == 10 ~ "School Bus",
    SCHTRN1 %in% c(13, 17,18,97) ~ "Other",
    SCHTRN1 %in% c(1, 2,20) ~ "Walked or Biked"
  )) %>%
  group_by(mode) %>%
  summarise(total_trips = sum(WTPERFIN , na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate percentage
  mutate(percent = total_trips / sum(total_trips) * 100) 

# disability mode

diability_altMODE<- NHTS_personReport %>%
  filter(R_AGE>18,
         CONDNONE ==2)%>%
  mutate(mode = case_when(
    CONDRIDE == 1 ~ "Asked others for rides",
    CONDSHARE == 1 ~ "Used rideshare",
    CONDSPEC == 1 ~ "Used special transportation",
    CONDTRAV == 1 ~ "Reduced travel",
    TRUE ~ NA_character_
  )) %>%
  group_by(mode) %>%
  summarise(total_trips = sum(WTPERFIN, na.rm = TRUE), .groups = "drop") %>%
  mutate(percent = total_trips / sum(total_trips) * 100)
