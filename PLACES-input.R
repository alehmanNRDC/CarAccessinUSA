# Calculations and data for PLACES data inputs

# by Ali Lehman
# Feb 2026

# PLACES from CDC is used for data related to disability prevalence
# CDC places API: get county-level data for the USA (release 2024)


PLACES <- get_places(geography = "census",
                     release = 2024,
                     geometry = FALSE,
                     measure = c("DISABILITY","COGNITION"))

# average across the counties, to find a state average 
PLACES_census <- PLACES %>% 
  filter(datavaluetypeid=="CrdPrv")%>% # PLACES reports prevalence as a percent
  mutate(
    data_value = as.numeric(data_value)* as.numeric(totalpop18plus),
    totalpop18plus = as.numeric(totalpop18plus)
  ) %>%
  group_by(measure,locationname) %>%
  summarise(affected = weighted.mean(data_value, totalpop18plus) )%>%
  pivot_wider(
    id_cols = locationname,
    names_from = measure,
    values_from = affected)



rm(PLACES)