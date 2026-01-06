# Calculations for chauffeuring and cost data
# by Ali Lehman
# December 2025


# package upload ---- 
packages <- c(
  'sf',
  'tidyverse',
  'CDCPLACES',
  'dplyr',
  'rjson',  
  'jsonlite',
  'tigris',
  'tidycensus',
  'data.table',
  'viridis',
  'zip',
  'fs',
  'here',
  'osmdata',
  'sfnetwork',
  'ggplot2',
  "httr", 
  "readr",
  'dodgr'
)

if(!require(pacman)) install.packages('pacman')
pacman::p_load(packages,character.only = T)

# Burden of chauffeuring-----
## NHTS trips, chauffeured vs non chauffeured 
# filter off vehicle trips only and hh with at least 1 car
vehicleTrips <- NHTS_tripReport%>%
  filter(TRPTRANS == c(1, 2, 3, 4, 6, 7) ) %>%
  filter(LIF_CYC == c(3:8) ) %>%
  filter(TRPHHVEH>0)

chaufe<-
  sum(vehicleTrips$WTTRDFIN[vehicleTrips$WHYTRP1S == "70"] *
        vehicleTrips$TRPMILES[vehicleTrips$WHYTRP1S == "70"],
      na.rm = TRUE)

allTrips <- 
  sum(vehicleTrips$WTTRDFIN*vehicleTrips$TRPMILES, 
      na.rm = TRUE)


pChauferedTrip<-as.numeric(chaufe / allTrips)
averageSL=25 #average speed limit in the usa  


base_url <- "https://htaindex.cnt.org/download/download.php?data_yr=2022&focus=tract&geoid="

state_fips <- sprintf("%02d", 13)

temp_dir <- tempdir()
dir.create(file.path(temp_dir, "hta_downloads"), showWarnings = FALSE)
df_list <- list()

# Loop over states to download, unzip, and read the CSVs
for (fips in state_fips) {
  # Construct file URL
  file_url <- paste0(base_url, fips)
  
  # Define file paths
  zip_file <- file.path(temp_dir, paste0("state_", fips, ".zip"))
  extract_folder <- file.path(temp_dir, paste0("extracted_", fips))
  dir.create(extract_folder, showWarnings = FALSE)
  
  # Download and unzip the file
  tryCatch({
    GET(file_url, write_disk(zip_file, overwrite = TRUE))
    
    unzip(zip_file, exdir = extract_folder)
    
    csv_file <- list.files(extract_folder, pattern = "\\.csv$", full.names = TRUE)

    if (length(csv_file) > 0) {
      df <- read_csv(csv_file[1], show_col_types = FALSE)
      df$state_fips <- fips  # Add state identifier
      df_list[[fips]] <- df
    }
    
    message("Processed: ", fips)
    
  }, error = function(e) {
    message("Error processing: ", fips, " - ", e$message)
  })
}

###### organize H& T data ----


# match with states 
HandTdata <- bind_rows(df_list)%>%
  mutate(
    state = str_extract(cbsa, "\\b[A-Z]{2}\\b"),
    tract=str_replace(tract,"\"",""),
    tract=str_replace(tract,"\"",""))

HandTdata1<- HandTdata %>%
  select(tract,
         auto_ownership_cost_ami,
         vmt_cost_ami,
         vmt_per_hh_ami,
         h_cost,
         emp_gravity)%>%
  mutate(transitCost=auto_ownership_cost_ami+vmt_cost_ami)%>%
  mutate(chaufferCOST=vmt_cost_ami*pChauferedTrip)%>%
  mutate(chaufferVMT=vmt_per_hh_ami*pChauferedTrip)%>%
  mutate(chaufferHOURS=vmt_per_hh_ami*pChauferedTrip/averageSL)%>%
  mutate(housingCost=12*h_cost)%>% 
  select(1,8:11)


# another difference is i save the cost and baseline data together 
 export<- merge(geofile,HandTdata1, by.x="GEOID", by.y="tract")

 write_sf(export,here("outputs/GAbasecalc.geojson)"))
 