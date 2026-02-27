# Calculations and data for NHTS inputs

# by Ali Lehman
# Nov 2025


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

NHTS_personReport<-nhts2022$perv2pub%>%
  filter(CENSUS_D== 5)
NHTS_tripReport<-nhts2022$tripv2pub%>%
  filter(CENSUS_D== 5)
NHTS_hhReport<-nhts2022$hhv2pub%>%
  filter(CENSUS_D== 5)
rm(nhts2022)

# create df with trip and person reports by matching the person ID
NHTS_tripReport$ID<-paste(NHTS_tripReport$HOUSEID,NHTS_tripReport$PERSONID)
NHTS_personReport$ID<-paste(NHTS_personReport$HOUSEID,NHTS_personReport$PERSONID)
peopletrip<-merge(NHTS_tripReport, NHTS_personReport, by="ID")