# Loading required libraries
library(rgbif)
library(tidyverse)

# Reading species list
sp_list <- read_csv("data/speciesList_up.csv", header = T)
sp <- unique(sp_list$species)

#sp = "Pandion haliaetus"

for (i in sp) try({
  print(i)
  
  occ_data <- occ_search(scientificName = i, hasCoordinate = T,
                         fields=c("species", "decimalLongitude", "decimalLatitude", "countryCode", 
                                                   "gbifID", "family", "taxonRank", "coordinateUncertaintyInMeters", "year", "month",
                                                   "basisOfRecord", "institutionCode", "datasetName"), limit=10000, country = "BD")$data
  
  if(nrow(occ_data > 0)) {
    speciesname <- gsub(" ", "_", i)
    occ_data <- occ_data %>% 
      dplyr::mutate(species = speciesname)
    fwrite(occ_data, file = paste0("data/gbif/", speciesname, ".csv"))
  }
}, silent = FALSE)

# Merging GBIF data
input_folder = "data/gbif"
gbif = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
gbif_data <- plyr::ldply(gbif, data.table::fread)

write_csv(gbif_data, "data/gbif_data.csv")

# Inspecting GBIF data
gbif <- read_csv("data/gbif_data.csv")
head(gbif)

sum <- gbif %>% 
  group_by(species) %>% 
  summarise(n = NROW(species)) %>% 
  ungroup()

write_csv(sum, "data/gbif_data_summarised.csv")

