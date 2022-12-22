# Loading required libraries
library(sp)
library(dplyr)
library(sf)
library(raster)
library(dismo)
library(rgeos)
library(stringr)
library(rgdal)
library(parallel)
library(foreach)

# Reading PA dataset
pa <- raster("data/layer/pa_bd_up.tif") 

# Resample PA layer
raster_file <- raster("outputs/BinaryRasters_Built-upAreasRemoved_GBIF/birds/Abroscopus_superciliaris.tif")
pa <- resample(pa, raster_file, method = "ngb")

tifs <- list.files(path = "outputs/BinaryRasters_Built-upAreasRemoved_GBIF/",
                   pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

speciesList <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
species <- unique(speciesList$species)

# Removed Regional Extinct (RE) species : "Arborophila_rufogularis", "Canis_lupus", "Francolinus_gularis", 
# "Francolinus_pondicerianus", "Pavo_cristatus", "Pavo_muticus"

for(i in species) try({
  
  print(i)
  
  raster_file <- tifs[stringr::str_detect(tifs, as.character(i))]
  sdm.hull <- raster(raster_file)
  
  # Protected area overlap
  sdm.area <- cellStats(sdm.hull, "sum") * 0.693
  ov.sdm <- cellStats(raster::Which(pa > 0 & sdm.hull > 0), "sum") * 0.693
  
  # Saving Model Output
  x <- cbind(sdm.area, ov.sdm)
  Output <- 
    as.data.frame(x) %>% 
    mutate(species = i) %>%
    dplyr::select(species, everything())
  
  write_csv(Output, file = paste0("outputs/pa_ov_GBIF/","", i, ".csv"))
  
}, silent = TRUE)

#####################################
# Merge output
input_folder = "outputs/pa_OV_GBIF"
output = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
pa_ov <- plyr::ldply(output, data.table::fread)

# Mutating % of coverage
pa_ov_per <- pa_ov %>% 
  dplyr::mutate(sdm = ov.sdm/sdm.area*100)
head(pa_ov_per)

write_csv(pa_ov_per, "outputs/pa_ov_GBIF.csv")

