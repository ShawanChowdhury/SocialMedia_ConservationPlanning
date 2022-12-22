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
raster_file <- raster("outputs/BinaryRasters_Built-upAreasRemoved/Chloropsis_hardwickii.tif")
pa <- resample(pa, raster_file, method = "ngb")

tifs <- list.files(path = "outputs/BinaryRasters_Built-upAreasRemoved",
                   pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

speciesList <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
species <- unique(speciesList$species)

# Removed Regional Extinc (RE) species : "Arborophila_rufogularis", "Canis_lupus", "Francolinus_gularis", 
# "Francolinus_pondicerianus", "Pavo_cristatus", "Pavo_muticus"

###############
# In parallel
# Define variables
n_threads <- 23

# Setup parallel cluster
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
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
})
clusterExport(cl, c("tifs", "pa"))

# Main processing
result <- try(parLapply(cl, species, function(i) {
  
  raster_file <- tifs[stringr::str_detect(tifs, as.character(i))]
  sdm.hull <- raster(raster_file)
  
  # Protected area overlap (CH)
  sdm.area <- cellStats(sdm.hull, "sum") * 0.693
  ov.sdm <- cellStats(raster::Which(pa > 0 & sdm.hull > 0), "sum") * 0.693
  
  # Saving Model Output
  x <- cbind(sdm.area, ov.sdm)
  Output <- 
    as.data.frame(x) %>% 
    mutate(species = i) %>%
    dplyr::select(species, everything())
  
  write_csv(Output, file = paste0("outputs/pa_ov/","", i, ".csv"))
  
}), silent = TRUE)

# Stop cluster
cl <- stopCluster(cl)

###################################
# Missing species
###################################
# Problematic species: "Saxicola_caprata", "Rynchops_albicollis", "Platalea_leucorodia"

# List of species
sp_list <- read_csv("outputs/ModelOutput.csv")

files <- list.files(path = "outputs/pa_ov/", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
files <- as.data.frame(files)

speciesList <- str_split_fixed(files$files, "/", 3)
speciesList <- as.data.frame(speciesList)

speciesList <- speciesList %>% 
  dplyr::select(V3)

colnames(speciesList) <- "sp"

sp <- unique(speciesList$sp)

for (i in sp) try({
  
  print(i)
  
  sp_name <- gsub(".csv", "", i)
  
  sp_list <- sp_list[!(sp_list$species %in% c(sp_name)), ]
  
}, silent = FALSE)

#########################
species <- unique(sp_list$species)

for (i in species) try({
  print(i)
  
  raster_file <- tifs[stringr::str_detect(tifs, as.character(i))]
  sdm.hull <- raster(raster_file)
  
  # Protected area overlap (CH)
  sdm.area <- cellStats(sdm.hull, "sum") * 0.693
  ov.sdm <- cellStats(raster::Which(pa > 0 & sdm.hull > 0), "sum") * 0.693
  
  # Saving Model Output
  x <- cbind(sdm.area, ov.sdm)
  Output <- 
    as.data.frame(x) %>% 
    mutate(species = i) %>%
    dplyr::select(species, everything())
  
  write_csv(Output, file = paste0("outputs/pa_ov/","", i, ".csv"))
  
}, silent = FALSE)
#####################################
# Merge output
input_folder = "outputs/pa_OV"
output = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
pa_ov <- plyr::ldply(output, data.table::fread)

# Mutating % of coverage
pa_ov_per <- pa_ov %>% 
  dplyr::mutate(sdm = ov.sdm/sdm.area*100)
head(pa_ov_per)

write_csv(pa_ov_per, "outputs/pa_ov.csv")

