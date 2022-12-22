# In this study, our aim is to assess the performance of current PAs and identifying irreplaceable areas
# to establish future PAs, and therefore, meeting global biodiversity conservation targets. It's kind of 
# impossible to designate new PAs in built-up areas. To solve the issue, we have removed built-up areas
# from the suitability map for each species.

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
library(tidyverse)


# Extracting built-up areas
lc <- raster("data/clim/lc_up.tif")
lc <- lc == 50
# lc[lc == 0] <- NA

# List of rasters
tifs <- list.files(path = "outputs/binary_rasters_GBIF/", pattern = ".tif", recursive = TRUE, full.names = TRUE)

# # Species list
# sp_list <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
# sp <- unique(sp_list$species)

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
clusterExport(cl, c("tifs", "lc"))

# Main processing
result <- try(parLapply(cl, tifs, function(i) {
  
  raster_file <- tifs[stringr::str_detect(tifs, as.character(i))]
  sdm.hull <- raster(raster_file)
  
  # new_raster <- overlay(sdm.hull,
  #                       lc,
  #                       fun=function(r1, r2){return(r1-r2)})
  
  new_raster <- sdm.hull - lc
  new_raster[new_raster == -1] <- NA
  #new_raster <- new_raster == 1
  
  speciesname <- gsub("outputs/binary_rasters_GBIF/Binary", "", i)
  speciesname <- gsub(".tif", "", speciesname)
  
  writeRaster(new_raster, paste0("outputs/BinaryRasters_Built-upAreasRemoved_GBIF/",
                                 speciesname, ".tif"), NAflag=-9999, overwrite = TRUE)
  
}), silent = TRUE)

# Stop cluster
cl <- stopCluster(cl)

############################
# Species list
sp_list <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
sp <- unique(sp_list$species)

# List of rasters
tifs <- list.files(path = "outputs/BinaryRasters_Built-upAreasRemoved_GBIF/all/", pattern = ".tif", recursive = TRUE, full.names = TRUE)

for(i in sp) try({
  print(i)
  
  raster_file <- tifs[stringr::str_detect(tifs, as.character(i))]
  
  r <- raster(raster_file)
  
  speciesname <- gsub("outputs/BinaryRasters_Built-upAreasRemoved_GBIF/all/", "", i)
  speciesname <- gsub(".tif", "", speciesname)
  
  writeRaster(r, paste0("outputs/BinaryRasters_Built-upAreasRemoved_GBIF/",
                                 speciesname, ".tif"), NAflag=-9999, overwrite = TRUE)
}, silent = FALSE)
