# Setting working directory
setwd("../ConservationPlanning/")

# Loading required libraries
library(sf)
library(wdpar)
library(dplyr)
library(sp)
library(raster)
library(fasterize)

# download protected area data for Bangladesh
# (excluding areas represented as point localities)
raw_pa_data <- wdpa_fetch("Bangladesh", wait = TRUE,
                              download_dir = rappdirs::user_data_dir("wdpar"))

# clean data
pa_data <- wdpa_clean(raw_pa_data)

# save data
st_write(pa_data, "data/layer/pa_bd_up.shp")

# Rasterize PA data
# Reading PA data
pa <- st_read("data/layer/pa_bd_up.shp")
sp_raster <- raster("outputs/BinaryRasters/BinaryAbisara_echerius.tif")

# Rasterize PA
pa_raster <- fasterize(pa, sp_raster)

writeRaster(pa_raster, "data/layer/pa_bd_up.tif")