# Loading required libraries
library(dplyr)
library(ggplot2)
library(countrycode)
library(CoordinateCleaner)
library(tidyverse)
library(rworldmap)
##############################
# Cleaning GBIF data
# Reading GBIF data
gbif_data <- read_csv("data/gbif_data.csv")

# Removing blank cells
gbif_data <- gbif_data[!(is.na(gbif_data$species) | gbif_data$species == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLongitude) | gbif_data$decimalLongitude == ""),]
gbif_data <- gbif_data[!(is.na(gbif_data$decimalLatitude) | gbif_data$decimalLatitude == ""),]

# Removing duplicated records
gbif_data1 <- gbif_data[!duplicated(gbif_data),]

# Cleaning memory
rm(gbif_data)

# Convert country code from ISO2c to ISO3c
gbif_data1$countryCode <-  countrycode(gbif_data1$countryCode, origin =  'iso2c', destination = 'iso3c')

# Flag problems
gbif_data1 <- data.frame(gbif_data1)
flags <- clean_coordinates(x = gbif_data1, lon = "decimalLongitude", lat = "decimalLatitude",
                           countries = "countryCode", 
                           species = "species",
                           tests = c("centroids", "gbif",
                                     "zeros", "countries")) # most test are on by default

# summary(flags)
# plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

# Exclude problematic records
gbif_data_cl <- gbif_data1[flags$.summary,]

write_csv(gbif_data_cl, "data/cleanedRecords_GBIF.csv")

# Cleaning memory
rm(gbif_data1, flags)

##############################################
# Cleaning Facebook records
# Reading Facebook data
fb_data <- read_csv("data/facebookDataUp.csv")
head(fb_data)

# Changing column names
colnames(fb_data) <- c("class", "order", "family", "species", "commonName", "iucn", "lifeStage", "date",
                       "month", "year", "location", "decimalLatitude", "decimalLongitude", "photographer",
                       "comment", "status")
# Selecting columns of interest
fb_data <- fb_data %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude)


# Removing blank cells
fb_data <- fb_data[!(is.na(fb_data$species) | fb_data$species == ""),]
fb_data <- fb_data[!(is.na(fb_data$decimalLongitude) | fb_data$decimalLongitude == ""),]
fb_data <- fb_data[!(is.na(fb_data$decimalLatitude) | fb_data$decimalLatitude == ""),]

write_csv(fb_data, "data/cleanedRecords_FB.csv")

###########################################
# Combining dataframes
gbif <- gbif_data_cl %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude)
fb <- fb_data %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude)

combined_data <- rbind(fb, gbif)

# Remove species with low occurrence records
combined_data <- combined_data %>%
  group_by(species) %>%
  filter(n() > 3) %>%
  ungroup()

write_csv(combined_data, "data/cleanedRecords_combined.csv")

#########################################
# gbif <- gbif_data_cl %>% 
#   dplyr::select(species, decimalLongitude, decimalLatitude)
# clean <- read.csv("data/cleanedRecords_prev.csv", header = T)
# combined_data <- rbind(clean, gbif)
# 
# # Remove species with low occurrence records
# combined_data <- combined_data %>%
#   group_by(species) %>%
#   filter(n() > 3) %>%
#   ungroup()
# 
# write_csv(combined_data, "data/cleanedRecords.csv")

#########################################
# Predictor variable collinearity
#########################################

#attaching climatic layers
# Assessing collinearity between predictor variables
vars<-c("bio1.tif","bio2.tif","bio3.tif","bio4.tif","bio5.tif","bio6.tif","bio7.tif","bio8.tif","bio9.tif","bio10.tif","bio11.tif","bio12.tif","bio13.tif","bio14.tif","bio15.tif","bio16.tif","bio17.tif","bio18.tif","bio19.tif")
clim.stack <- stack(paste(getwd(),"/clim/", vars, sep=""))

# Assesing correlations between predictor variables
pairs(clim.stack, maxpixels=20000)

# Highly correlated variables (>0.75): bio2, bio4-8, bio10, bio12-13, bio16, bio19

# Predictor variables
vars<-c("bio1.tif","bio3.tif","bio9.tif","bio11.tif","bio14.tif","bio15.tif","bio17.tif","bio18.tif")

#########################################
# HFP data
#########################################
hfp <- raster("data/layer/HFP2009_clip_df.tif")
priori <- raster("outputs/spatial_prioritization.tif")

# Match rasters
# crs(hfp) <- crs(priori)
# hfp_up <- setExtent(hfp, extent(priori), snap = TRUE, keepres = TRUE)
# crs(hfp_up) <- crs(priori)
# extent(hfp_up) <- extent(priori)
# hfp_re <- resample(hfp_up, priori, method = "ngb")

crs(hfp) <- crs(priori)
# extent(hfp) <- extent(priori)
hfp_re <- resample(hfp, priori, method = "ngb")

# Exporting output
writeRaster(hfp_re, "data/layer/hfp_bd_re.tif", overwrite = TRUE)

#######################################
# Rasterize Bangladesh layer
#######################################
bd <- st_read("data/layer/bd_dissolved.shp")
pa <- raster("data/layer/pa_bd_up.tif")
crs(pa) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

# Rasterize CH
bd_raster <- fasterize(bd, pa)

bd_raster[is.na(bd_raster[])] <- 0

writeRaster(bd_raster, "data/layer/bd_ras_up.tif", overwrite = TRUE)

# ###########
# # Human footprint data
# pa <- raster("data/layer/pa_bd_up.tif")
# hfp <- raster("data/layer/hfp_bd.tif")
# 
# hfp_resampled <- resample(hfp, pa, method = "ngb")
# crs(hfp_resampled) <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
# writeRaster(hfp_resampled, "data/layer/hfp_bd_up.tif", overwrite = TRUE)

