# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(spocc)
library(ENMeval)
library(stringr)
library(rgdal)
library(tidyverse)
library(parallel)
library(foreach)

# Reading data file
sp_data <- read_csv("data/combinedRecords_thinned.csv")
sp_data <- sp_data %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude)

#attaching climatic layers
vars<-c("bio1.tif","bio3.tif","bio9.tif","bio11.tif","bio14.tif","bio15.tif","bio17.tif","bio18.tif","lc_up.tif")
clim.stack <- stack(paste(getwd(),"/data/clim/", vars, sep=""))

species <- unique(sp_data$species)

# Define variables
n_threads <- 23

# Setup parallel cluster
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(sp)
  library(raster)
  library(dismo)
  library(dplyr)
  library(sf)
  library(spocc)
  library(ENMeval)
  library(stringr)
  library(rgdal)
  library(tidyverse)
  library(parallel)
  library(foreach)
})
clusterExport(cl, c("sp_data", "clim.stack"))

# Main processing
result <- try(parLapply(cl, species, function(i) {
  
  speciesname <- gsub(" ", "_", i)
  dc <- sp_data %>% filter(species==i)
  d.c<- dc[,2:3]
  d.c <- as.data.frame(d.c)
  d.c.sp <- SpatialPoints(d.c)
  bb <- bbox(d.c.sp)
  bb.buf <- extent(bb[1]-10, bb[3]+10, bb[2]-10, bb[4]+10)
  #clim.stack <- crop(clim.stack, bb.buf)
  bg<-randomPoints(clim.stack[[9]], n=5000)
  bg <- as.data.frame(bg)
  pred.mod.allyr <- ENMevaluate(d.c, clim.stack, bg, method='checkerboard2', 
                                RMvalues=seq(0.5, 4, 0.5), fc=c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), 
                                parallel=FALSE, kfolds = 10, algorithm='maxent.jar')
  
  aic.opt <- pred.mod.allyr@models[[which.max((pred.mod.allyr@results$avg.test.AUC))]]
  
  # Creating output directory
  dir.create(path = speciesname)
  output_dir <- paste0(speciesname, "/")
  
  ModelOutput <- 
    as.data.frame(aic.opt@results) %>% 
    rownames_to_column(var = "param") %>%
    spread(key = param, value = V1) %>%
    mutate(species = speciesname) %>%
    dplyr::select(species, everything())
  
  
  write_csv(ModelOutput, file = paste0(output_dir, "ModelOutput_", speciesname, ".csv"))
  
  VariableContribution <- var.importance((aic.opt))
  write_csv(VariableContribution, file = paste0(output_dir, "VariableContribution_", speciesname, ".csv"))
  
  r <- dismo::predict(aic.opt, clim.stack)
  writeRaster(r, paste0(output_dir, "Map_", speciesname, ".tif"), NAflag=-9999, overwrite = TRUE)
  
  r_bin <- r >= ModelOutput$Maximum.training.sensitivity.plus.specificity.Cloglog.threshold
  
  writeRaster(r_bin, paste0(output_dir, "Binary", speciesname, ".tif"), NAflag=-9999, overwrite = TRUE)
  
}), silent = FALSE)

# Stop cluster
cl <- stopCluster(cl)

###################################
# Identifying species for which there was no model output
# List of species
files <- list.files(path = "outputs/sdm/", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
files <- files[stringr::str_detect(files, "ModelOutput")]
files <- as.data.frame(files)

speciesList <- str_split_fixed(files$files, "/", 5)
speciesList <- as.data.frame(speciesList)
speciesList <- speciesList %>% 
  dplyr::select(V4)

colnames(speciesList) <- "sp"

sp <- unique(speciesList$sp)

for (i in sp) try({
  
  print(i)
  
  sp_name <- gsub("_", " ", i)
  
  sp_data <- sp_data[!(sp_data$species %in% c(sp_name)), ]
  
}, silent = FALSE)


###################################
# Merging model output and joining that with the total number of occurrence records by species
tifs <- list.files(path = "outputs/sdm/", pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
tifs <- tifs[stringr::str_detect(tifs, "Binary")]

# Exporting species details
speciesList <- as.data.frame(tifs)
speciesList <- str_split_fixed(speciesList$tifs, "/", 5)
speciesList <- as.data.frame(speciesList)
speciesList <- speciesList %>% 
  dplyr::select(V4)
colnames(speciesList) <- "species"
write_csv(speciesList, "outputs/species_sdm.csv")

# Merging model output
input_folder = "outputs/ModelOutput"
sdm = dir(input_folder, "^.*\\.csv$", full.names = TRUE)
ModelOutput <- plyr::ldply(sdm, data.table::fread)

sp_data <- read_csv("data/combinedRecords_thinned_up.csv")

n_rec <- sp_data %>% 
  group_by(species) %>% 
  summarise(n = NROW(species))

ModelOutput_n_rec <- dplyr::left_join(ModelOutput, n_rec, by = "species")

write_csv(ModelOutput_n_rec, "outputs/ModelOutput.csv")
