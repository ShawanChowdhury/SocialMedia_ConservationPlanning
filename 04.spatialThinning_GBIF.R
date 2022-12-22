# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(raster)
library(dismo)
library(spThin)
library(tidyverse)
library(dplyr)

data <- read_csv("data/cleanedRecords_GBIF.csv")
head(data)

#######################
# List of species
species_list <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
sp <- unique(species_list$species)

#######################
# List of species
files <- list.files(path = "data/sp_thin_GBIF/", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
files <- as.data.frame(files)
colnames(files) <- "sp"

sp <- unique(files$sp)

for (i in sp) try({
  
  print(i)
  sp_name <- gsub("data/sp_thin_GBIF/", "", i)
  sp_name <- gsub("_thin.csv", "", sp_name)
  
  species_list <- species_list[!(species_list$species %in% c(sp_name)), ]
  
}, silent = FALSE)

# data1 <- data %>%
#   group_by(species) %>%
#   filter(NROW(species) > 999)
#######################

sp <- unique(species_list$species)

for (i in sp) try({
  print(i)
  
  sp_data <- data %>% 
    filter(species == i)
  
  # Spatial thinning
  thinned_dataset_full <-
    thin( loc.data = sp_data, 
          lat.col = "decimalLatitude", long.col = "decimalLongitude", 
          spec.col = "species", 
          thin.par = 1, reps = 10000, 
          locs.thinned.list.return = TRUE, 
          write.files = FALSE, 
          write.log.file = FALSE)
  
  max_idx <- which.max(sapply(thinned_dataset_full, nrow))
  thinned_dataset_max_rows <- thinned_dataset_full [[max_idx]]
  colnames(thinned_dataset_max_rows) <- c("decimalLongitude", "decimalLatitude")
  
  thin_data <- thinned_dataset_max_rows %>%
    dplyr::select("decimalLongitude", "decimalLatitude") %>% 
    dplyr::mutate(species = i)
  
  write_csv(thin_data, paste0("data/sp_thin_GBIF/", i, "_thin.csv"))
}, silent = FALSE)


# In parallel
# Define variables
n_threads <- 23

# Setup parallel cluster
cl <- makeCluster(n_threads, "PSOCK") # create workers
clusterEvalQ(cl, { # load packages into workers
  library(raster)
  library(dismo)
  library(spThin)
  library(tidyverse)
  library(dplyr)
  library(parallel)
  library(foreach)
})
clusterExport(cl, c("sp", "data1"))

# Main processing
result <- try(parLapply(cl, sp, function(i) {
  
  sp_data <- data1 %>% 
    filter(species == i)
  
  # Spatial thinning
  thinned_dataset_full <-
    thin( loc.data = sp_data, 
          lat.col = "decimalLatitude", long.col = "decimalLongitude", 
          spec.col = "species", 
          thin.par = 1, reps = 10000, 
          locs.thinned.list.return = TRUE, 
          write.files = FALSE, 
          write.log.file = FALSE)
  
  max_idx <- which.max(sapply(thinned_dataset_full, nrow))
  thinned_dataset_max_rows <- thinned_dataset_full [[max_idx]]
  colnames(thinned_dataset_max_rows) <- c("decimalLongitude", "decimalLatitude")
  
  thin_data <- thinned_dataset_max_rows %>%
    dplyr::select("decimalLongitude", "decimalLatitude") %>% 
    dplyr::mutate(species = i)
  
  write.csv(thin_data, paste0("data/sp_thin_GBIF/", i, "_thin.csv"))
  
}), silent = FALSE)

# Stop cluster
cl <- stopCluster(cl)
# Merge species
input_folder <- "data/sp_thin_GBIF/"
list <- dir(input_folder, "^.*\\.csv$", full.names = TRUE)
ov <- plyr::ldply(list, readr::read_csv)

write_csv(ov, "data/combinedRecords_thinned_GBIF.csv")

##########################################################################
# Spatial thinnin [GBIF, overall]
data <- read_csv("data/cleanedRecords_GBIF.csv")
head(data)

data <- data %>% mutate(species = "species") %>% 
  select(species, decimalLongitude, decimalLatitude)

# Spatial thinning
thinned_dataset_full <-
  thin( loc.data = data, 
        lat.col = "decimalLatitude", long.col = "decimalLongitude", 
        spec.col = "species", 
        thin.par = 1, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = FALSE, 
        write.log.file = FALSE)

max_idx <- which.max(sapply(thinned_dataset_full, nrow))
thinned_dataset_max_rows <- thinned_dataset_full [[max_idx]]
colnames(thinned_dataset_max_rows) <- c("decimalLongitude", "decimalLatitude")

thin_data <- thinned_dataset_max_rows %>%
  dplyr::select("decimalLongitude", "decimalLatitude")

write.csv(thin_data, paste0("data/sp_thin_GBIF/", i, "_thin.csv"))