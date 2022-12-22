# Calculate target protection
library(proto)
library(prioritizr)
library(tidyverse)
library(data.table)

sdm <- read_csv("outputs/pa_ov.csv")
head(sdm)

# create series of x-values
x <- sdm$sdm.area
# interpolate y-values for the x-values given the two reference points:
y <- loglinear_interpolation(x, 1000, 100, 148460, 10) # Area of Bangladesh = 148,460
y <- as.data.frame(y)

interpolation_sdm <- cbind(sdm, y)
# fwrite(interpolation_sdm, "interpolation_sdm.csv")

rm(sdm, y)

# Inadequate protection
head(interpolation_sdm)
# interpolation_sdm <- interpolation_sdm[,3:7]

colnames(interpolation_sdm)[5] <- c("TargetCoverage")

interpolation_gap_sdm <- interpolation_sdm %>%
  mutate(gap = (TargetCoverage - sdm))

write_csv(interpolation_gap_sdm, "outputs/interpolation_gap.csv")

###################
# Merging with the original file with the taxonomic details
# Reading the original file
all <- read_csv("data/taxa_iucn_details.csv")

head(all)
all <- all[,1:7]
head(interpolation_gap_sdm)

combined <- dplyr::right_join(all, interpolation_gap_sdm, by = c("species"))
write_csv(combined, "outputs/targetRepresentation_taxonomicDetails.csv")