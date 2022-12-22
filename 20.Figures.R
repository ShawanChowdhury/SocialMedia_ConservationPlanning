library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(rasterVis)
library(viridis)
library(rworldmap)

#####################################################################
# Plotting priori areas
# Loading rasters
priori_gbif_birds <- raster("outputs/spatial_prioritization_birds_GBIF.tif")
priori_gbif_butterflies <- raster("outputs/spatial_prioritization_butterflies_GBIF.tif")
priori_combined_birds <- raster("outputs/spatial_prioritization_birds_combined.tif")
priori_combined_butterflies <- raster("outputs/spatial_prioritization_butterflies_combined.tif")

#####################################################################
# Plotting irreplaceable areas
ir_gbif_birds <- raster("outputs/irreplacecable_areas_birds_GBIF.tif")
ir_gbif_butterflies <- raster("outputs/irreplacecable_areas_butterflies_GBIF.tif")
ir_combined_birds <- raster("outputs/irreplacecable_areas_birds_combined.tif")
ir_combined_butterflies <- raster("outputs/irreplacecable_areas_butterflies_combined.tif")

s <- stack(ir_gbif_birds, ir_combined_birds, ir_gbif_butterflies, ir_combined_butterflies)

# levelplot(s, 
#           margin=FALSE,                       
#           colorkey=list(
#             space='bottom',                   
#             labels=list(at=0:0.004, font=4),
#             axis.line=list(col='black'),
#             width=0.75
#           ),    
#           par.settings=list(
#             strip.border=list(col='transparent'),
#             strip.background=list(col='transparent'),
#             axis.line=list(col='transparent')
#           ),
#           scales=list(draw=FALSE),            
#           col.regions=viridis(n = 10),                   
#           at=seq(0, 0.004, len=101),
#           names.attr=rep('', nlayers(s)))

cols <- colorRampPalette(viridis(200))

pdf("figures/irreplaceable_areas.pdf")

levelplot(s,
          layout = c(2,2),
          colorkey=list(
            space='right'),
          col.regions=cols,    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          names.attr=rep('', nlayers(s)))
dev.off()

#####################################################
# Proportion of records by species
# Importing data
sp_list <- read_csv("outputs/targetRepresentation_taxonomicDetails_up.csv")
head(sp_list)
sp_list <- sp_list[,c(1,4)]

gbif <- read_csv("data/combinedRecords_thinned_GBIF.csv")
combined <- read_csv("data/combinedRecords_thinned_up.csv")

gbif_sum <- gbif %>%
  group_by(species) %>% 
  summarise(gbif = NROW(species))

combined_sum <- combined %>%
  group_by(species) %>% 
  summarise(combined = NROW(species))
com <- dplyr::left_join(combined_sum, gbif_sum, by = "species")

com_up <- dplyr::left_join(sp_list, com, by = "species")
com_up <- com_up %>% 
  mutate(prop = (gbif/combined)*100)
write.csv(com_up, "data/combined_summary.csv")

###############################
com <- read_csv("data/combined_summary.csv")
head(com)

ggplot(com, aes(fct_reorder(species, prop), prop, fill = group, col = group)) +
  geom_bar(position = "identity", stat = "identity") + theme_classic() +
  scale_fill_manual(values = c("orange", "blue")) +
  scale_color_manual(values = c("orange", "blue")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "right",
        legend.title = element_blank()) + coord_flip() + 
  geom_hline(yintercept= 0, color = "black")

ggsave("figures/missing_GBIF.png")


###############################
# Basic data exploration [priori map and irreplaceable areas]
# Loading rasters
priori_gbif_birds <- raster("outputs/spatial_prioritization_birds_GBIF.tif")
priori_gbif_butterflies <- raster("outputs/spatial_prioritization_butterflies_GBIF.tif")
priori_combined_birds <- raster("outputs/spatial_prioritization_birds_combined.tif")
priori_combined_butterflies <- raster("outputs/spatial_prioritization_butterflies_combined.tif")

# Calculating area selection
table(getValues(priori_gbif_birds)) # 43887/173224 = 25.33%
table(getValues(priori_gbif_butterflies)) # 34600/173224 = 19.97%
table(getValues(priori_combined_birds)) # 46343/173224 = 26.75%
table(getValues(priori_combined_butterflies)) # 40520/173224 = 23.39%


# Resample land-cover data
lc <- raster("data/layer/land_cover_map_bd_crop.tif")
lc <- resample(lc, priori_gbif_birds, method = "ngb")

# Extracting land-use types
shrub <- lc == 20
herb <- lc == 30
crop <- lc == 40
water <- lc %in% c(80, 200)
herb_wetland <- lc == 90
forest <- lc %in% c(111:116, 121:126)

#############################################
# gbif_birds
# Prioritization area by land class types
shrub_priori <- cellStats(raster::Which(shrub > 0 & priori_gbif_birds > 0), "sum")
herb_priori <- cellStats(raster::Which(herb > 0 & priori_gbif_birds > 0), "sum")
crop_priori <- cellStats(raster::Which(crop > 0 & priori_gbif_birds > 0), "sum")
water_priori <- cellStats(raster::Which(water > 0 & priori_gbif_birds > 0), "sum")
herb_wetland_priori <- cellStats(raster::Which(herb_wetland > 0 & priori_gbif_birds > 0), "sum")
forest_priori <- cellStats(raster::Which(forest > 0 & priori_gbif_birds > 0), "sum")

# Converting it into a dataframe
lc_priori_gbif_birds <- as.data.frame(rbind(shrub_priori, herb_priori, crop_priori, water_priori,
                                 herb_wetland_priori, forest_priori))
colnames(lc_priori_gbif_birds) <- c("ncell")

lc_priori_gbif_birds <- lc_priori_gbif_birds %>% 
  mutate(lc = rownames(lc_priori_gbif_birds),
         per = ncell/sum(ncell)*100,
         source = "GBIF_birds")

rm(shrub_priori, herb_priori, crop_priori, water_priori, herb_wetland_priori, forest_priori)

#############################################
# gbif_butterflies
# Prioritization area by land class types
shrub_priori <- cellStats(raster::Which(shrub > 0 & priori_gbif_butterflies > 0), "sum")
herb_priori <- cellStats(raster::Which(herb > 0 & priori_gbif_butterflies > 0), "sum")
crop_priori <- cellStats(raster::Which(crop > 0 & priori_gbif_butterflies > 0), "sum")
water_priori <- cellStats(raster::Which(water > 0 & priori_gbif_butterflies > 0), "sum")
herb_wetland_priori <- cellStats(raster::Which(herb_wetland > 0 & priori_gbif_butterflies > 0), "sum")
forest_priori <- cellStats(raster::Which(forest > 0 & priori_gbif_butterflies > 0), "sum")

# Converting it into a dataframe
lc_priori_gbif_butterflies <- as.data.frame(rbind(shrub_priori, herb_priori, crop_priori, water_priori,
                                            herb_wetland_priori, forest_priori))
colnames(lc_priori_gbif_butterflies) <- c("ncell")

lc_priori_gbif_butterflies <- lc_priori_gbif_butterflies %>% 
  mutate(lc = rownames(lc_priori_gbif_butterflies),
         per = ncell/sum(ncell)*100,
         source = "GBIF_butterflies")

rm(shrub_priori, herb_priori, crop_priori, water_priori, herb_wetland_priori, forest_priori)

#############################################
# combined_birds
# Prioritization area by land class types
shrub_priori <- cellStats(raster::Which(shrub > 0 & priori_combined_birds > 0), "sum")
herb_priori <- cellStats(raster::Which(herb > 0 & priori_combined_birds > 0), "sum")
crop_priori <- cellStats(raster::Which(crop > 0 & priori_combined_birds > 0), "sum")
water_priori <- cellStats(raster::Which(water > 0 & priori_combined_birds > 0), "sum")
herb_wetland_priori <- cellStats(raster::Which(herb_wetland > 0 & priori_combined_birds > 0), "sum")
forest_priori <- cellStats(raster::Which(forest > 0 & priori_combined_birds > 0), "sum")

# Converting it into a dataframe
lc_priori_combined_birds <- as.data.frame(rbind(shrub_priori, herb_priori, crop_priori, water_priori,
                                                  herb_wetland_priori, forest_priori))
colnames(lc_priori_combined_birds) <- c("ncell")

lc_priori_combined_birds <- lc_priori_combined_birds %>% 
  mutate(lc = rownames(lc_priori_combined_birds),
         per = ncell/sum(ncell)*100,
         source = "compiled_birds")

rm(shrub_priori, herb_priori, crop_priori, water_priori, herb_wetland_priori, forest_priori)

#############################################
# combined_butterflies
# Prioritization area by land class types
shrub_priori <- cellStats(raster::Which(shrub > 0 & priori_combined_butterflies > 0), "sum")
herb_priori <- cellStats(raster::Which(herb > 0 & priori_combined_butterflies > 0), "sum")
crop_priori <- cellStats(raster::Which(crop > 0 & priori_combined_butterflies > 0), "sum")
water_priori <- cellStats(raster::Which(water > 0 & priori_combined_butterflies > 0), "sum")
herb_wetland_priori <- cellStats(raster::Which(herb_wetland > 0 & priori_combined_butterflies > 0), "sum")
forest_priori <- cellStats(raster::Which(forest > 0 & priori_combined_butterflies > 0), "sum")

# Converting it into a dataframe
lc_priori_combined_butterflies <- as.data.frame(rbind(shrub_priori, herb_priori, crop_priori, water_priori,
                                                herb_wetland_priori, forest_priori))
colnames(lc_priori_combined_butterflies) <- c("ncell")

lc_priori_combined_butterflies <- lc_priori_combined_butterflies %>% 
  mutate(lc = rownames(lc_priori_combined_butterflies),
         per = ncell/sum(ncell)*100,
         source = "compiled_butterflies")

rm(shrub_priori, herb_priori, crop_priori, water_priori, herb_wetland_priori, forest_priori)

merge_lc_priori <- rbind(lc_priori_gbif_birds, lc_priori_gbif_butterflies, 
                         lc_priori_combined_birds, lc_priori_combined_butterflies)
write_csv(merge_lc_priori, "outputs/merge_lc_priori.csv")

###########################################
merge_lc_priori <- read_csv("outputs/merge_lc_priori.csv")

merge_lc_priori %>% 
  dplyr::mutate(x = fct_reorder(lc, per)) %>% 
  ggplot( aes(x, per, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() + xlab("") + ylab("")  +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_fill_manual(values = c("orange", "blue")) + facet_wrap(~taxa)

###########################
# HFP and prioritization
###########################
# Loading rasters
priori_gbif_birds <- raster("outputs/spatial_prioritization_birds_GBIF.tif")
priori_gbif_butterflies <- raster("outputs/spatial_prioritization_butterflies_GBIF.tif")
priori_combined_birds <- raster("outputs/spatial_prioritization_birds_combined.tif")
priori_combined_butterflies <- raster("outputs/spatial_prioritization_butterflies_combined.tif")

hfp <- raster("data/layer/hfp_bd_re.tif")

r <- raster::stack(priori_gbif_birds, priori_gbif_butterflies, priori_combined_birds, priori_combined_butterflies, hfp)

vals <- values(r)
coord <- xyFromCell(r, 1:ncell(r))
combine <- cbind(coord, vals)
combine <- as.data.frame(combine)
head(combine)

# Removing NA values
combine <- combine %>% 
  filter(hfp_bd_re >= 0)

# Long dataframe
combine_long <- combine %>%
  pivot_longer(
    cols = starts_with("spatial_prioritization"),
    names_to = "group",
    values_to = "value"
  )

# Removing NA values
combine_long <- combine_long %>% 
  filter(value >= 0)

write_csv(combine_long, "outputs/combine_hfp_priori.csv")

######################################
com <- read_csv("outputs/combine_hfp_priori.csv")
head(com)

bin_size <- 2

com %>% 
  mutate(bin_y = factor(hfp_bd_re%/%bin_size)) %>% 
  ggplot(aes(bin_y, value, fill = source)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  labs(x = "", y = "") + facet_wrap(~taxa)+
  scale_fill_manual(values = c("orange", "blue"))

ggplot(com, aes(group, hfp_bd_re, fill = source)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "", y = "")

com_sum_group <- com %>% 
  group_by(group) %>% 
  summarise(mean_hfp = mean(hfp_bd_re), median_hfp = median(hfp_bd_re))

###################################
# Loading IR rasters
ir_gbif_birds <- raster("outputs/irreplacecable_areas_birds_GBIF.tif")
ir_gbif_butterflies <- raster("outputs/irreplacecable_areas_butterflies_GBIF.tif")
ir_combined_birds <- raster("outputs/irreplacecable_areas_birds_combined.tif")
ir_combined_butterflies <- raster("outputs/irreplacecable_areas_butterflies_combined.tif")

s <- stack(ir_gbif_birds, ir_combined_birds, ir_gbif_butterflies, ir_combined_butterflies)

vals <- values(s)
coord <- xyFromCell(s, 1:ncell(s))
combine <- cbind(coord, vals)
combine <- as.data.frame(combine)
head(combine)

# Long dataframe
combine_long <- combine %>%
  pivot_longer(
    cols = starts_with("irreplacecable_areas"),
    names_to = "group",
    values_to = "value"
  )

# Removing NA values
combine_long <- combine_long %>% 
  filter(value >= 0)
head(combine_long)

ggplot(combine_long, aes(group, value)) +
  geom_boxplot() +
  theme_classic() + 
  labs(x = "", y = "")

com_sum_group <- combine_long %>% 
  group_by(group) %>% 
  summarise(mean_hfp = mean(value), median_hfp = median(value), quantile = quantile(value))

com <- combine %>% 
  dplyr::filter(irreplacecable_areas_birds_GBIF > 0,
                irreplacecable_areas_birds_combined > 0,
                irreplacecable_areas_butterflies_GBIF > 0,
                irreplacecable_areas_butterflies_combined > 0)


quantile(com$irreplacecable_areas_birds_GBIF, na.rm = TRUE)
quantile(com$irreplacecable_areas_butterflies_GBIF, na.rm = TRUE)
quantile(com$irreplacecable_areas_birds_combined, na.rm = TRUE)
quantile(com$irreplacecable_areas_butterflies_combined, na.rm = TRUE)

#####################################################
# Supplementary figures
#####################################################
# Data distribution [Figure S1]
# Reading data file
data <- read_csv("data/combinedRecords_thinned_GBIF_fb_up.csv")

# Inspecting data
head(data)

# Getting Bangladesh map data
world <- getMap(resolution = "low")
bd <- world[world@data$NAME %in% "Bangladesh", ] 

ggplot(data, aes(lon, lat, col = source)) +
  geom_point(size = 0.05) + theme_classic() +
  xlab("Longitude") +ylab("Latitude") + scale_color_manual(values = c("deepskyblue4", "darkgoldenrod1")) +
  geom_polygon(data = bd, aes(x = long, y = lat, group = group),fill = NA, colour = "black") +
  coord_quickmap() + xlim(88, 93) + ylim(20, 27) + 
  theme(legend.title = element_blank(), legend.position = "none") + facet_wrap(~source)

ggsave("figures/data_dist_supp_fig.png")

#####################################################
# Spatial prioritisation for species that we obtained in both approaches [Figure S2]
library(raster)
library(RColorBrewer)
library(rasterVis)
library(viridis)

# Loading rasters
bird <- raster("outputs/spatial_prioritization_birds_combined_species_specific.tif")
butterflies <- raster("outputs/spatial_prioritization_butterflies_combined_species_specific.tif")

s <- stack(bird, butterflies)

png("figures/GraphicalAbstract.png")

levelplot(s,
          layout = c(1,1),
          colorkey=list(
            space='right'),
          col.regions=viridis,    
          par.settings=list(
            strip.border=list(col='transparent'),
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent')
          ),
          scales=list(draw=FALSE),
          names.attr=rep('', nlayers(s)))

dev.off()

# I finally created this figure in ArcMap
#####################################################
# Land-cover type and prioritisation [Figure S3]
# Reading data file
data <- read_csv("outputs/merge_lc_priori.csv")
head(data) 

data %>% 
  dplyr::mutate(x = fct_reorder(lc, per)) %>% 
  ggplot( aes(x, per)) +
  geom_bar(stat = "identity", position = "identity", fill = "darkgoldenrod1") +
  theme_classic() + xlab("") + ylab("")  +
  theme(legend.position = "none", legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_wrap(~source_taxa)

ggsave("figures/lc_priori_supp_fig.png")

#####################################################
# HFP index and prioritisation [Figure S4]
# Reading data file
data <- read_csv("outputs/combine_hfp_priori.csv")
head(data) 

bin_size <- 1

data %>% 
  filter(value == 1) %>% 
  mutate(bin_y = factor(hfp_bd_re%/%bin_size)) %>% 
  ggplot(aes(bin_y, value)) +
  geom_bar(stat = "identity", fill = "darkgoldenrod1") +
  theme_classic() + 
  labs(x = "Human footprint index", y = "Number of priority cells")  +
  theme(legend.position = "none", legend.title = element_blank()) + facet_wrap(~group) +
  scale_x_discrete(breaks = c(0, 10, 20, 30, 40, 50))

ggsave("figures/hfp_priori_supp_fig.png")

