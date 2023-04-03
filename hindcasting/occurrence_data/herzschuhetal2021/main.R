
#--------------------------------------------------------#
# Generate occurrence data to evaluate model performance #
#--------------------------------------------------------#

# Author: V. Van der Meersch
# Date: 10/03/2023

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"

input_folder <- "D:/species/pollen/herzschuhetal2021"
output_folder <- "D:/species/pollen/processed/fagus_sylvatica"

library(data.table)
library(dplyr)
library(terra)


# Load European dataset (Herzschuh et al. 2022)
dataset_count <- fread(file.path(input_folder, "Herzschuh-etal_2021_Europe-counts.tab"), skip = 1302) %>%
  mutate(pollen_count = rowSums(across(15:223))) %>%
  dplyr::filter(pollen_count >= 150) # removing sequences with less thant 150 pollen grain counts

# Load metadata
metadata <- fread(file.path(input_folder, "Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))

# Removing marine sites following Maguire et al. (2016)
dataset_count <- left_join(dataset_count, metadata) %>%
  dplyr::filter(`Loc type` != "Marine" & `Loc type` != "Lagoon")

# Load climate data grid
grid <- fread("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/05deg/15BP/HadCM3B_Altitude.fit")
names(grid) <- c("lat", "lon", "alt")
grid <- rast(grid[,c("lon", "lat", "alt")])

field <- "Fag [#]" # species fieldname in Herzschuh dataset 
for(year in seq(0, 21000, 500)){
  pres_r <- create_species_raster(year, field = field, dataset_count, grid)
  pres_df <- as.data.frame(pres_r, xy = T)
  names(pres_df) <- c("lon", "lat", "pres")
  
  saveRDS(pres_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
}



