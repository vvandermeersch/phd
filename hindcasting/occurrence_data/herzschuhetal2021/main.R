
#--------------------------------------------------------#
# Generate occurrence data to evaluate model performance #
#--------------------------------------------------------#

# Author: V. Van der Meersch
# Date: 10/03/2023

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"
input_folder <- "D:/species/pollen/herzschuhetal2021/Europe_pollen_datasets"

library(data.table)
library(dplyr)
library(terra)


#..................#
### PREPARE DATA ###
#..................#

# Load European dataset (Herzschuh et al. 2022)
dataset_count <- fread(file.path(input_folder, "pollen_counts_europe.csv")) %>%
  as.data.frame()

# Load taxa harmonization table:
Harmonisation_Master <- read.csv(file.path(wd, "LegacyPollen", "taxa_harmonisation_table_VV.csv"), 
                                 header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, encoding = "UTF-8")

# Make minor corrections
source(file.path(wd, "scripts", "legacy_corrections.R"))

# Filter data
dataset_count <- dataset_count %>%
  dplyr::select(-c(`Cyperaceae (#)`)) %>% # remove Cyperaceae
  dplyr::mutate(pollen_count = rowSums(across(15:222))) %>%
  dplyr::filter(pollen_count >= 200) %>% # removing sequences with less than 200 pollen grain counts 
  dplyr::filter(`maximum_Age (cal. ka BP)` - `minimum_Age (cal. ka BP)` <= 0.5) %>% # less than 500yr uncertainty
  dplyr::filter(!(`ID (Dataset)` %in% c(3902, 45214, 45216, 46713, 46714))) # problem with Neotoma2 database for these sample 

#N.B.: https://data.neotomadb.org/3902, https://data.neotomadb.org/45214, https://data.neotomadb.org/45216
# https://data.neotomadb.org/46713, https://data.neotomadb.org/46714
# these URLs do not exist!

# Load metadata
metadata <- fread(file.path(input_folder, "Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))

# Removing marine sites following Maguire et al. (2016)
dataset_count <- left_join(dataset_count, metadata, by = c("Dataset_ID" = "ID (Dataset)")) %>%
  dplyr::filter(`Loc type` != "Marine" & `Loc type` != "Lagoon")


#........................#
### COMPUTE OCCURRENCE ###
#........................#

# Choose species and year range
field <- "Abies (#)" # species fieldname in Herzschuh dataset 
years <- seq(500, 18000, 500)

# Find threshold with maximum relative abundance (not used anymore ?)
#sp_threshold <- find_threshold(years, window = 250, field, dataset_count, factor = 0.05)
sp_threshold <- 0.01 # fagus, abies
# sp_threshold <- 0.025 # quercus

# Loop on years
for(i in 1:length(years)){
  year <- years[i]
  
  grid <- readRDS(paste0("D:/simulations/phenofit/paleo/expert/025deg/fagus_sylvatica/",ifelse(year==0,15,year),"BP.rds"))
  names(grid) <- c("lat", "lon", "pred")
  grid <- rast(grid[,c("lon", "lat", "pred")])
  
  # near 1km resolution (needed for migration)
  grid_res <- grid
  res(grid_res) <- c(0.01, 0.01)
  grid <- resample(grid, grid_res)

  # "one is enough" binarization - specific threshold
  pollen_r <- create_pollen_raster(year, window = 250, 
                                   field = field, dataset_count, 
                                   grid,
                                   method = "one_is_enough", threshold = sp_threshold)
  pollen_df <- as.data.frame(pollen_r, xy = T)
  names(pollen_df) <- c("lon", "lat", "pres")
  pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  output_folder <- "D:/species/pollen/processed/quercus/1km/001thr_500yrunc"
  saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
}









