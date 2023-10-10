
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

# Load European dataset (Herzschuh et al. 2022)
dataset_count <- fread(file.path(input_folder, "pollen_counts_europe.csv")) %>%
  dplyr::select(-c(`Cyperaceae (#)`)) %>% # remove Cyperaceae
  dplyr::mutate(pollen_count = rowSums(across(15:222))) %>%
  dplyr::filter(pollen_count >= 200) %>% # removing sequences with less than 200 pollen grain counts 
  dplyr::filter(`maximum_Age (cal. ka BP)` - `minimum_Age (cal. ka BP)` <= 0.5)


# Load metadata
metadata <- fread(file.path(input_folder, "Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))

# Removing marine sites following Maguire et al. (2016)
dataset_count <- left_join(dataset_count, metadata, by = c("Dataset_ID" = "ID (Dataset)")) %>%
  dplyr::filter(`Loc type` != "Marine" & `Loc type` != "Lagoon")



field <- "Abies (#)" # species fieldname in Herzschuh dataset 
years <- seq(500, 18000, 500)

# Find threshold with maximum relative abundance
sp_threshold <- find_threshold(years, window = 250, field, dataset_count, factor = 0.05)
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
  
  
  # "one is enough" binarization - constant threshold of 2%
  # pollen_r <- create_pollen_raster(year, window = 250, 
  #                                  field = field, dataset_count, 
  #                                  grid,
  #                                  method = "one_is_enough", threshold = 0.02)
  # pollen_df <- as.data.frame(pollen_r, xy = T)
  # names(pollen_df) <- c("lon", "lat", "pres")
  # pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  # output_folder <- "D:/species/pollen/processed/larix_decidua/025deg/002_thr"
  # saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  # 
  # 
  # # "one is enough" binarization - constant threshold of 1%
  # pollen_r <- create_pollen_raster(year, window = 250, 
  #                                  field = field, dataset_count, 
  #                                  grid,
  #                                  method = "one_is_enough", threshold = 0.01)
  # pollen_df <- as.data.frame(pollen_r, xy = T)
  # names(pollen_df) <- c("lon", "lat", "pres")
  # pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  # output_folder <- "D:/species/pollen/processed/larix_decidua/025deg/001_thr"
  # saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  
  
  # relative abundance
  # pollen_r <- create_pollen_raster(year, window = 250, 
  #                                 field = field, dataset_count, 
  #                                 grid)
  # pollen_df <- as.data.frame(pollen_r, xy = T)
  # names(pollen_df) <- c("lon", "lat", "prop")
  # pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  # output_folder <- "D:/species/pollen/processed/fagus_sylvatica/025deg/prop"
  # saveRDS(pollen_df, file.path(output_folder, paste0("prop_",year, "BP", ".rds")))
}


# Keep only cells sample in each time interval (constant prevalence across time)
years <- seq(0, 15000, 500) 
input_folder <- "D:/species/pollen/processed/fagus_sylvatica/025deg/001_thr"
pollen_mask <- rast(readRDS(file.path(input_folder, paste0("pres_",max(years), "BP", ".rds"))))
pollen_mask <- rast(lapply(years, function(year) crop(rast(readRDS(file.path(input_folder, paste0("pres_",year, "BP", ".rds")))), ext(pollen_mask))))
pollen_mask <- sum(pollen_mask, na.rm = FALSE)

for(year in years){
  
  pollen_r <- mask(crop(rast(readRDS(file.path(input_folder, paste0("pres_",year, "BP", ".rds")))), ext(pollen_mask)), pollen_mask)
  pollen_df <- as.data.frame(pollen_r, xy = T)
  names(pollen_df) <- c("lon", "lat", "pres")
  pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  cat(paste0(nrow(pollen_df[pollen_df$pres == 1,]), " presences across ", nrow(pollen_df), " records\n"))
  output_folder <- "D:/species/pollen/processed/fagus_sylvatica/025deg/001_thr_cstprev15"
  saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  
}








# Particular case of Quercus deciduous and Quercus evergreen
library(plyr)

# combine csv files
pollen_counts <- lapply(list.files(file.path(wd, "quercus", "output"), pattern = "pollen_counts", full.names = T), function(x){
  data <- read.table(x, header = T, sep= "\t")
  return(data)
})
pollen_counts <- do.call(rbind.fill, pollen_counts)
pollen_counts[is.na(pollen_counts)] <- 0

# removing sequences with less thant 50 pollen grain counts
pollen_counts <- pollen_counts %>% dplyr::mutate(pollen_count = rowSums(dplyr::across(12:221))) %>%
  dplyr::filter(pollen_count >= 100) %>%
  dplyr::filter(maxAgeBP - minAgeBP <= 1000)

# Load metadata
metadata <- fread(file.path("D:/species/pollen/herzschuhetal2021", "Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))
names(metadata) <- c("datasetid", "loctype")

# Removing marine sites following Maguire et al. (2016)
pollen_counts <- left_join(pollen_counts, metadata) %>%
  dplyr::filter(loctype != "Marine" & loctype != "Lagoon")


field <- "Quercus" # Quercus = deciduous/ Quercus.evg.type = evergreen


# Find threshold with maximum relative abundance
years <- seq(0, 15000, 500) 
sp_threshold <- find_threshold_quercus(years, window = 250, field, dataset_count = pollen_counts, factor = 0.01)

# Loop on years
for(year in years){
  
  grid <- fread(paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/",ifelse(year==0,15,year),"BP/HadCM3B_Altitude.fit"))
  names(grid) <- c("lat", "lon", "alt")
  grid <- rast(grid[,c("lon", "lat", "alt")])
  
  # "one is enough" binarization - specific threshold
  pollen_r <- create_pollen_raster_quercus(year, window = 250, 
                                   field = field, pollen_counts, 
                                   grid,
                                   method = "one_is_enough", threshold = sp_threshold)
  pollen_df <- as.data.frame(pollen_r, xy = T)
  names(pollen_df) <- c("lon", "lat", "pres")
  pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  output_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/002adp_thr"
  saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  
  
  # "one is enough" binarization - constant threshold of 0.02
  pollen_r <- create_pollen_raster_quercus(year, window = 250, 
                                   field = field, pollen_counts, 
                                   grid,
                                   method = "one_is_enough", threshold = 0.02)
  pollen_df <- as.data.frame(pollen_r, xy = T)
  names(pollen_df) <- c("lon", "lat", "pres")
  pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  output_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/002_thr"
  saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  
  
  # "one is enough" binarization - constant threshold of 0.01
  pollen_r <- create_pollen_raster_quercus(year, window = 250, 
                                   field = field, pollen_counts, 
                                   grid,
                                   method = "one_is_enough", threshold = 0.01)
  pollen_df <- as.data.frame(pollen_r, xy = T)
  names(pollen_df) <- c("lon", "lat", "pres")
  pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  output_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/001_thr"
  saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  
  
  # relative abundance
  # pollen_r <- create_pollen_raster_quercus(year, window = 250, 
  #                                  field = field, pollen_counts, 
  #                                  grid)
  # pollen_df <- as.data.frame(pollen_r, xy = T)
  # names(pollen_df) <- c("lon", "lat", "prop")
  # pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  # output_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/prop"
  # saveRDS(pollen_df, file.path(output_folder, paste0("prop_",year, "BP", ".rds")))
}













