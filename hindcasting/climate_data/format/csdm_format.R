
#-------------------------------------------------#
# Script to format variables as required by cSDMs #
#-------------------------------------------------#

# input data : climate variable in Phenofit format

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/format"

# Settings
phenofitdata_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/05deg"
bioclimdata_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/bioclim_format"
csdm_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"

library(future.apply)
library(abind)
library(data.table)
library(dplyr)

# Functions
source(file.path(wd, 'functions','compute_biovars.R')) # to compute bioclim variables from Phenofit daily variables

# Run script
for(year in c(5000, 5500, 6000)){
  years <- -c(year -15, year + 15)
  try(source(file.path(wd, "scripts", "compute_predictors.R")))
  gc()
}

# Soil predictors (from present data)
r_res <- rast(predictors_data[,c("lon", "lat")])
pred_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors"
load(file.path(pred_dir, "predictors_data.Rdata"))
soil_predictors <- predictors_data %>% 
  dplyr::select(all_of(c("lat", "lon", "WHC", "pH")))
r_pred <- rast(lapply(c("WHC", "pH"), 
                      function(i) rast(predictors_data %>% 
                                         dplyr::select(all_of(c("lon", "lat", i))), crs = "EPSG:4326")))
r_pred <- terra::resample(r_pred, r_res, method = "average")
soil_predictors <- as.data.frame(r_pred, xy = T)
names(soil_predictors)[1:2] <- c("lon", "lat")
saveRDS(soil_predictors, file.path(csdm_folder, "soil_predictors.rds"))
