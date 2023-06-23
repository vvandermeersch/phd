
#-------------------------------------------------#
# Script to format variables as required by cSDMs #
#-------------------------------------------------#

# input data : climate variable in Phenofit format

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/format"

# Settings
phenofitdata_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min"
bioclimdata_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/bioclim_format/dscl_15min"
csdm_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/dscl_15min"

library(future.apply)
library(abind)
library(data.table)
library(dplyr)
library(terra)

# Functions
source(file.path(wd, 'functions','compute_biovars.R')) # to compute bioclim variables from Phenofit daily variables

# Choose extent
extent <- ext(c(-10,35,36,71))

# Soil predictors (from present data)
pred_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors"
load(file.path(pred_dir, "predictors_data.Rdata"))
soil_pred <- rast(lapply(c("WHC", "pH"), 
                         function(i) rast(predictors_data %>% 
                                            dplyr::select(all_of(c("lon", "lat", i))), crs = "EPSG:4326")))

# Run script
for(year in seq(14250, 18000, 250)){
  years <- -c(year -15, year + 15)
  try(source(file.path(wd, "scripts", "compute_predictors.R")))
  gc()
}




