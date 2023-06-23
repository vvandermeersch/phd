#########################################################################
# Script to format ERA5-Land climate data as required by Castanea model #
#########################################################################

# author : V. Van der Meersch
# date : 16/02/2022

# in order to increase computing speed and avoid unnecessary calculations (hourly data -> daily data)
# this script use pre-processed daily Phenofit4 climate files (see ~climate_data/format/phenofit)

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/format/castanea/"

## Packages needed

library(future)
library(future.apply)
library(data.table)
options(future.globals.maxSize= 500*1024^2)
library(progressr)

## Functions needed

# source(paste0(wd, 'functions/format_climate.R')) #old version
source(paste0(wd, 'functions/format_climate_2.R'))


## Loop on year
for(year in seq(1000,12000,500)){
  
  phenofit_folder <- paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/",year,"BP/")
  processeddata_folder <- paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/castanea_format/025deg/",year,"BP/")
  
  tmp_file <- paste0(phenofit_folder, "HadCM3B_", "tmp", "_", -year, "_dly.fit")
  temp <- fread(tmp_file, showProgress=F, skip = 4)
  ncells <- nrow(temp)
  
  dir.create(path = processeddata_folder, showWarnings = FALSE)
  
  # second version with a different memory usage and parallel file reading strategy (faster ?)
  format_climate_2(years = -year-15, ncells = ncells, out_folder = processeddata_folder, source_folder = phenofit_folder, ncores = 6)
  for(yr in (-year-14):(-year+15)){
    format_climate_2(years = yr, ncells = ncells, out_folder = processeddata_folder, source_folder = phenofit_folder, ncores = 6, create_files = FALSE)
  }
  
  
}







