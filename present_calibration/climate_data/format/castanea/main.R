#########################################################################
# Script to format ERA5-Land climate data as required by Castanea model #
#########################################################################

# author : V. Van der Meersch
# date : 16/02/2022

# in order to increase computing speed and avoid unnecessary calculations (hourly data -> daily data)
# this script use pre-processed daily Phenofit4 climate files (see ~climate_data/format/phenofit)

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/present_calibration/climate_data/format/castanea/"


## Packages needed

library(future)
library(future.apply)
library(data.table)
options(future.globals.maxSize= 500*1024^2)
library(progressr)

## Functions needed

# source(paste0(wd, 'functions/format_climate.R')) #old version
source(paste0(wd, 'functions/format_climate_2.R'))


## Settings 

phenofit_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed/"
processeddata_folder <- "D:/climate/ERA5-Land/castanea_format3/"


## Run

# old version
# format_climate(years = 1969:2000, out_folder = processeddata_folder, source_folder = phenofit_folder, ncores = 12)
# long runtime despite parallelisation, 
# code might not be optimal...

# second version with a different memory usage and parallel file reading strategy (faster ?)
format_climate_2(years = 1969, ncells = 101510, out_folder = processeddata_folder, source_folder = phenofit_folder, ncores = 20)
for(yr in 1970:2000){
  format_climate_2(years = yr, ncells = 101510, out_folder = processeddata_folder, source_folder = phenofit_folder, ncores = 20, create_files = FALSE)
}





