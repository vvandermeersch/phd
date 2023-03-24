#########################################################################
# Script to format ERA5-Land climate data as required by Phenofit model #
#########################################################################

# author : V. Van der Meersch
# date : 16/02/2022

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/climate_data/format/phenofit/"


## Packages needed

library(ncdf4)
library(future)
library(future.apply)
library(dplyr)
library(raster)


## Functions needed

source(paste0(wd, 'functions/phenofit_formatting2.R')) # just to format variables for Phenofit, v2 with raster format (faster)
source(paste0(wd, 'functions/phenofit_processing_and_formatting2.R')) # to calculate supplementary variables and format them for Phenofit, v2 with raster format (faster)
source(paste0(wd, 'functions/phenofit_compute_PET2.R')) # to calculate evapotranspiration, v2 with different parallel architecture
source(paste0(wd, 'functions/phenofit_get_altitude.R')) # to get altitude
source(paste0(wd, 'functions/phenofit_get_WHC.R')) # to get WHC


## Settings

rawdata_folder <- "D:/climate/ERA5-Land/raw/"
processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/"


## Run

phenofit_formatting2(2001:2020, "2m_temperature", 'mean', rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting2(2001:2020, "2m_temperature", "min", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting2(2001:2020, "2m_temperature", "max", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting2(1950:1968, "total_precipitation", "sum", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting2(1950:1968, "surface_solar_radiation_downwards", "sum", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting2(1969:1999, "potential_evaporation", "sum", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_processing_and_formatting2(1950:1968, "wind", "mean", rawdata_folder, processeddata_folder, ncores=5) #used only for ETP calculation
gc()

phenofit_processing_and_formatting2(1950:1968, "relative_humidity", "mean", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_processing_and_formatting2(1950:1968, "relative_humidity", "min", rawdata_folder, processeddata_folder, ncores=5) #used only for ETP calculation
gc()

phenofit_processing_and_formatting2(1950:1968, "relative_humidity", "max", rawdata_folder, processeddata_folder, ncores=5) #used only for ETP calculation
gc()



## Altitude

tmin_file <- paste0(processeddata_folder, "ERA5LAND_", "tmn", "_", 1973, "_dly.fit")
tmin <- fread(tmin_file, showProgress=F)
loc_needed <- data.frame(tmin[,1], tmin[,2])
names(loc_needed) <- c("lat", "lon")

phenofit_get_altitude(loc = loc_needed, rawdata_folder, processeddata_folder)

rm(tmin, loc_needed)
gc()


## Evapotranspiration

system.time(phenofit_compute_PET2(2000, processeddata_folder, method = "PenmanMonteith", ncores=1))
gc()


phenofit_compute_PET2(1950:1968, processeddata_folder, method = "PenmanMonteith", ncores=2)


## Evapotranspiration without zero and NA

phenofit_compute_PET2(1969:1970, processeddata_folder, method = "PenmanMonteith", ncores=2, transform_NA_and_zero=T)



## WHC

phenofit_get_WHC(rawdata_folder, processeddata_folder)





