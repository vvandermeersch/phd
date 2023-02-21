#########################################################################
# Script to format ERA5-Land climate data as required by Forceeps model #
#########################################################################

# author : V. Van der Meersch
# date : 16/02/2022

# in order to increase computing speed and avoid unnecessary calculations (hourly data -> monthly data)
# this script use pre-processed daily Phenofit4 climate files (see ~climate_data/format/phenofit)

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/climate_data/format/forceeps/"


## Packages needed

library(future)
library(future.apply)
library(data.table)
options(future.globals.maxSize= 500*1024^2)
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")
library(dplyr)

## Functions needed

# source(paste0(wd, 'functions/format_climate.R')) #old version
source(paste0(wd, 'functions/format_forceeps_climate.R'))
source(paste0(wd, 'functions/create_forceeps_sites.R'))


## Settings 

phenofit_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed/"
processeddata_folder <- "D:/climate/ERA5-Land/forceeps_format/"


## Run

format_forceeps_climate(years = 1969, ncells = 101510, out_folder = processeddata_folder, source_folder = phenofit_folder, ncores = 20)
for(yr in 1981:2000){
  format_forceeps_climate(years = yr, ncells = 101510, out_folder = processeddata_folder, source_folder = phenofit_folder, ncores = 20, create_files = FALSE)
}

alt_file <- paste0(phenofit_folder, "ERA5LAND_Altitude.fit")
alt <- fread(alt_file, showProgress=F, nrows = 101510)
names(alt) <- c("lat", "lon", "alt")
lat <- alt$lat

site_data <- list(lat = lat, bucket_size = rep(20, 101510), nitrogen = rep(100, 101510),
                  reg_forclim_density = rep(0.006, 101510), max_ETP_rate = rep(12, 101510),
                  fraction_interc_prec = rep(0.3, 101510))

create_forceeps_sites(ncells = 101510, out_folder = processeddata_folder, source_folder = phenofit_folder, data = site_data)




