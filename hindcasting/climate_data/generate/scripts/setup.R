# R packages
library(terra)
library(data.table)
library("doFuture")
registerDoFuture()
library(future.apply)
library(dplyr)
library(zoo)
library(progressr)

options(future.globals.maxSize= 850*1024^2)

# Custom functions
source(file.path(wd, "functions", "gwgen_in_parallel.R"))
source(file.path(wd, "functions", "write_gwgen_csv.R"))
source(file.path(wd, "functions", "format_phenofit.R"))
source(file.path(wd, "functions", "compute_toa_radiation.R"))
source(file.path(wd, "functions", "compute_peqin.R"))
source(file.path(wd, "functions", "compute_airmass.R"))
source(file.path(wd, "functions", "compute_thermo_variables.R"))
source(file.path(wd, "functions", "generate_radiation_PET.R"))


WHC_present <- fread("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_WHC.fit")
names(WHC_present) <- c("lat", "lon", "whc")

dir_gwgen <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/scripts/daily_weather_generator"
