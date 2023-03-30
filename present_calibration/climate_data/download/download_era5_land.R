library(reticulate)
library(future)
library(future.apply)

setwd("D:/climate/ERA5-Land/raw") # folder where files will be downloaded

# Python config
use_python("C:/Users/vandermeersch/AppData/Local/Programs/Python/Python310/python.exe", required=T)
cdsapi <- import("cdsapi")
source_python("C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/climate_data/download/ERA5_land_download_2.py") # custom script


# select the variable; name must be a valid ERA5 CDS API name
var <- 'surface_solar_radiation_downwards'

# For valid keywords, see Table 2 of:
# https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation#ERA5Land:datadocumentation-parameterlistingParameterlistings




# Select years
years <- as.character(1950:1968)

# Maximum number of per-user requests that access the online CDS data is 2
# plan(multisession, workers = 2) ?!
# But Python from reticulate cannot run in parallel because of external pointers !
# We have to launch manually 2 R sessions
years_1 <- years[1:(length(years)%/%2)]
years_2 <- years[(length(years)%/%2+1):length(years)]

#Run script
era5_land(years = as.character(2016:2017), var = var)



