
################################
#                              #
# PALEOCLIMATE DATA GENERATION #
#                              #
################################


# Author : V. Van der Meersch

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate"


# Load libraries and functions
source(file.path(wd, "scripts", "setup.R"))

# Setup folders
raw_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw_dscl_15min" # folder with HadCM3B datafiles
out_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min" # output folder, 10min


# Years to compute in BP (BP = before present, pre-industrial, i.e. 1950)
years_to_compute <- seq(250, 18000, 250)

years_to_compute <- c(40)

# Choose extent
extent <- ext(c(-10,35,36,71))

for(year in years_to_compute){
  
  years <- c(year -9, year + 10) # 31-year interval
  
  # Write CSV file as required by GWGEN
  input_file <- write_gwgen_csv(years, extent, raw_folder, output_dir = file.path(wd, "inputs"),
                                WHC_present = WHC_present,
                                debug_wet = TRUE, debug_wndcld = TRUE)

  # Run GWGEN in parallel
  gwgen_file <- gwgen_in_parallel(input_file, wd, ncores = 30, dir_gwgen)
  
  input_file <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/inputs/",
                       years[1], "_", years[2],
                       "BP_gwgen.csv")
  gwgen_file <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/outputs/",
                       years[1], "_", years[2],
                       "BP_gwgen_out.csv")

  # Calculate radiation, PET, and format file
  source(file.path(wd, "scripts", "compute_format_climate.R"))
  
  gc(verbose = FALSE)
}







