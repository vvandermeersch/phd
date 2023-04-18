
################################
#                              #
# PALEOCLIMATE DATA GENERATION #
#                              #
################################


# Author : V. Van der Meersch

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate"


# Load libraries and functions
source(file.path(wd, "scripts", "setup.R"))
options(future.globals.maxSize= 850*1024^2)


# Setup folders
raw_clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw_dscl_15min" # folder with HadCM3B datafiles
out_clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min" # output folder, 10min


# Year interval in BP (BP = before present, pre-industrial, i.e. 1950)
# years <- c(985, 1015)

# Choose extent
#extent <- ext(c(-10,40,34,71))
extent <- ext(c(-10,35,36,71))

for(year in c(7500,8000)){
  years <- c(year -15, year + 15)
  try(source(file.path(wd, "scripts", "paleoclimate_main.R")))
  gc()
}



# create additional variable (for diagnosis)
for(year in seq(17000, 1000, -2000)){
  yrb <- year - 15
  yre <- year + 15
  input_file <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/inputs/",
                       yrb, "_", yre,
                       "BP_gwgen.csv")
  gwgen_file <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/outputs/",
                       yrb, "_", yre,
                       "BP_gwgen_out.csv")
  toa_file <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/outputs/",
                       yrb, "_", yre,
                       "BP_toarad.csv")
  glo_file <- paste0("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/outputs/",
                     yrb, "_", yre,
                     "BP_glorad.csv")
  # format_gwgen_to_phenofit_addvar(input_file, toa_file, addvar = "TOA", 
  #                                 pd_folder = out_clim_dir, debug_first_row = FALSE, WHC_present = WHC_present)
  format_gwgen_to_phenofit_addvar(input_file, glo_file, addvar = "cld", 
                                  pd_folder = out_clim_dir, debug_first_row = FALSE, WHC_present = WHC_present)
  format_gwgen_to_phenofit_addvar(input_file, glo_file, addvar = "wind", 
                                  pd_folder = out_clim_dir, debug_first_row = FALSE, WHC_present = WHC_present)
  # format_gwgen_to_phenofit_addvar(input_file, toa_file, addvar = "mbar", pd_folder = out_clim_dir, debug_first_row = FALSE)
  
}




