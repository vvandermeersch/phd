
##################################
#                                #
# PALEOCLIMATE GENERATION SCRIPT #
#                                #
##################################


# Author : V. Van der Meersch

message(paste0("Doing year ", years[2]-15))


########################
# 1. Weather generator #
########################

# Based on GWGEN (Sommer & Kaplan, 2017)
# Variables involved: temperatures, cloudiness, wind, precipitation and number of rainy days

# Write csv input file for GWGEN
if((years[2]-15) %in% seq(500, 20000, 500)){
  input_file <- write_gwgen_csv_2rasters(years, extent, raw_clim_dir, output_dir = file.path(wd, "inputs"), 
                                         WHC_present = WHC_present, debug_wet = TRUE, 
                                         debug_wndcld = TRUE, debug_years = FALSE, res = "10min")
}else{
  input_file <- write_dscl_gwgen_csv(years, extent, raw_clim_dir, output_dir = file.path(wd, "inputs"), 
                                WHC_present = WHC_present, debug_wet = TRUE, debug_wndcld = TRUE, debug_years = FALSE)
}




# Run GWGEN in parallel 
gwgen_file <- gwgen_in_parallel(input_file, wd, ncores = 30) 



############################
# 2. Radiation computation #
############################

# # If we had monthly clear-sky values:
# # Trigonometric interpolation of daily values from monthly values (clear-sky)
# rad_file <- interpolate_cs_radiation(years, extent, raw_clim_dir, output_dir = file.path(wd, "inputs"))
# # Cloudiness correction to infer global radiation
# glo_rad_file <- compute_glo_radiation(rad_file, gwgen_file, output_dir = file.path(wd, "outputs"))

# Compute daily TOA radiation
orbit_data <- fread(file.path(wd, "inputs", "orbit_parameters.csv"))
output_dir <- file.path(wd, "outputs")
ncores <- 10
source(file.path(wd, "scripts", "compute_toa_radiation.R")) # much more faster in a script rather than in a function...
# toa_file <- compute_toa_radiation(gwgen_file, input_file, output_dir = file.path(wd, "outputs"), orbit_data, ncores = 5)

# Compute daily global radiation (surface downwelling shortwave)
output_dir <- file.path(wd, "outputs")
pet_method <- "faoPM"
# options(future.globals.maxSize= 7000*1024^2)
source(file.path(wd, "scripts", "compute_glo_radiation.R")) # much more faster in a script rather than in a function...
# glo_file <- compute_glo_radiation_from_toa(toa_file, input_file, gwgen_file, elev_data = NULL, output_dir = file.path(wd, "outputs"), 
#                                            pet_method = "faoPM", ncores = 10)



################################
# 3. PHENOFIT-based formatting #
################################

mid_year <- (years[1]+years[2])/2
alt <- load_altitude_ICE6GC(year = mid_year, folder = "D:/climate/ICE-6G-C", folder_hadcm3b = "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw", extent)
phenofit_yr <- format_gwgen_to_phenofit(input_file, glo_file, pd_folder = out_clim_dir, debug_first_row = T, 
                                        alt= alt, WHC_present = WHC_present)



##################
# 4. Downscaling #
##################

# From 0.5 degree to 0.1 degree

# Custom method, inspired by Copernicus Climate Change Service, using altitude and the 8 neighbor cells
# Applied to temperatures and radiation
# alt_01deg <- fread("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_Altitude.fit")
# downscale_with_altitude(phenofit_yr, hr_alt = alt_01deg, in_folder = out_clim_dir, out_folder = ds_out_clim_dir, ncores = 20)
