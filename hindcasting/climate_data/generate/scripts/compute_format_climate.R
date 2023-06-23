
# Load data
orbit_data <- fread(file.path(wd, "inputs", "orbit_parameters.csv"))
gwgen_data <- data.frame(fread(gwgen_file))
input_data <- data.frame(fread(input_file))

time_slice <- paste0(years[2], "_", years[1], "kyr")

# Add latitude and longitude
latlon_data <- unique(input_data[,c("station.id", "lat", "lon")])
names(latlon_data) <- c("id", "lat", "lon")

# Calculate DOY
list_ids <- unique(gwgen_data$id)
temp <- data.frame(gwgen_data[gwgen_data$id == list_ids[1],])
doy_temp <- sapply(1:nrow(temp), function(i){
  if(temp[i, "month"] == 1){
    return(temp[i, "day"])
  }else{
    return(temp[i, "day"] + sum(sapply(1:(temp[i, "month"]-1), function(m) max(temp[temp$year == temp[i, "year"] & temp$month == m, "day"]))))
  }
})
gwgen_data$doy <- rep(doy_temp, length(unique(gwgen_data$id)))
rm(temp, doy_temp, list_ids)
gwgen_data <- left_join(gwgen_data[,c("id", "year", "doy", "tmin", "tmax", "mean_cloud", "prcp", "wind")], latlon_data)
rm(latlon_data)


# Loop on years
for(yr in years[2]:years[1]){
  start_time <- Sys.time()
  extent <- ext(c(-10,35,36,71))
  
  cat(paste0("Processing year ", yr,"\n"))
  gwgen_df <- data.frame(gwgen_data[gwgen_data$year == yr,])
  input_df <- input_data[input_data$year == yr,]
  
  # Load orbital parameters
  orbit <- load_orbit_parameters(yr, orbit_data)
  
  # Declare rasters
  tmin <- rast(lapply(unique(gwgen_df$doy), function(d)rast(gwgen_df[gwgen_df$doy == d,c("lon", "lat", "tmin")])))
  tmax <- rast(lapply(unique(gwgen_df$doy), function(d)rast(gwgen_df[gwgen_df$doy == d,c("lon", "lat", "tmax")])))
  cloud <- rast(lapply(unique(gwgen_df$doy), function(d)rast(gwgen_df[gwgen_df$doy == d,c("lon", "lat", "mean_cloud")])))
  pre <- rast(lapply(unique(gwgen_df$doy), function(d)rast(gwgen_df[gwgen_df$doy == d,c("lon", "lat", "prcp")])))
  wind <- rast(lapply(unique(gwgen_df$doy), function(d)rast(gwgen_df[gwgen_df$doy == d,c("lon", "lat", "wind")])))
  doy <- rast(lapply(unique(gwgen_df$doy), function(d) rast(gwgen_df[gwgen_df$doy == d,c("lon", "lat", "doy")])))
  lat <- rast(gwgen_df[gwgen_df$doy == 1,c("lon", "lat", "lat")])
  
  # Load altitude
  alt <- rast(file.path(raw_folder, "altitude_ICE6GC_15minRes_Europe_24000_0kyr",
                        paste0("altitude_ICE6GC_15minRes_Europe_", time_slice, ".nc")))
  alt <- resample(alt, tmin) # just to have exactly the same extent
  
  # Load WHC
  whc <- rast(file.path(raw_folder, "WHC_present_15minRes_Europe_24000_0kyr",
                        paste0("WHC_present_15minRes_Europe_", time_slice, ".nc")))
  whc <- resample(whc, tmin) # just to have exactly the same extent
  
  # Relative atmospheric pressure
  z0 <- 1/8000
  ratm <- exp(-alt * z0)
  
  # Load albedo
  ind <- which(years[2]:years[1] == yr) # index of year
  ind <- 1
  albedo <- rast(file.path(raw_folder, "albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                           paste0("albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", time_slice, ".nc")))
  alb <- subset(albedo, (1+(ind-1)*12):(1+(ind-1)*12+11)) # keep only the months of the year considered
  alb <- resample(alb, tmin) # just to have exactly the same extent
  # repeat over days
  if(max(unique(gwgen_df$doy)) == 366){
    ndays <- c(31,29,31,30,31,30,31,31,30,31,30,31)
    alb <- rast(lapply(1:12, function(m) return(rep(subset(alb, m), ndays[m]))))
  }else{
    ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    alb <- rast(lapply(1:12, function(m) return(rep(subset(alb, m), ndays[m]))))
  }
  
  # Compute TOA radiation
  toa_data <- compute_toa_radiation(doy, lat, orbit)
  gc(verbose = FALSE)
  
  # Declare rasters
  dayl <- toa_data$dayl
  delta <- toa_data$delta
  toa <- toa_data$toa
  
  # Compute "precipitation equitability index", used thereafter to compute global radiation
  peqin_data <- compute_peqin(input_df)
  gc(verbose = FALSE)
  
  # Compute daily airmass
  airmass_data <- compute_airmass(dayl, lat, delta, ratm)
  gc(verbose = FALSE)
  
  # Compute thermodynamic-related variables
  thermo_data <- compute_thermo_variables(tmin, tmax)
  gc(verbose = FALSE)
  
  # Compute global radiation and PET
  glopet_data <- generate_radiation_PET(tmin, tmax, cloud, pre, wind, alt, ratm, dayl, toa, airmass_data, peqin_data, thermo_data)
  gc(verbose = FALSE)
  
  # Declare rasters
  glo <- glopet_data$glo
  pet <- glopet_data$pet
  tdew <- thermo_data$tdew
  
  # Write in PHENOFIT format
  out_folder_yr <- file.path(out_folder, paste0(mean(years), "BP"))
  dir.create(out_folder_yr, showWarnings = FALSE)
  format_phenofit(yr, tmin, tmax, pre, pet, glo, wind, tdew, alt, whc, out_folder_yr)
  
  end_time <- Sys.time()
  cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
}
