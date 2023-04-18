# function to write CSV in the format required by GWGEN
# adapted to match with 10min downscaled netcdf format
# return output file name

# author : V. Van der Meersch - 18/01/2022

# debug option (strange wet days without precipitation ?)
# + some wind and cloud data are missing (in rare case)

write_gwgen_csv <- function(years, extent, source_dir, output_dir, 
                                 WHC_present, debug_wet = F, debug_wndcld = F){
  
  # get file spec
  filename <- paste0(years[2], "_", years[1], "kyr")
  outname <-   ifelse(!is.na(years [2]) ,paste0(years[1], "_",years[2],"BP"), paste0(years[1], "_BP"))
  
  # write header as require by gwgen
  header <- c("station id", "lon", "lat", "year", "month", "min.temperature", "max.temperature",
              "cloud fraction", "wind speed", "precipitation", "wet")
  write.table(t(header), file = file.path(output_dir, paste0(outname, "_gwgen.csv")), col.names = FALSE, row.names = FALSE, sep=",")
  
  # load raster files
  cat("Loading raster files \n")
  r_tmin <- rast(file.path(source_dir, "tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                           paste0("tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", filename, ".nc")),
                 subds = "tempmin_av", opts="HONOUR_VALID_RANGE=NO")
  r_tmax <- rast(file.path(source_dir, "tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                           paste0("tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", filename, ".nc")),
                 subds = "tempmax_av", opts="HONOUR_VALID_RANGE=NO")
  r_pre <- rast(file.path(source_dir, "precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", filename, ".nc")),
                subds = "precip_mm_srf", opts="HONOUR_VALID_RANGE=NO")
  r_wet <- rast(file.path(source_dir, "rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", filename, ".nc")),
                subds = "rd3_mm_srf", opts="HONOUR_VALID_RANGE=NO")
  r_cloud <- rast(file.path(source_dir, "totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                            paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", filename, ".nc")),
                  subds = "totCloud_mm_ua", opts="HONOUR_VALID_RANGE=NO")
  r_uwind <- rast(file.path(source_dir, "u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                            paste0("u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", filename, ".nc")),
                  subds = "u_mm_10m", opts="HONOUR_VALID_RANGE=NO")
  r_vwind <- rast(file.path(source_dir, "v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                            paste0("v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", filename, ".nc")),
                  subds = "v_mm_10m", opts="HONOUR_VALID_RANGE=NO")
  
  # crop
  cat("Cropping raster files \n")
  r_tmin <- crop(r_tmin, extent)
  r_tmax <- crop(r_tmax, extent)
  r_pre <- crop(r_pre, extent)
  r_wet <- crop(r_wet, extent)
  r_cloud <- crop(r_cloud, extent)
  r_uwind <- crop(r_uwind, extent)
  r_vwind <- crop(r_uwind, extent)
  
  
  cat(paste0("Number of cells: ", ncell(r_tmin), "\n"))
  cat(paste0("Number of NA cells: ", freq(r_tmin, value=NA)$count[1], "\n"))
  
  tmin <- as.vector(r_tmin)
  tmax <- as.vector(r_tmax)
  pre <- as.vector(r_pre)
  wet <- as.vector(r_wet)
  cloud <- as.vector(r_cloud)
  uwind <- as.vector(r_uwind)
  vwind <- as.vector(r_vwind)
  wind <- sqrt(uwind^2 + vwind^2)
  
  id <- rep(1:ncell(r_tmin), dim(r_tmin)[3])
  lon <- rep(unlist(lapply(1:ncell(r_tmin), function(i)xFromCell(r_tmin, i))), dim(r_tmin)[3])
  lat <- rep(unlist(lapply(1:ncell(r_tmin), function(i)yFromCell(r_tmin, i))), dim(r_tmin)[3])
  month <- rep(unlist(lapply(1:12, function(i)rep(i,ncell(r_tmin)))), length(years[1]:years[2]))
  yr <- unlist(lapply(years[2]:years[1], function(i)rep(i,ncell(r_tmin)*12))) # time goes forward

  data <- data.frame(id = id, lon = lon, lat = lat, yr = yr, month = month, 
                     tmin = round(tmin, 4), tmax = round(tmax, 4), 
                     cloud = round(cloud, 4), wind = round(wind, 4), 
                     pre = round(pre*30, 4), wetd = round(wet))
  data <- data[!is.na(data$tmin),]
  data <- data[with(data, order(id, yr, month)),]
  
  # check if some rare points miss data (landmask may change bewteen year-15 and year +15), and remove them
  nobs <- data %>% group_by(id) %>% count()
  nobs_max <- max(nobs$n)
  nobs_incomplete <- nobs %>% filter(n != nobs_max)
  cat(paste0(nrow(nobs_incomplete), " cells are incomplete.\n"))
  data <- data %>% filter(!(id %in% nobs_incomplete$id))
  
  
  cat(paste0("Number of cells with data: ", nrow(data)/dim(r_tmin)[3], "\n"))
  
  if(debug_wet){
    data[data$pre >= 1 & data$pre <= 2, "wetd"] <- 1
    data[data$pre < 1, "wetd"] <- 0
    data[data$pre < 1, "pre"] <- 0
    data[data$pre > 2 & data$wetd == 0, "wetd"] <- 1
  }
  
  if(debug_wndcld){
    data <- na.locf(data, na.rm=FALSE) # carry the last observation forward to replace rare missing wind/cloud values
    data[data$cloud > 1, "cloud"] <- 1
  }
  
  fwrite(data, file = file.path(output_dir, paste0(outname, "_gwgen.csv")), 
         append = T, col.names = FALSE, row.names = FALSE, sep=",")
  
  message("CSV file created !")
  
  return(file.path(output_dir, paste0(outname, "_gwgen.csv")))
  
  
  
  
  
}
