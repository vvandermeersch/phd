# get years
years <- rev(unique(gwgen_data$year))

cat("Processing data\n")
# get lat/lon of station
latlon_data <- unique(input_data[,c("station id", "lat", "lon")])
names(latlon_data) <- c("id", "lat", "lon")

# Calculate DOY
# out_data$doy <- yday(as.Date(paste(1950-out_data$year, out_data$month, out_data$day, sep ="-"), format = "%y-%m-%d")) # does not work BCE
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

dayl_r <- rast()
toarad_r <- rast()
delta_r <- rast()

cat("Computing TOA radiation\n")
start_time <- Sys.time()
for(yr in years){
  cat(paste0(yr, "\n"))
  
  gwgen_data_yr <- left_join(gwgen_data[gwgen_data$year == yr,c("id", "doy")], latlon_data)
  doy <- rast(lapply(unique(gwgen_data_yr$doy), function(d) rast(gwgen_data_yr[gwgen_data_yr$doy == d,c("lon", "lat", "doy")])))
  lat <- rast(gwgen_data_yr[gwgen_data_yr$doy == 1,c("lon", "lat", "lat")])
  orbit <- load_orbit_parameters(yr, orbit_data)
  
  pir <- pi/180
  
  # parameters
  ss <- 1361
  tau <- 86.4
  step <- 360/365.25
  
  # set orbital parameter values
  ecc  <- orbit$ecc
  pre  <- orbit$pre
  perh <- calculate_perh(ecc, pre)
  xob  <- orbit$xob
  
  
  sf  <- tau * ss / pi
  so  <- sin(xob * pir)
  xl  <- perh + 180
  
  xllp <- xl * pir
  xee  <- ecc * ecc
  xse  <- sqrt(1 - xee)
  xlam <- (ecc / 2 + ecc * xee / 8) * (1 + xse) * sin(xllp) - xee / 4 * (0.5 + xse) * 
    sin(2 * xllp) + ecc * xee / 8 * (1 / 3 + xse) * sin(3 * xllp)
  
  xlam  <- 2 * xlam / pir
  dlamm <- xlam + (doy - 80) * step
  anm   <- dlamm - xl
  
  ranm <- anm * pir
  xec  <- xee * ecc
  
  ranv <- ranm + (2 * ecc - xec/4) * sin(ranm) + 5/4 * ecc**2 * sin(2 * ranm) +
    13/12 * xec * sin(3 * ranm)
  
  anv  <- ranv / pir
  tls  <- anv + xl
  
  dlam <- tls
  
  rphi <-  lat * pir
  ranv <-  (dlam - xl) * pir
  rau <-  (1 - ecc * ecc) / (1 + ecc * cos(ranv))
  
  s <- sf / rau / rau
  rlam <- dlam * pir
  sd <- so * sin(rlam)
  cd <- sqrt(1 - sd * sd)
  
  rdelta <- atan(sd / cd)
  delta <- rdelta / pir
  spa <- sd * sin(rphi)
  
  cp <- cd * cos(rphi)
  aphi <- abs(lat)
  adelta <- abs(delta)
  
  dayl <- ifel(tt <= 1e-8 & adelta <= 1e-8, 0,
               ifel(adelta <= 1e-8, 12,
                    ifel(aphi <= 1e-8, 12, 
                         ifel(aphi < (90 - adelta), 24 * acos(-spa / cp) / pi,
                              ifel((lat * delta) > 0, 24,
                                   ifel((lat * delta) < 0, 0, 24 * acos(-spa / cp) / pi))))))
  
  toarad <- ifel(tt <= 1e-8 & adelta <= 1e-8, 0,
               ifel(adelta <= 1e-8, s * cos(rphi),
                    ifel(aphi <= 1e-8, s * cos(rdelta), 
                         ifel(aphi < (90 - adelta), s * (acos(-spa / cp) * spa + cp * sqrt(1 - (-spa / cp) * (-spa / cp))),
                              ifel((lat * delta) > 0, s * spa * pi,
                                   ifel((lat * delta) < 0, 0, s * (acos(-spa / cp) * spa + cp * sqrt(1 - (-spa / cp) * (-spa / cp)))))))))
  
  delta_r <- c(delta_r, delta)
  dayl_r <- c(dayl_r, dayl)
  toarad_r <- c(toarad_r, toarad)
  
  print(max(unique(gwgen_data_yr$doy)))
  print(dim(toarad[3]))
  
}
end_time <- Sys.time()
cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))

cat("Writing final rasters\n")
start_time <- Sys.time()
time_slice <- paste0(max(years), "_", min(years), "kyr")
writeCDF(delta_r, file.path(out_folder, "processed_delta_daily_15minRes_Europe_24000_0kyr",
                              paste0("processed_delta_daily_15minRes_Europe_", time_slice, ".nc")),
         varname = "delta", overwrite=TRUE)
writeCDF(dayl_r, file.path(out_folder, "processed_dayl_daily_15minRes_Europe_24000_0kyr",
                              paste0("processed_dayl_daily_15minRes_Europe_", time_slice, ".nc")),
         varname = "dayl", overwrite=TRUE)
writeCDF(toarad_r, file.path(out_folder, "processed_toarad_daily_15minRes_Europe_24000_0kyr",
                              paste0("processed_toarad_daily_15minRes_Europe_", time_slice, ".nc")),
         varname = "toarad", overwrite=TRUE)
end_time <- Sys.time()
cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
























