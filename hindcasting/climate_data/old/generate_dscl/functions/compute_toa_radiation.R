# function to compute daily TOA radiation
# given a day of the year and the latitude

# this code is largely inspired (or shamefully copied ?) from J. Kaplan code available on GitHub: https://github.com/ARVE-Research/LPJ-LMfire
# as stipulated in Kaplan code, this method comes from Berger (1978), and is valid only "within +- 1.000.000 yr centered on 1950 AD"

# orbital parameters are transient and come from E. Amstrong dataset boundary conditions (we do not calculate them as J. Kaplan)

# solar "constant" is set to 1361+/-0.5 as in PMIP4

# adapted by V. Van der Meersch - 12/12/2022

calculate_toa <- function(orbit, doy, lat){
  
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
  
  #particular cases for lat = 0 or delta = 0
  tt <- abs(aphi - 90)
  if (tt <= 1e-8 & adelta <= 1e-8){
    
    dayl <- 0
    toa_rad <- 0
    
  } else if(adelta <= 1e-8){
    
    dayl <- 12
    toa_rad <- s * cos(rphi)
    
  } else if(aphi <= 1e-8){
    
    dayl <- 12
    toa_rad <- s * cos(rdelta)
    
  } else{
    
    at <- 90 - adelta
    spd <- lat * delta
    
    if(aphi < at){
      
      tp <- -spa / cp
      stp <- sqrt(1 - tp * tp)
      rdayl <- acos(tp)
      dayl <- 24 * rdayl / pi
      toa_rad <- s * (rdayl * spa + cp * stp)
      
    } else if(spd > 0){
      
      dayl <- 24
      toa_rad <- s * spa * pi
      
    } else if(spd < 0){
      
      dayl <- 0
      toa_rad <- 0
      
    } else{
      
      tp <- -spa /cp
      stp <- sqrt(1 - tp * tp)
      rdayl <- acos(tp)
      dayl <- 24 * rdayl / pi
      toa_rad <- s * (rdayl * spa + cp * stp)
    }
    
  }
  
  #return(list(toa_rad = toa_rad, dayl = dayl, delta = delta, s = s, rdayl = rdayl, spa = spa, cp = cp, stp = stp))
  return(list(toa_rad = toa_rad, dayl = dayl, delta = delta))  
  
}



compute_toa_radiation <- function(gwgen_file, input_file, output_dir, orbit_data, ncores = 1){
  
  output_file <- file.path(output_dir,
                           paste0(strsplit(basename(gwgen_file), "_")[[1]][1], 
                                  "_", strsplit(basename(gwgen_file), "_")[[1]][2], "_toarad.csv"))
  
  # read data
  gwgen_data <- fread(gwgen_file)
  input_data <- fread(input_file)
  
  # get latitude of station
  lat_data <- unique(input_data[,c("station id", "lat")])
  names(lat_data) <- c("id", "lat")
  
  # prepare output
  out_data <- left_join(data.frame(id = gwgen_data[, id]), lat_data)
  out_data <- cbind(out_data, gwgen_data[, c("year", "month", "day")])
  # Calcultate DOY
  # out_data$doy <- yday(as.Date(paste(1950-out_data$year, out_data$month, out_data$day, sep ="-"), format = "%y-%m-%d")) # does not work BCE
  temp <- out_data[out_data$id == 60,]
  doy_temp <- sapply(1:nrow(temp), function(i){
    if(temp[i, "month"] == 1){
      return(temp[i, "day"])
    }else{
      return(temp[i, "day"] + sum(sapply(1:(temp[i, "month"]-1), function(m) max(temp[temp$year == temp[i, "year"] & temp$month == m, "day"]))))
    }
  })
  out_data$doy <- rep(doy_temp, length(unique(out_data$id)))
  rm(temp, doy_temp)
  gc()
  
  # compute
  for(yr in unique(out_data$year)){
    cat(paste0("Year ", yr, "\n"))
    
    orbit <- load_orbit_parameters(yr, orbit_data)
    
    out_data_temp <- out_data[out_data$year == yr,]
    
    plan(multisession, workers = ncores)
    
    runt <- system.time(toa <- future_lapply(1:nrow(out_data_temp), function(i){
      calculate_toa(orbit, out_data_temp[i, "doy"], out_data_temp[i, "lat"])
    }))
    
    plan(sequential)
    
    cat(paste0("Runtime: ", runt[3], "s \n"))
    
    toa <- do.call(rbind, toa)
    
    out_data[out_data$year == yr, "toa"] <- unlist(toa[,1])
    out_data[out_data$year == yr, "dayl"] <- unlist(toa[,2])
    out_data[out_data$year == yr, "delta"] <- unlist(toa[,3])
    
    gc()
    
  }

  fwrite(out_data, file = output_file, col.names = T, row.names = FALSE, sep=",")
  
  message("Daily TOA radiation computed !")
  
  return(output_file)
  
}



load_orbit_parameters <- function(yr, orbit_data){
  
  i <- 1
  while(abs(yr-1950) > (orbit_data[i, "yr_kyr"]*1000+500)){
    i <- i + 1
  }
  
  return(list(ecc = as.numeric(orbit_data[i, "eccentricity"]), 
              pre = as.numeric(orbit_data[i, "precession"]), 
              xob = as.numeric(orbit_data[i, "obliquity"])))

}
