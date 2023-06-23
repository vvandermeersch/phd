
generate_radiation_PET <- function(tmin, tmax, cloud, pre, wind, alt, ratm, dayl, toa, airmass_data, peqin_data, thermo_data){
  
  cat("Computing global radiation and PET\n")
  
  # "identity" raster
  rid <- dayl 
  rid[!is.na(rid)] <- 1
  
  pet <- pet0 <- 0*rid
  
  rs <- .compute_global_radiation(ratm, alb, pre, pet, cloud, toa, dayl, airmass_data, peqin_data)
  
  pet <- .compute_pet(rs, cloud, wind, tmax, tmin, dayl, thermo_data)
  
  # J. Kaplan: "weak dependence between global radiation and PET, need to equilibrate them"
  k <- 0
  while(global(any(abs(pet - pet0) > 0.01), max, na.rm = TRUE)==1 & k < 100){
    
    pet0 <- pet
    rs <- .compute_global_radiation(ratm, alb, pre, pet, cloud, toa, dayl, airmass_data, peqin_data)
    pet <- .compute_pet(rs, cloud, wind, tmax, tmin, dayl, thermo_data)
    
    k <- k+1
    
  }
  cat(paste0("Number of iterations: ", k, "\n"))
  
  return(list(glo = rs, pet = pet))
  
}



.compute_global_radiation <- function(ratm, alb, pre, pet, cloud, toa, dayl, airmass_data, peqin_data){
  
  # parameters
  kp <- 0.5
  kag <- 3.3
  kan <- 2.32
  kn <- 0.686
  
  # "identity" raster
  rid <- dayl 
  rid[!is.na(rid)] <- 1
  
  # declare rasters
  t_cm <- peqin_data$t_cm
  peqin <- peqin_data$peqin
  mbar <- airmass_data$mbar
  mo <- airmass_data$mo
  mc <- airmass_data$mc
  ml <- airmass_data$ml
  
  sunf <- 1 - cloud
  
  # tropics indicator (tropical = 1, else 0)
  x <- rid*ifel(t_cm < 10, 0,
                ifel(t_cm>20, 1, sin(pi / 2 * (t_cm / 10 - 1))))
  
  # direct insolation atmospheric turbidity factor
  tau <- exp(-0.115 * ratm * ((2.15 - 0.713 * x + exp(-6.74 / (pre + 1))) * exp(0.0971 * pet) - 0.650 * (1 - x) * peqin))
  
  # atmospheric transmittance function
  fm <- 0.01452 * (mbar + ml) * exp(1.403 * tau) - 0.1528 * mo + mc + 0.48700 * (mc - ml) + 0.2323 
  
  # direct downwelling shortwave radiation
  direct <- sunf * tau**kp * toa * tau**fm
  
  # diffuse insolation atmospheric turbidity factor
  zeta0 <- 0.503 * exp(-1.20 * ratm * exp(-0.633 / (pre + 1) - 0.226 * pet))
  
  # diffuse downwelling shortwave radiation
  diffuse <- zeta0 * kag**alb * kan**(1 - sunf) * (1 - kn * (1 - sunf)) * (tau**kp * toa - direct)
  
  # global radiation
  glo <- (diffuse+direct)
  glo[glo<0] <- 0
  
  rs <- glo/1000 # convert to MJ/m2
  
  return(rs)
  
}



# function to compute PET (FAO standard Penman-Monteith equation)
.compute_pet <- function(rs, cloud, wind, tmax, tmin, dayl, thermo_data){
  
  # "identity" raster
  rid <- dayl 
  rid[!is.na(rid)] <- 1
  
  # Declare rasters
  # es <- thermo$es
  tdew <- thermo_data$tdew
  ss <- thermo_data$ss
  gamma <- thermo_data$gamma
  temp <- (tmin+tmax)/2
  
  # albedo FAO
  albFAO <- 0.23*rid 
  
  # wind speed
  z_meas <- 10
  u2 <- wind * 4.87/log(67.8 * z_meas - 5.42)
  
  # saturation vapour pressure
  es_Tmax <- 0.6108 * exp(17.27 * tmax/(tmax + 237.3))
  es_Tmin <- 0.6108 * exp(17.27 * tmin/(tmin + 237.3))
  es <- (es_Tmax + es_Tmin)/2
  
  # actual vapour pressure
  ea <- 0.6108 * exp((17.27 * tdew)/(tdew+237.3))
  
  # slope of vapour pressure curve (delta)
  ss <- ss/1000 # convert to kPa/C
  
  # psychrometric constant
  gamma <- gamma/1000 #convert to kPa/C
  
  # effect of cloudiness (relative shortwave radiation)
  rs_rs0 <- 1-0.29*(cloud + (cloud)^2) # Antoine et al, 1996
  # rs_rs0 <- 1 - cloud
  
  # estimated net outgoing longwave radiation
  r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) *
    ((tmax + 273.2)^4 + (tmin + 273.2)^4)/2 *
    (1.35 * rs_rs0 - 0.35)
  
  # net radiation
  r_ng <- (1 - albFAO) * rs  - r_nl
  
  # compute PET (Penman-Monteith equation) 
  pet <- (0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (temp + 273.15))/
    (ss + gamma * (1 + 0.34 * u2))
  pet[pet<0] <- 0
  
  return(pet)
  
}



