compute_toa_radiation <- function(doy, lat, orbit){
  
  cat("Computing TOA radiation\n")
  
  # parameters
  pir <- pi/180
  ss <- 1361
  tau <- 86.4
  step <- 360/365.25
  
  # set orbital parameter values
  ecc  <- orbit$ecc
  pre  <- orbit$pre
  perh <- .calculate_perh(ecc, pre)
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
  
  tt <- abs(aphi - 90)
  dayl <- ifel(tt <= 1e-8 & adelta <= 1e-8, 0,
               ifel(adelta <= 1e-8, 12,
                    ifel(aphi <= 1e-8, 12, 
                         ifel(aphi < (90 - adelta), 24 * acos(-spa / cp) / pi,
                              ifel((lat * delta) > 0, 24,
                                   ifel((lat * delta) < 0, 0, 24 * acos(-spa / cp) / pi))))))
  
  toa <- ifel(tt <= 1e-8 & adelta <= 1e-8, 0,
                 ifel(adelta <= 1e-8, s * cos(rphi),
                      ifel(aphi <= 1e-8, s * cos(rdelta), 
                           ifel(aphi < (90 - adelta), s * (acos(-spa / cp) * spa + cp * sqrt(1 - (-spa / cp) * (-spa / cp))),
                                ifel((lat * delta) > 0, s * spa * pi,
                                     ifel((lat * delta) < 0, 0, s * (acos(-spa / cp) * spa + cp * sqrt(1 - (-spa / cp) * (-spa / cp)))))))))
  
  return(list(toa=toa, dayl = dayl, delta = delta))
  
}


# Orbital parameters
# These 3 orbital parameters are available from the boundary conditions of E. Amstrong dataset (2019)
# https://www.nature.com/articles/s41597-019-0277-1/tables/2:
# ecc: eccentricity parameter
# pre: precession parameter
# xob: obliquity (tilt) (degrees)
# We just need to calculate this one:
# perh: longitude of perhelion
.calculate_perh <- function(ecc, pre){
  
  pir <- pi/180
  
  perh <- 1/pir * asin(pre/ecc)
  
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

