
compute_airmass <- function(dayl, lat, delta, ratm){
  
  cat("Computing daily airmass\n")
  
  # parameters
  m0 <- 1
  m80 <- 5.6
  m90 <- 39.7
  pir <- pi/180
  mindayl <- 2 * 10e-30
  cos80 <- cos(80 * pir)
  c00 <- c(0.008307, 0, 0)
  c00[2] <- (m0 - m80) * (c00[1] + 1) * (c00[1] + cos80) / (cos80 - 1)
  c00[3] <- m0 - c00[2] / (c00[1] + 1)
  c80 <- c(0.037160, 0, 0)
  c80[2] <- (m90 - m80) * c80[1] * (c80[1] + cos80) / cos80
  c80[3] <- m90 - c80[2] / c80[1]
  w  = 15
  
  # "identity" raster
  rid <- dayl 
  rid[!is.na(rid)] <- 1
  
  # declare raster
  mbar <- mc <- ml <- mo <- rid
  
  
  t1 <- ifel(dayl > mindayl, 0.5*dayl, dayl)
  Zn <- ifel(abs(lat + delta) >= 90, acos(sin(pir * lat) * sin(pir * delta) - cos(pir * lat) * cos(pir * delta)) / pir, 90)
  Z0 <- ifel(abs(lat - delta) >= 90, 90, lat - delta)
  b <- cos(pir * lat) * cos(pir * delta)
  t80 <- 1 / w * acos((cos80 - sin(pir * lat)  * sin(pir * delta)) / (cos(pir * lat) * cos(pir * delta))) / pir
  
  mbar <- ifel(dayl == 0, m90, 
               ifel(t1 == 0, m90, 
                    ifel(abs(Zn) <= 80, (1/t1) * .Fint(t1,a = sin(pir * lat) * sin(pir * delta) + c00[1], b, c00),
                         ifel(abs(Z0) >= 80,  (1/t1) * .Fint(t1,a = c80[1] + sin(pir * lat) * sin(pir * delta), b,c80),
                              (1/t1) * (.Fint(t80,a = c00[1] + sin(pir * lat) * sin(pir * delta),b,c00) + 
                                          .Fint(t1,a = c80[1] + sin(pir * lat) * sin(pir * delta),b,c80) - 
                                          .Fint(t80,a = c80[1] + sin(pir * lat) * sin(pir * delta),b,c80))))))
  
  Z <- Z0
  cosZ <- cos(Z * pir)
  mo <- ifel(dayl == 0, m90, 
             ifel(Z <= 80, c00[2] / (c00[1] + cosZ) + c00[3],
                  c80[2] / (c80[1] + cosZ) + c80[3]))
  
  Z <- (Z0 + Zn) / 2
  cosz = (cos(Z0 * pir) + cos(Zn * pir)) / 2
  mc <- ifel(dayl == 0, m90, 
             ifel(Z <= 80, c00[2] / (c00[1] + cosZ) + c00[3],
                  c80[2] / (c80[1] + cosZ) + c80[3]))
  
  Z <- (Z0 + 3 * Zn) / 4
  cosz <- (cos(Z0 * pir) + 3 * cos(Zn * pir)) / 4
  ml <- ifel(dayl == 0, m90, 
             ifel(Z <= 80, c00[2] / (c00[1] + cosZ) + c00[3],
                  c80[2] / (c80[1] + cosZ) + c80[3]))
  
  
  # correct calculated air mass for elevation
  mbar <- ratm * mbar
  mo <- ratm * mo
  mc <- ratm * mc
  ml <- ratm * ml
  
  
  return(list(mbar = mbar, mo = mo, mc = mc, ml = ml))
  
}

.Fint <- function(t1,a,b,c){
  
  pir <- pi / 180
  w <- 15
  rw <- pir * w
  wpi <- 180 / (pi * w)
  wt1 <- rw * t1
  
  Fair <- ifel(a > b, wpi * c[2] / sqrt(a**2 - b**2) * acos((b + a * cos(wt1)) / (a + b * cos(wt1))) + c[3] * t1,
               ifel(a < b, F <- wpi * c[2] / sqrt(b**2 - a**2) * log((sqrt((b + a) * (1 + cos(wt1))) + sqrt((b - a) * (1 - cos(wt1)))) / (sqrt((b + a) * (1 + cos(wt1))) - sqrt((b - a) * (1 - cos(wt1))))) + c[3] * t1, 
                    wpi * c[2] / a * tan(wt1 / 2) + c[3] * t1))
  
  return(Fair)
  
}
