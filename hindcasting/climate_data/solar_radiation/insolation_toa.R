
# insolation at the top of the model atmosphere
# computed using the method of Berger (1978)
# (as implemented in CAM 3.0)

# lat: latitude in degrees
# lon: longitude in degrees starting from Greenwich running eastward
# d: calendar day d (values of d at 0 GMT for January 1 and December 31 are 0 and 364) 
#     => for example, a calendar day d having no fraction (such as 182.00) refer to local midnight at Greenwich, and to local noon at the date line (180 long)
# orbital parameters = list(eccentricity, obliquity, precession)
# S0 = solar constant

insolation_toa <- function(d, lat, lon, orbital_parameters, S0 = 1361){
  
  # latitude in radians
  phi <- lat * pi/180
  
  # hour angle
  H = 2*pi*(d+(lon/360))
  
  # omega, longitude of the perihelion from the moving equinox in degrees
  omega <- asin(orbital_parameters$precession/orbital_parameters$eccentricity)*180/pi
  
  # omega_v, longitude of the perihelion + 180deg
  omega_v <- omega + 180
  
  # mean longitude at the time of the vernal equinox
  beta <- sqrt(1-orbital_parameters$eccentricity^2)
  lambda_m0 <- 2*((orbital_parameters$eccentricity/2+orbital_parameters$eccentricity^3/8)*(1+beta)*sin(omega_v) -
                    orbital_parameters$eccentricity^2/4*(1/2+beta)*sin(omega_v*2) +
                    orbital_parameters$eccentricity^3/8*(1/3 +beta)*sin(omega_v*3))
  
  # mean longitude
  lambda_m <- lambda_m0 + (2*pi*(d-80.5))/365
  
  # lambda true longitude of the earth relative to vernal equinox
  lambda <- lambda_m+(2*orbital_parameters$eccentricity-orbital_parameters$eccentricity^3/4)*sin(lambda_m-omega_v) +
    5*orbital_parameters$eccentricity^2/4*sin(2*(lambda_m-omega_v)) +
    13*orbital_parameters$eccentricity^3/12*sin(3*(lambda_m-omega_v))
  
  # rho
  rho <- (1-orbital_parameters$eccentricity^2)/(1+orbital_parameters$eccentricity*cos(lambda-omega_v))
  
  # delta, solar declination in radians
  delta <- asin(sin(orbital_parameters$obliquity)*sin(lambda))
  
  # cos of solar zenith angle mu
  cos_mu <- H*sin(phi)*sin(delta)-cos(phi)*cos(delta)*cos(H)
  
  # insolation
  Si <- S0 * rho^(-2) * cos_mu
  
  return(Si)
 
}



