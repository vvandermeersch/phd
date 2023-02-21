
# Function adapted by V. Van der Meersch from Evapotranspiration package (created by Danlu Guo)

# First use: present climate, added some modifications (to correct polar night effect...), see version 1

# Second use: past climate, don't use extraterrestrial radiation anymore (formula for solar declination and distance Earth-Sun are not valid in the past)
# Simple way: direct use of Rso and Rs (we have these data)
# Also modify formula for actual vapour pressure, following approximation of Allen et al. (1998), because we do not have RH or TDew in the past

# data: Tmax, Tmin, Rso (clear-sky surface solar radiation downward), Rs (surface solar radiation downward = global radiation), uz (wind speed)
# elev : ground elevation above mean sea level in m

compute_PET2 <- function(data, elev, constants, alpha = 0.23){
  
  z0 <- 0.02 # short crop, i.e. method for FAO-56 hypothetical short grass
  
  Ta <- (data$Tmax + data$Tmin)/2
  
  # Saturated vapour pressure
  vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax + 237.3))
  vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin + 237.3))
  vas <- (vs_Tmax + vs_Tmin)/2
  
  # Vapour pressure, following Allen et al. (1998) : Tdew can be reasonably replaced by Tmin in Europe (be carefull of arid/humid regions !)
  vabar <- vs_Tmin
  
  P <- 101.3 * ((293 - 0.0065 * elev)/293)^5.26 # atmospheric pressure
  # effect of atmospheric pressure (i.e. evaporation at high altitudes promoted due to low atmospheric pressure) is small
  # thus, the average value of atmospheric pressure for a location is sufficient
  
  delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 237.3)^2) # slope of vapour pressure curve
  gamma <- 0.00163 * P/constants$lambda # psychrometric constant
  
  R_so <- data$Rso
  
  R_s <- data$Rs
  
  R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) * 
    ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4)/2 * 
    (1.35 * R_s/R_so - 0.35) # estimated net outgoing longwave radiation
  R_nsg <- (1 - alpha) * R_s # net incoming shortwave radiation - water or other evaporative surface with specified Albedo
  R_ng <- R_nsg - R_nl
  
  u2 <- data$uz * 4.87/log(67.8 * constants$z - 5.42)
  
  ET_RC  <- (0.408 * delta * (R_ng - constants$G) + 
               gamma * 900 * u2 * (vas - vabar)/(Ta + 273))/(delta + 
                                                               gamma * (1 + 0.34 * u2))
  
  ET  <- ET_RC 
  
  return(unlist(ET))
  
}