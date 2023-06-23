
compute_thermo_variables <- function(tmin, tmax){
  
  cat("Computing humidity-related variables\n")
  
  # parameters
  sb <- 5.6704e-8
  e <- 0.98 
  lw_albedo <- 0.045 
  a <- 10.77
  b <- 2.34
  c <- 18.44
  cs <- 1.5 
  Tfreeze <- 273.15 
  
  # set values
  temp <- (tmin + tmax)/2 # approximation
  
  # temperature in Kelvin
  Tk <- temp + Tfreeze
  
  gamma <- 65.05 + temp * 0.064 # psychrometric constant
  
  lvap <- 0.001 * 1.91846e6 * (Tk / (Tk - 33.91))**2  # latent heat
  
  # these coeff are for celcius temperature
  bl <- c(4.44017302e-1, 2.86064092e-2, 7.94683137e-4, 1.21211669e-5, 1.03354611e-7,
          4.04125005e-10, -7.88037859e-13, -1.14596802e-14, 3.81294516e-17)
  bi <- c(5.03277922e-1, 3.77289173e-2, 1.26801703e-3, 2.49468427e-5, 3.13703411e-7,
          2.57180651e-9, 1.32268878e-11, 3.94116744e-14, 4.98070196e-17)
  
  # slope of vapour pressure curve
  ss <- 100*ifel(Tk <= Tfreeze, sum(bi[1]*(Tk - Tfreeze)**0,  tapp(rast(lapply(2:9, function(i) bi[i]*(Tk - Tfreeze)**(i-1))), 1:365, sum)),
                 sum(bl[1]*(Tk - Tfreeze)**0, tapp(rast(lapply(2:9, function(i) bl[i]*(Tk - Tfreeze)**(i-1))), 1:365, sum)))
  
  Ts <- Tk # approximation: mean daily surface temperature = air temperature
  
  # these coeff are for celcius temperature
  al <- c(6.11213476, 4.44007856e-1, 1.43064234e-2, 2.64461437e-4, 3.05903558e-6,
          1.96237241e-8, 8.92344772e-11, -3.73208410e-13, 2.09339997e-16)
  ai <- c(6.11123516, 5.03109514e-1, 1.88369801e-2, 4.20547422e-4,
          6.14396778e-6, 6.02780717e-8, 3.87940929e-10, 1.49436277e-12, 2.62655803e-15)
  
  # saturated vapor pressure 
  es <- ifel(Tk <= Tfreeze, sum(ai[1]*(Tk - Tfreeze)**0,  tapp(rast(lapply(2:9, function(i) ai[i]*(Tk - Tfreeze)**(i-1))), 1:365, sum)),
             sum(al[1]*(Tk - Tfreeze)**0, tapp(rast(lapply(2:9, function(i) al[i]*(Tk - Tfreeze)**(i-1))), 1:365, sum)))  
  
  TdewK <- 34.07 + 4157 / log(2.1718e8 / es)
  tdew <- TdewK - Tfreeze
  
  return(list(tdew = tdew, ss = ss, gamma = gamma, es = es))
  
}


