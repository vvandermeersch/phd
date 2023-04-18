
# Functions to compute daily global radiation and potential evaporation
# from "recalculated" TOA values, and generated precipitation, cloudiness and temperature (GWGEN)

# this code is largely inspired by J. Kaplan LPJ_LMfire code available on GitHub: https://github.com/ARVE-Research/LPJ-LMfire
# mainly based on a paper by X. Yin (1998)

# adapted by V. Van der Meersch - 12/12/2022 (code was originally written in C)

# one can choose the method to calculate the potential evapotranspiration and the net outgoing longwave radiation
# pet_method : "kaplan" or "faoPM"
# - "kaplan" stand for the original method found in LPJ_LMfire code (J. Kaplan) - this method is not suited for our models, since it seems to
#    compute PET as an open water evaporation, using Josey et al. method for determining longwave flux at the ocean surface
# - "faoPM" stand for the FAO Penman-Monteith method, where PET is computed  for agricultural land as if it is well watered.
#   To compute longwave flux, we use the Stefan-Boltzmann law corrected by two factors (humidity and cloudiness)


# Main function
compute_glo_radiation_from_toa <- function(toa_file, input_file, gwgen_file, elev_data, output_dir, pet_method, ncores = 1){
  
  # name of output file
  output_file <- file.path(output_dir,
                           paste0(strsplit(basename(toa_file), "_")[[1]][1], 
                                  "_", strsplit(basename(toa_file), "_")[[1]][2], "_glorad.csv"))
  
  # read data
  cat("Reading data...\n")
  toa_data <- fread(toa_file) # TOA radiation computed before
  input_data <- fread(input_file) # raw monthly data of GCM simulations
  gwgen_data <- fread(gwgen_file) # generated daily values
  
  # in case of parallel computing
  plan(multisession, workers = ncores)
  
  # compute the "precipitation equitability index" (approx tmean = tmin + tmax /2), used thereafter to compute global radiation
  cat("Compute the precipitation equitability index...\n")
  input_data$temp <- (input_data$min.temperature + input_data$max.temperature)/2
  coldest_month <- input_data %>%
    group_by(`station id`, year) %>%
    filter(temp == min(temp)) %>%
    dplyr::select(`station id`, year, temp, precipitation)
  names(coldest_month) <- c("id", "year", "t_cm", "p_cm")
  warmest_month <- input_data %>%
    group_by(`station id`, year) %>%
    filter(temp == max(temp)) %>%
    dplyr::select(`station id`, year, temp, precipitation)
  names(warmest_month) <- c("id", "year", "t_wm", "p_wm")
  peqin_data <- left_join(coldest_month, warmest_month, by = c("id", "year"))
  cat("Test...\n")
  peqin <- future_lapply(1:nrow(peqin_data), function(i){calculate_prec_eq_index(peqin_data[i,])})
  peqin_data$peqin <- unlist(peqin) 
  toa_data <- left_join(toa_data, peqin_data, by = c("id", "year"))
  
  # daily mean temperature, approximation
  gwgen_data$temp <- (gwgen_data$tmin + gwgen_data$tmax)/2
  
  # compute the relative atmospheric pressure - TO MODIFY
  #z0 <- 1/8000
  #elev_data$ratm <- exp(-elev_data$alt * z0)
  #toa_data <- left_join(toa_data, elev_data, by = c("id"))
  toa_data$ratm <- 0.9
  
  # compute daily airmass, used thereafter to compute global radiation
  cat("Compute daily airmass...\n")
  airmass <- future_lapply(1:nrow(toa_data), function(i){
    as.numeric(calculate_airmass(toa_data[i,]))
  })
  airmass <- do.call(rbind, airmass)
  airmass <- data.frame(airmass)
  names(airmass) <- c("mbar", "mo", "mc", "ml")
  toa_data <- cbind(toa_data, airmass)
  
  
  # compute long-wave radiation (needed to compute PET in Kaplan method),
  # saturation vapour pressure, slope of vapour pressure curve, latent heat and psychrometric constant
  cat("Compute long-wave radiation...\n")
  lw <- future_lapply(1:nrow(toa_data), function(i){
    as.numeric(calculate_lw(toa_data[i,], gwgen_data[i,]))
  })
  lw <- do.call(rbind, lw)
  lw <- data.frame(lw)
  names(lw) <- c("ss", "gamma", "lvap", "lw", "es", "TdewK")
  toa_data <- cbind(toa_data, lw)
  
  toa_data <- data.frame(toa_data)
  
  # compute global radiation
  cat("Compute global radiation...\n")
  sw_pet <- future_lapply(1:nrow(toa_data), function(i){
    
    if(pet_method == "kaplan"){ # not used for our models
      
      k <- 1
      albedo <- 0.17 # surface shortwave albedo in LPJ_LMfire code
      pet <- pet0 <- 0
      
      # compute global radiation
      sw_i <- calculate_glo_from_toa(toa_data[i,], gwgen_data[i,], pet)
      
      # compute PET from net radiation (as in LPJ_LMfire)
      netrad <- (1 - albedo) * sw_i - toa_data[i,"lw"]
      pet <- max((toa_data[i,"ss"] / (toa_data[i,"ss"] + toa_data[i,"gamma"])) * netrad / toa_data[i,"lvap"], 0)
      
      # J. Kaplan: "weak dependence between global radiation and PET, need to equilibrate them"
      while(abs(pet - pet0) > 0.01 & k < 100){
        
        pet0 <- pet
        
        # compute global radiation
        sw_i <- calculate_glo_from_toa(toa_data[i,], gwgen_data[i,], pet)
        
        # compute PET from net radiation (as in LPJ_LMfire)
        netrad <- (1 - albedo) * sw_i - toa_data[i,"lw"]
        pet <- max((toa_data[i,"ss"] / (toa_data[i,"ss"] + toa_data[i,"gamma"])) * netrad / toa_data[i,"lvap"], 0) 
        
        k <- k + 1
        
        if(k == 100){
          warning("Problem of convergence")
        }
        
      }
      
    }
    else if(pet_method == "faoPM"){
      
      k <- 1
      albedo <- 0.23 # hypothetical reference crop with an albedo of 0.23
      pet <- pet0 <- 0
      
      # wind speed
      uz <- gwgen_data[i,"wind"]
      z_meas <- 10
      u2 <- uz * 4.87/log(67.8 * z_meas - 5.42)
      
      # saturation vapour pressure
      es_Tmax <- 0.6108 * exp(17.27 * gwgen_data[i,"tmax"]/(gwgen_data[i,"tmax"] + 237.3))
      es_Tmin <- 0.6108 * exp(17.27 * gwgen_data[i,"tmin"]/(gwgen_data[i,"tmin"] + 237.3))
      es <- (es_Tmax + es_Tmin)/2
      
      # Tdew <- 34.07 + 4157 / log(2.1718e8 / es) - 273.15
      Tdew <- gwgen_data[i,"tmin"] # approximation
      ea <- 0.6108 * exp((17.27 * Tdew)/(Tdew+237.3))
      
      # slope of vapour pressure curve (delta)
      # ss <- 4098 * (0.6108 * exp((17.27 * gwgen_data[i,"temp"])/(gwgen_data[i,"temp"] + 237.3)))/((gwgen_data[i,"temp"] + 237.3)^2)
      ss <- toa_data[i,"ss"]/1000 # convert to kPa/C
      
      # elev <- elev_data$alt - TO MODIFY
      elev <- 10
      P <- 101.3 * ((293 - 0.0065 * elev)/293)^5.26 # atmospheric pressure
      
      # psychrometric constant
      #lambda <- 2.45
      #gamma <- 0.00163 * P/lambda
      gamma <- toa_data[i,"gamma"]/1000 #convert to kPa/C
      
      # effect of cloudiness (relative shortwave radiation)
      # r_so <- (0.75 + (2 * 10^-5) * elev) * toa_data[i,"toa"]/1000
      rs_rs0 <- 1-0.29*(gwgen_data[i,"mean_cloud"] + (gwgen_data[i,"mean_cloud"])^2) # Antoine et al, 1996
      
      # compute global radiation
      sw_i <- calculate_glo_from_toa(toa_data[i,], gwgen_data[i,], pet)
      r_s <- sw_i/1000 # convert to MJ/m2
      
      # estimated net outgoing longwave radiation
      r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
        ((gwgen_data[i,"tmax"] + 273.2)^4 + (gwgen_data[i,"tmin"] + 273.2)^4)/2 * 
        (1.35 * rs_rs0 - 0.35) 
      
      # net radiation
      r_ng <- (1 - albedo) * r_s  - r_nl
      
      # compute PET (Penman-Monteith equation) 
      pet <- max((0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (gwgen_data[i,"temp"] + 273.15))/
        (ss + gamma * (1 + 0.34 * u2)),0)
      
      while(abs(pet - pet0) > 0.01 & k < 100){
        pet0 <- pet
        
        # compute global radiation and pet 
        sw_i <- calculate_glo_from_toa(toa_data[i,], gwgen_data[i,], pet)
        r_s <- sw_i/1000 # convert to MJ/m2
        r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
          ((gwgen_data[i,"tmax"] + 273.2)^4 + (gwgen_data[i,"tmin"] + 273.2)^4)/2 * 
          (1.35 * rs_rs0 - 0.35) # estimated net outgoing longwave radiation
        r_ng <- (1 - albedo) * r_s - r_nl # net radiation
        pet <- max((0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (gwgen_data[i,"temp"] + 273.15))/
                     (ss + gamma * (1 + 0.34 * u2)),0)
        
        k <- k + 1
        
        if(k == 100){
          warning("Problem of convergence")
        }
        
      }
  
      netrad <- r_ng
      
    }
    
    return(c(glo = sw_i, pet = pet, netrad = netrad))
  
  })
  
  sw_pet <- do.call(rbind, sw_pet)
  sw_pet <- data.frame(sw_pet)
  gwgen_data <- cbind(gwgen_data, sw_pet)
  gwgen_data$glo <- round((gwgen_data$glo/1000),4) #convert to MJ.m-2
  gwgen_data$pet <- round(gwgen_data$pet, 4)
  
  # write data in output file
  write.table(gwgen_data, file = output_file, col.names = T, row.names = FALSE, sep=",")
  
  plan(sequential)
  gc()
  
  message("Daily radiation computed !")
  
  return(output_file)
  
}



# function to calculate surface downwelling shortwave radiation - from Yin, 1998
# original author: J. Kaplan
calculate_glo_from_toa <- function(toa_data, gwgen_data, pet, alb = 0.23){
  
  # parameters
  kp <- 0.5
  kag <- 3.3
  kan <- 2.32
  kn <- 0.686
  albedo <- alb
  
  # set values
  mbar <- toa_data$mbar # daytime mean optical air mass
  mo <- toa_data$mo # air mass at cosine zenith angle maximum
  mc <- toa_data$mc # air mass at cosine zenith angle medium
  ml <- toa_data$ml # air mass at cosine zenith angle bottom quarter range point
  toa <- toa_data$toa
  cldf <- gwgen_data$mean_cloud
  dayl <- toa_data$dayl
  prec <- gwgen_data$prcp
  tcm <- toa_data$t_cm
  peqin <- toa_data$peqin
  Ratm <- toa_data$ratm
  
  # clear-sky factor
  # sunf <- 1-0.29*(cldf + (cldf)^2) # Antoine et al. 1996
  sunf <- 1 - cldf
  
  # tropics indicator (tropical = 1, else 0)
  if (tcm < 10){
    x <- 0
  } else if (tcm > 20){
    x <- 1
  } else{
    x <- sin(pi / 2 * (tcm / 10 - 1.))
  }
  
  # direct insolation atmospheric turbidity factor
  tau <- exp(-0.115 * Ratm * ((2.15 - 0.713 * x + exp(-6.74 / (prec + 1))) * exp(0.0971 * pet) - 0.650 * (1 - x) * peqin))
  
  # atmospheric transmittance function
  fm <- 0.01452 * (mbar + ml) * exp(1.403 * tau) - 0.1528 * mo + mc + 0.48700 * (mc - ml) + 0.2323 
  
  # direct downwelling shortwave radiation
  direct <- sunf * tau**kp * toa * tau**fm
  
  # diffuse insolation atmospheric turbidity factor
  zeta0 <- 0.503 * exp(-1.20 * Ratm * exp(-0.633 / (prec + 1.) - 0.226 * pet))
  
  # diffuse downwelling shortwave radiation
  diffuse <- zeta0 * kag**albedo * kan**(1 - sunf) * (1 - kn * (1 - sunf)) * (tau**kp * toa - direct)
  
  # global radiation
  glo <- (diffuse+direct)
  
  # print(diffuse/glo)
  
  return(glo)
  
}



# function to calculate the "precipitation equitability index"
# original author: J. Kaplan
calculate_prec_eq_index <- function(data){
  
  if((data$p_wm + data$p_cm) > 0){
    peqin = 2 * (data$p_wm - data$p_cm) / (data$p_wm + data$p_cm)
    peqin = max(peqin,0)
    
  }else{
    peqin = 0
  }
  
  return(peqin)
  
}



# function to calculate air mass - from Yin, 1997
# original author: J. Kaplan
calculate_airmass <- function(data){
  
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

  
  # set values
  lat <- data$lat
  delta <- data$delta
  dayl <- data$dayl
  Ratm <- data$ratm
  
  if(dayl == 0){
    
    mbar <- m90
    mc <- m90
    ml <- m90
    mo <- m90
    
  }else{
    
    rlat <- pir * lat
    rdelta <- pir * delta
    
    sinlat <- sin(rlat)
    sindel <- sin(rdelta)
    coslat <- cos(rlat)
    cosdel <- cos(rdelta)
    
    
    if(dayl > mindayl){
      
      t1 <- 0.5 * dayl
      
    }else{
      
      t1 <- dayl
      
    }
    
    if (abs(lat + delta) >= 90){
      
      Zn <- acos(sinlat * sindel - coslat * cosdel) / pir
      
    }else{
      
      Zn <- 90
      
    }
    
    if (abs(lat - delta) >= 90){
      
      Z0 <- 90
      
    }else{
      
      Z0 <- lat - delta
      
    }
    
    # conversion to radian
    rZ0 <- Z0 * pir  
    rZn <- Zn * pir
    
    b <- coslat * cosdel
    
    if (t1 == 0){
      
      mbar <- m90
      
    }else if(abs(Zn) <= 80){
      
      tinv <- 1/t1
      
      c <- c00
      a <- c[1] + sinlat * sindel
      mbar <- tinv * F(t1,a,b,c)
      
    }else if(abs(Z0) >= 80){
      
      tinv <- 1 / t1
      
      c <- c80
      a <- c[1] + sinlat * sindel
      mbar <- tinv * F(t1,a,b,c)
      
    }else{
      
      t80 <- 1 / w * acos((cos80 - sinlat * sindel) / (coslat * cosdel)) / pir
      
      c <- c00
      a <- c[1] + sinlat * sindel
      
      tmp1 <- F(t80,a,b,c)
      
      c <- c80
      a <- c[1] + sinlat * sindel
      tmp2 <- F(t1,a,b,c)
      
      c <- c80
      a <- c[1] + sinlat * sindel
      tmp3 <- F(t80,a,b,c)
      
      tinv <- 1 / t1
      mbar <- tinv * (tmp1 + tmp2 - tmp3)
      
    }

    
    Z <- Z0
    
    cosZ <- cos(Z * pir)
    
    if (Z <= 80){
      c <- c00
    }else{
      
      c <- c80
      
    }
    
    mo <- c[2] / (c[1] + cosZ) + c[3]
    
    
    Z <- (Z0 + Zn) / 2
    
    cosz = (cos(rZ0) + cos(rZn)) / 2.
    
    if (Z <= 80){
      c <- c00
    }else{
      c <- c80
    }
    
    mc <- c[2] / (c[1] + cosZ) + c[3]
    
    Z <- (Z0 + 3 * Zn) / 4
    
    cosz <- (cos(rZ0) + 3 * cos(rZn)) / 4
    
    if (Z <= 80){
      c <- c00
    }else{
      c <- c80
    }

    ml = c[2] / (c[1] + cosZ) + c[3]
    
    # correct calculated air mass for elevation
    mbar <- Ratm * mbar
    mo <- Ratm * mo
    mc <- Ratm * mc
    ml <- Ratm * ml
    
  }
  
  return(list(mbar = mbar, mo = mo, mc = mc, ml = ml))
  
}



# integral air mass function F - from Yin, 1997
# original author: J. Kaplan
F <- function(t1,a,b,c){
  
  pir <- pi / 180
  w <- 15
  rw <- pir * w
  wpi <- 180 / (pi * w)
  wt1 <- rw * t1
  
  if (a > b){
    
    F <- wpi * c[2] / sqrt(a**2 - b**2) * acos((b + a * cos(wt1)) / (a + b * cos(wt1))) + c[3] * t1
    
  }else if(a < b){
    
    e1 <- sqrt((b + a) * (1 + cos(wt1))) + sqrt((b - a) * (1 - cos(wt1)))
    e2 <- sqrt((b + a) * (1 + cos(wt1))) - sqrt((b - a) * (1 - cos(wt1)))
    
    F <- wpi * c[2] / sqrt(b**2 - a**2) * log(e1 / e2) + c[3] * t1
    
  }else{
    
    F <- wpi * c[2] / a * tan(wt1 / 2) + c[3] * t1
    
  }
  
  return(F)
  
}



# function to calculate long-wave radiation - from Haxeltine and Prentice (1983), with modifications
# original author: J. Kaplan
calculate_lw <- function(toa_data, gwgen_data){
  
  # parameters
  sb <- 5.6704e-8
  e <- 0.98 
  al <- 0.045 
  a <- 10.77
  b <- 2.34
  c <- 18.44
  cs <- 1.5 
  Tfreeze <- 273.15 
  
  # set values
  dayl <- toa_data$dayl
  temp <- (gwgen_data$tmin + gwgen_data$tmax)/2 # approximation
  tmin <- gwgen_data$tmin
  tmax <- gwgen_data$tmax 
  cldf <- gwgen_data$mean_cloud
  
  # clear-sky factor
  sunf <- 1 - cldf
  
  Tk <- temp + Tfreeze
  
  gamma <- 65.05 + temp * 0.064 # psychrometric constant
  
  lvap <- 0.001 * 1.91846e6 * (Tk / (Tk - 33.91))**2  # latent heat
  
  ss <- sapply(Tk, desdT) # slope of vapour pressure curve
  
  f <- 0.2 + 0.8 * sunf  
  
  Ts <- Tk # approximation: mean daily surface temperature = air temperature
  
  Ql_up <- e * sb * Ts**4
  
  es <- 0.01 * esat(Tk)  # saturated vapor pressure - commented by V.V.
  # added by V.V.
  # from FAO: "using mean air temperature instead of daily minimum and maximum temperatures 
  # results in lower estimates for the mean saturation vapour pressure" => One should use tmin and tmax
  # TminK <- tmin + Tfreeze
  # TmaxK <- tmax + Tfreeze
  # es_min <- 0.01 * sapply(TminK, esat)
  # es_max <- 0.01 * sapply(TmaxK, esat)
  # es <- (es_min + es_max)/2

  TdewK <- 34.07 + 4157 / log(2.1718e8 / es)  

  D <- TdewK - Tk

  Ql_dn <- sb * (Tk + a*cldf**2 + b*cldf + c + 0.84 * (D + 4.01))**4 

  Ql <- Ql_up - (1 - al) * Ql_dn   

  lw_rad <- 0.001 * 3600 * dayl * Ql  
  tdew <- TdewK - Tfreeze
  
  return(list(ss = ss, gamma = gamma, lvap = lvap, lw = lw_rad, es = es, TdewK = TdewK))
  
}



# function to calculate saturation vapor pressure in water and ice - from Flatau et al., 1992
# temperature in Kelvin
# original author: J. Kaplan
esat <- function(temp){
  
  tfreeze <- 273.15 
  # these coeff are for celcius temperature
  al <- c(6.11213476, 4.44007856e-1, 1.43064234e-2, 2.64461437e-4, 3.05903558e-6,
         1.96237241e-8, 8.92344772e-11, -3.73208410e-13, 2.09339997e-16)
  ai <- c(6.11123516, 5.03109514e-1, 1.88369801e-2, 4.20547422e-4,
          6.14396778e-6, 6.02780717e-8, 3.87940929e-10, 1.49436277e-12, 2.62655803e-15)
  
  
  if (temp <= tfreeze){
    
    a <- ai
    
  }else{
    
    a <- al
  }
  
  T <- temp - tfreeze # convert to celcius
  
  esat = a[1]
  
  for(i in 2:9){
    esat <- esat + a[i] * T**i
  }
  
  esat <- 100 * esat
  
  return(esat)
  
}



# function to calculate the first derivative of saturation vapor pressure in water and ice - from Flatau et al., 1992
# temperature in Kelvin
# original author: J. Kaplan
desdT <- function(temp){
  
  tfreeze <- 273.15 
  # these coeff are for celcius temperature
  bl <- c(4.44017302e-1, 2.86064092e-2, 7.94683137e-4, 1.21211669e-5, 1.03354611e-7,
          4.04125005e-10, -7.88037859e-13, -1.14596802e-14, 3.81294516e-17)
  bi <- c(5.03277922e-1, 3.77289173e-2, 1.26801703e-3, 2.49468427e-5, 3.13703411e-7,
          2.57180651e-9, 1.32268878e-11, 3.94116744e-14, 4.98070196e-17)
  
  if(temp <= tfreeze){
    b <- bi
  }else{
    b <- bl
  }
  
  T = temp - tfreeze
  
  desdT <- b[1]
  
  for(i in 2:9){
    desdT <- desdT + b[i] * T**i
  }
  
  desdT = 100 * desdT
  
  return(desdT)
  
  
}

  

