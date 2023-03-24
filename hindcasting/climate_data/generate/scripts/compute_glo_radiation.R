# Script to compute global radiation
# Much more faster to use a script rather than to use a function... Don't know why !
message("Starting the script to compute global radiation...")
# name of output file
output_file <- file.path(output_dir,
                         paste0(strsplit(basename(toa_file), "_")[[1]][1], 
                                "_", strsplit(basename(toa_file), "_")[[1]][2], "_glorad.csv"))

# read data
cat("Preparing data...\n")
toa_data <- fread(toa_file) # TOA radiation computed before
input_data <- fread(input_file) # raw monthly data of GCM simulations
gwgen_data <- fread(gwgen_file) # generated daily values

# load altitude data (yearly data)
mid_year <- (years[1]+years[2])/2
alt <- load_altitude_ICE6GC(year = mid_year, folder = "D:/climate/ICE-6G-C", folder_hadcm3b = "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw", extent)

# compute the relative atmospheric pressure
z0 <- 1/8000
alt$ratm <- exp(-alt$alt * z0)
toa_data <- left_join(toa_data, alt[,c("lat", "lon", "alt", "ratm")], by = c("lat", "lon"))
# test
if(length(which(is.na(toa_data))) != 0){
  stop("problem with altitude data !")
}

# load albedo (monthly data)
alb <- lapply(years[1]:years[2], function(yr){
  alb_yr <- load_albedo(yr, raw_clim_dir, ext(c(-14,40,34,72)))
  # print(nrow(alb_yr))
  alb_yr$year <- yr
  return(alb_yr)
})
alb <- do.call(rbind, alb)
alb <- data.frame(alb)
# mean over 30 years (no significant changes)
alb <- alb %>% 
  group_by(id, lon, lat, month) %>%
  summarise(alb = mean(alb))
alb$month <- as.integer(alb$month)
toa_data <- left_join(toa_data, alb[,c("lat", "lon", "month", "alb")], by = c("lat", "lon", "month"))
# test
if(length(which(is.na(toa_data))) != 0){
  stop("problem with albedo data !")
}


# in case of parallel computing
ncores <- 10
plan(multisession, workers = ncores)

# compute the "precipitation equitability index" (approx tmean = tmin + tmax /2), used thereafter to compute global radiation
cat("Computing precipitation equitability index...\n")
start_time <- Sys.time()
input_data$temp <- (input_data$min.temperature + input_data$max.temperature)/2
coldest_month <- input_data %>%
  group_by(`station id`, year) %>%
  filter(temp == min(temp)) %>%
  filter(1:n() == 1) %>% # only first min if multiple
  dplyr::select(`station id`, year, temp, precipitation)
names(coldest_month) <- c("id", "year", "t_cm", "p_cm")
warmest_month <- input_data %>%
  group_by(`station id`, year) %>%
  filter(temp == max(temp)) %>%
  filter(1:n() == 1) %>% # only first min if multiple
  dplyr::select(`station id`, year, temp, precipitation)
names(warmest_month) <- c("id", "year", "t_wm", "p_wm")
peqin_data <- left_join(coldest_month, warmest_month, by = c("id", "year"))
peqin <- future_lapply(1:nrow(peqin_data), function(i){calculate_prec_eq_index(peqin_data[i,])})
peqin_data$peqin <- unlist(peqin) 
toa_data <- left_join(toa_data, peqin_data, by = c("id", "year"))
end_time <- Sys.time()
# cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))

# daily mean temperature, approximation
gwgen_data$temp <- (gwgen_data$tmin + gwgen_data$tmax)/2



plan(sequential)
gc()


# compute daily airmass, used thereafter to compute global radiation
cat("Computing daily airmass...\n")
start_time <- Sys.time()
for(yr in unique(toa_data$year)){
  # cat(paste0("Year ", yr, "\n"))
  
  toa_data_temp <- toa_data[toa_data$year == yr,]
  
  plan(multisession, workers = ncores)
  
  runt <- system.time(airmass <- future_lapply(1:nrow(toa_data_temp), function(i){
    as.numeric(calculate_airmass(toa_data_temp[i,]))
  }))
  
  plan(sequential)
  
  # cat(paste0("Runtime: ", round(runt[3]), "s \n"))
  
  airmass <- do.call(rbind, airmass)
  
  toa_data[toa_data$year == yr, "mbar"] <- unlist(airmass[,1])
  toa_data[toa_data$year == yr, "mo"] <- unlist(airmass[,2])
  toa_data[toa_data$year == yr, "mc"] <- unlist(airmass[,3])
  toa_data[toa_data$year == yr, "ml"] <- unlist(airmass[,4])
  
  gc()
  
}
end_time <- Sys.time()
cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))


# compute long-wave radiation (needed to compute PET in Kaplan method),
# saturation vapour pressure, slope of vapour pressure curve, latent heat and psychrometric constant
cat("Computing long-wave radiation...\n")
ncores <- 20
start_time <- Sys.time()
with_progress({
  p <- progressor(along = unique(toa_data$year))
  for(yr in unique(toa_data$year)){
  # cat(paste0("Year ", yr, "\n"))
  
  b <- Sys.time()
  
  toa_data_temp <- toa_data[toa_data$year == yr,]
  gwgen_data_temp <- gwgen_data[gwgen_data$year == yr,]
  
  plan(multisession, workers = ncores)
  # p <- progressor(nrow(toa_data_temp))
  
  lw <- future_lapply(1:nrow(toa_data_temp), function(i){
    as.numeric(calculate_lw(toa_data_temp[i,], gwgen_data_temp[i,]))
    # p()
  })
  
  plan(sequential)
  
  lw <- do.call(rbind, lw)
  
  toa_data[toa_data$year == yr, "ss"] <- unlist(lw[,1])
  toa_data[toa_data$year == yr, "gamma"] <- unlist(lw[,2])
  toa_data[toa_data$year == yr, "lvap"] <- unlist(lw[,3])
  toa_data[toa_data$year == yr, "lw"] <- unlist(lw[,4])
  toa_data[toa_data$year == yr, "es"] <- unlist(lw[,5])
  toa_data[toa_data$year == yr, "TdewK"] <- unlist(lw[,6])
  
  gc()
  
  e <- Sys.time()
  p()
  # cat(paste0("Runtime: ", round(as.numeric(e-b)), "m \n"))
  
}})
end_time <- Sys.time()
cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))


# lw <- future_lapply(1:nrow(toa_data), function(i){
#   as.numeric(calculate_lw(toa_data[i,], gwgen_data[i,]))
# })
# lw <- do.call(rbind, lw)
# lw <- data.frame(lw)
# names(lw) <- c("ss", "gamma", "lvap", "lw", "es", "TdewK")
# toa_data <- cbind(toa_data, lw)

toa_data <- data.frame(toa_data)

# compute global radiation
cat("Compute global radiation...\n")
start_time <- Sys.time()
with_progress({
  p <- progressor(along = unique(toa_data$year))
  for(yr in unique(toa_data$year)){
  #cat(paste0("Year ", yr, "\n"))
  
  b <- Sys.time()
  
  toa_data_temp <- toa_data[toa_data$year == yr,]
  gwgen_data_temp <- gwgen_data[gwgen_data$year == yr,]
  
  # arbitrary correction term for cloudiness because of overestimated generated values by GWGEN
  # not necessarily the best way to do it...
  if(year >= 12000){
    gwgen_data_temp$mean_cloud <- 0.95*gwgen_data_temp$mean_cloud
  }else if(year < 12000){
    gwgen_data_temp$mean_cloud <- 0.92*gwgen_data_temp$mean_cloud
  }
  
  
  plan(multisession, workers = ncores)
  
  sw_pet <- future_lapply(1:nrow(toa_data_temp), function(i){
    
    if(pet_method == "kaplan"){ # not used for our models
      
      k <- 1
      # albedoLPJ <- 0.17 # surface shortwave albedo in LPJ_LMfire code
      albedoLPJ <- albedo <- toa_data_temp[i, "alb"]
      pet <- pet0 <- 0
      
      # compute global radiation
      sw_i <- calculate_glo_from_toa(toa_data_temp[i,], gwgen_data_temp[i,], pet, alb = albedo)
      
      # compute PET from net radiation (as in LPJ_LMfire)
      netrad <- (1 - albedoLPJ) * sw_i - toa_data_temp[i,"lw"]
      pet <- max((toa_data_temp[i,"ss"] / (toa_data_temp[i,"ss"] + toa_data_temp[i,"gamma"])) * netrad / toa_data_temp[i,"lvap"], 0)
      
      # J. Kaplan: "weak dependence between global radiation and PET, need to equilibrate them"
      while(abs(pet - pet0) > 0.01 & k < 100){
        
        pet0 <- pet
        
        # compute global radiation
        sw_i <- calculate_glo_from_toa(toa_data_temp[i,], gwgen_data_temp[i,], pet, alb = albedo)
        
        # compute PET from net radiation (as in LPJ_LMfire)
        netrad <- (1 - albedoLPJ) * sw_i - toa_data_temp[i,"lw"]
        pet <- max((toa_data_temp[i,"ss"] / (toa_data_temp[i,"ss"] + toa_data_temp[i,"gamma"])) * netrad / toa_data_temp[i,"lvap"], 0) 
        
        k <- k + 1
        
        if(k == 100){
          warning("Problem of convergence")
        }
        
      }
      
      # calculate pet PM
      albedoPM <- 0.23 # hypothetical reference crop with an albedo of 0.23
      
      # wind speed
      uz <- gwgen_data_temp[i,"wind"]
      z_meas <- 10
      u2 <- uz * 4.87/log(67.8 * z_meas - 5.42)
      
      # saturation vapour pressure
      es_Tmax <- 0.6108 * exp(17.27 * gwgen_data_temp[i,"tmax"]/(gwgen_data_temp[i,"tmax"] + 237.3))
      es_Tmin <- 0.6108 * exp(17.27 * gwgen_data_temp[i,"tmin"]/(gwgen_data_temp[i,"tmin"] + 237.3))
      es <- (es_Tmax + es_Tmin)/2
      
      # Tdew <- 34.07 + 4157 / log(2.1718e8 / es) - 273.15
      Tdew <- gwgen_data_temp[i,"tmin"] # approximation
      ea <- 0.6108 * exp((17.27 * Tdew)/(Tdew+237.3))
      
      # slope of vapour pressure curve (delta)
      ss <- toa_data_temp[i,"ss"]/1000 # convert to kPa/C
      
      # atmospheric pressure
      elev <- toa_data_temp[i, "alt"]
      P <- 101.3 * ((293 - 0.0065 * elev)/293)^5.26 
      
      # psychrometric constant
      gamma <- toa_data_temp[i,"gamma"]/1000 #convert to kPa/C
      
      # effect of cloudiness (relative shortwave radiation)
      # rs_rs0 <- (0.75 + (2 * 10^-5) * elev) * toa_data_temp[i,"toa"]/1000 # as in FAO
      rs_rs0 <- 1-0.29*(gwgen_data_temp[i,"mean_cloud"] + (gwgen_data_temp[i,"mean_cloud"])^2) # Antoine et al, 1996
      
      # global radiation
      r_s <- sw_i/1000 # convert to MJ/m2
      
      # estimated net outgoing longwave radiation
      r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
        ((gwgen_data_temp[i,"tmax"] + 273.2)^4 + (gwgen_data_temp[i,"tmin"] + 273.2)^4)/2 * 
        (1.35 * rs_rs0 - 0.35) 
      
      # net radiation
      r_ng <- (1 - albedoPM) * r_s  - r_nl
      
      # compute PET (Penman-Monteith equation) 
      petPM <- max((0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (gwgen_data_temp[i,"temp"] + 273.15))/
                   (ss + gamma * (1 + 0.34 * u2)),0)
      
      
    }
    else if(pet_method == "faoPM"){ 
      
      k <- 1
      albedoPM <- 0.23 # hypothetical reference crop with an albedo of 0.23
      albedo <- toa_data_temp[i, "alb"]
      pet <- pet0 <- 0
      
      # wind speed
      uz <- gwgen_data_temp[i,"wind"]
      z_meas <- 10
      u2 <- uz * 4.87/log(67.8 * z_meas - 5.42)
      
      # saturation vapour pressure
      es_Tmax <- 0.6108 * exp(17.27 * gwgen_data_temp[i,"tmax"]/(gwgen_data_temp[i,"tmax"] + 237.3))
      es_Tmin <- 0.6108 * exp(17.27 * gwgen_data_temp[i,"tmin"]/(gwgen_data_temp[i,"tmin"] + 237.3))
      es <- (es_Tmax + es_Tmin)/2
      
      # Tdew <- 34.07 + 4157 / log(2.1718e8 / es) - 273.15
      Tdew <- gwgen_data_temp[i,"tmin"] # approximation
      ea <- 0.6108 * exp((17.27 * Tdew)/(Tdew+237.3))
      
      # slope of vapour pressure curve (delta)
      # ss <- 4098 * (0.6108 * exp((17.27 * gwgen_data[i,"temp"])/(gwgen_data[i,"temp"] + 237.3)))/((gwgen_data[i,"temp"] + 237.3)^2)
      ss <- toa_data_temp[i,"ss"]/1000 # convert to kPa/C
      
      elev <- toa_data_temp[i, "alt"]
      # elev <- 10
      P <- 101.3 * ((293 - 0.0065 * elev)/293)^5.26 # atmospheric pressure
      
      # psychrometric constant
      #lambda <- 2.45
      #gamma <- 0.00163 * P/lambda
      gamma <- toa_data_temp[i,"gamma"]/1000 #convert to kPa/C
      
      # effect of cloudiness (relative shortwave radiation)
      # rs0 <- (0.75 + (2 * 10^-5) * elev) * toa_data_temp[i,"toa"]/1000 # as in FAO
      rs_rs0 <- 1-0.29*(gwgen_data_temp[i,"mean_cloud"] + (gwgen_data_temp[i,"mean_cloud"])^2) # Antoine et al, 1996
      # rs_rs0 <- 1 - gwgen_data_temp[i,"mean_cloud"]
      
      # compute global radiation
      sw_i <- calculate_glo_from_toa(toa_data_temp[i,], gwgen_data_temp[i,], pet, alb = albedo)
      r_s <- sw_i/1000 # convert to MJ/m2
      
      # estimated net outgoing longwave radiation
      r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
        ((gwgen_data_temp[i,"tmax"] + 273.2)^4 + (gwgen_data_temp[i,"tmin"] + 273.2)^4)/2 * 
        (1.35 * rs_rs0 - 0.35) 
      
      # net radiation
      r_ng <- (1 - albedo) * r_s  - r_nl
      
      # compute PET (Penman-Monteith equation) 
      pet <- max((0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (gwgen_data_temp[i,"temp"] + 273.15))/
                   (ss + gamma * (1 + 0.34 * u2)),0)
      
      while(abs(pet - pet0) > 0.01 & k < 10000){
        pet0 <- pet
        
        # compute global radiation and pet 
        sw_i <- calculate_glo_from_toa(toa_data_temp[i,], gwgen_data_temp[i,], pet, alb = albedo)
        r_s <- sw_i/1000 # convert to MJ/m2
        r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
          ((gwgen_data_temp[i,"tmax"] + 273.2)^4 + (gwgen_data_temp[i,"tmin"] + 273.2)^4)/2 * 
          (1.35 * rs_rs0 - 0.35) # estimated net outgoing longwave radiation
        r_ng <- (1 - albedo) * r_s - r_nl # net radiation
        pet <- max((0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (gwgen_data_temp[i,"temp"] + 273.15))/
                     (ss + gamma * (1 + 0.34 * u2)),0)
        
        k <- k + 1
        
        if(k == 100){
          warning("Problem of convergence")
        }
        
      }
      
      netrad <- r_ng
      
      # calculate FAO PM pet
      r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
        ((gwgen_data_temp[i,"tmax"] + 273.2)^4 + (gwgen_data_temp[i,"tmin"] + 273.2)^4)/2 * 
        (1.35 * rs_rs0 - 0.35) # estimated net outgoing longwave radiation
      r_ng <- (1 - albedoPM) * r_s - r_nl # net radiation
      petPM <- max((0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (gwgen_data_temp[i,"temp"] + 273.15))/
                   (ss + gamma * (1 + 0.34 * u2)),0)
      
    }
    
    return(c(glo = sw_i, pet = pet, petPM = petPM, netrad = netrad))
    
  })
  
  
  plan(sequential)
  
  sw_pet <- do.call(rbind, sw_pet)
  
  gwgen_data[gwgen_data$year == yr, "glo"] <- unlist(sw_pet[,1])
  gwgen_data[gwgen_data$year == yr, "pet"] <- unlist(sw_pet[,2])
  gwgen_data[gwgen_data$year == yr, "petPM"] <- unlist(sw_pet[,3])
  gwgen_data[gwgen_data$year == yr, "netrad"] <- unlist(sw_pet[,4])
  
  gc()
  
  e <- Sys.time()
  p()
  # cat(paste0("Runtime: ", round(as.numeric(e-b)), "m \n"))
  
}})
end_time <- Sys.time()
cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
gwgen_data$glo <- round((gwgen_data$glo/1000),4) #convert to MJ.m-2
gwgen_data$pet <- round(gwgen_data$pet, 4)

# write data in output file
fwrite(gwgen_data, file = output_file, col.names = T, row.names = FALSE, sep=",")

plan(sequential)
rm(gwgen_data, gwgen_data_temp, sw_pet, toa, toa_data, toa_data_temp, lw, airmass, 
   input_data, peqin_data, warmest_month, orbit, peqin, coldest_month, alb, alt)
gc()

message("Daily radiation computed !")

glo_file <- output_file
