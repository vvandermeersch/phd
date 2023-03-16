# function to downscale temperatures and radiation

# inspired by a Copernicus Climate Change Service (CCCS) method
# use the 8 neighbor cells to apply a height correction 

# author : V. Van der Meersch - 20/10/2022

downscale_with_altitude <- function(phenofit_file_pattern, hr_alt, in_folder, out_folder, ncores){
  
  plan(multisession, workers = ncores)
  
  tmax <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_tmx_", yr, "_dly.fit"))))
  tmin <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_tmn_", yr, "_dly.fit"))))
  tmean <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_tmp_", yr, "_dly.fit"))))
  glo <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_glo_", yr, "_dly.fit"))))
  
  # Load data
  # alt <- fread(file.path(in_folder, "HadCM3B_Altitude.fit"))
  # alt <- rast(alt[, c(2,1,3)])
  alt <- resample(hr_alt, rast(tmax[, c(2,1)]))
  .alt <- wrap(alt)
  
  # High res altitude to raster format
  # hr_alt <- rasterFromXYZ(hr_alt[, c(2,1,3)])
  
  # Create rasters
  tmax_reg <- rast()
  tmin_reg <- rast()
  tmean_reg <- rast()
  glo_reg <- rast()
  
  # Create rasters for the loop
  tmax_regd <- alt
  tmin_regd <- alt
  tmean_regd <- alt
  glo_regd <- alt
  
  
  # 1. quantify the change with height at the native coarse resolution
  for(day in 3:ncol(tmax)){ # sequential loop on days
    
    tmax_d <- rast(tmax[, c(2,1,day)])
    tmin_d <- rast(tmin[, c(2,1,day)])
    tmean_d <- rast(tmean[, c(2,1,day)])
    glo_d <- rast(glo[, c(2,1,day)])
    
    ##### JUST TEMPORARY ######
    tmax_d <- mask(tmax_d, alt)
    tmin_d <- mask(tmin_d, alt)
    tmean_d <- mask(tmean_d, alt)
    glo_d <- mask(glo_d, alt)
    ##########################
    
    # create objects that can be passed over a serialized connection
    .tmax_d <- wrap(tmax_d)
    .tmin_d <- wrap(tmin_d)
    .tmean_d <- wrap(tmean_d)
    .glo_d <- wrap(glo_d)
    
    
    
    coef_linreg <- foreach(fcell = 1:ncell(tmax_d)) %dopar% { # parallel loop on raster cells
      tmax_d <- rast(.tmax_d)
      tmin_d <- rast(.tmin_d)
      tmean_d <- rast(.tmean_d)
      glo_d <- rast(.glo_d)
      alt <- rast(.alt)
      if(!is.na(tmax_d[fcell])){
        ind_ncells <- as.numeric(terra::adjacent(tmax_d, fcell, 8, include = T))
        tmax_linreg <- lm(unlist(tmax_d[ind_ncells]) ~ unlist(alt[ind_ncells])) # linear regression for maximum temperature
        tmin_linreg <- lm(unlist(tmin_d[ind_ncells]) ~ unlist(alt[ind_ncells])) # minimum temperature
        tmean_linreg <- lm(unlist(tmean_d[ind_ncells]) ~ unlist(alt[ind_ncells])) # mean temperature
        glo_linreg <- lm(unlist(glo_d[ind_ncells]) ~ unlist(alt[ind_ncells])) # radiation
        c(as.numeric(tmax_linreg$coefficients[2]), as.numeric(tmin_linreg$coefficients[2]),
          as.numeric(tmean_linreg$coefficients[2]), as.numeric(glo_linreg$coefficients[2]))
      }else{
        c(NA, NA, NA, NA)
      }
    }
    coef_linreg <- do.call("rbind", coef_linreg)
    tmax_regd [] <- coef_linreg[,1]
    tmin_regd[] <- coef_linreg[,2]
    tmean_regd[] <- coef_linreg[,3]
    glo_regd[] <- coef_linreg[,4]
    tmax_reg <- c(tmax_reg, tmax_regd)
    tmin_reg <- c(tmin_reg, tmin_regd)
    tmean_reg <- c(tmean_reg, tmean_regd)
    glo_reg <- c(glo_reg, glo_regd)
    
  }
  
  # 2. interpolate the slope to the high-resolution grid
  hr_tmax_reg <- resample(tmax_reg, hr_alt)
  hr_tmin_reg <- resample(tmin_reg, hr_alt)
  hr_tmean_reg <- resample(tmean_reg, hr_alt)
  hr_glo_reg <- resample(glo_reg, hr_alt)
  
  # 3. interpolate the coarse variable to the high-resolution
  tmax_rast <- rast(lapply(3:ncol(tmax), function(day) rast(tmax[, c(2,1,day)])))
  tmin_rast <- rast(lapply(3:ncol(tmin), function(day) rast(tmin[, c(2,1,day)])))
  tmean_rast <- rast(lapply(3:ncol(tmax), function(day) rast(tmean[, c(2,1,day)])))
  glo_rast <- rast(lapply(3:ncol(glo), function(day) rast(glo[, c(2,1,day)])))
  hr_tmax <- resample(tmax_rast, hr_alt)
  hr_tmin <- resample(tmin_rast, hr_alt)
  hr_tmean <- resample(tmean_rast, hr_alt)
  hr_glo <- resample(glo_rast, hr_alt)
  
  # 4. create coarse elevation field at high resolution
  coarse_alt_hr <- resample(alt, hr_alt)
  
  # 5. apply height correction 
  hr_tmax_cor <- hr_tmax + hr_tmax_reg * (hr_alt - coarse_alt_hr)
  hr_tmin_cor <- hr_tmin + hr_tmin_reg * (hr_alt - coarse_alt_hr)
  hr_tmean_cor <- hr_tmean + hr_tmean_reg * (hr_alt - coarse_alt_hr)
  hr_glo_cor <- hr_glo + hr_glo_reg * (hr_alt - coarse_alt_hr)
  
  # 6. transform to matrix
  tmax_data <- lapply(1:nlayers(hr_tmax_cor), function(i){na.omit(as.data.frame(subset(hr_tmax_cor, i)))})
  tmin_data <- lapply(1:nlayers(hr_tmin_cor), function(i){na.omit(as.data.frame(subset(hr_tmin_cor, i)))})
  tmean_data <- lapply(1:nlayers(hr_tmean_cor), function(i){na.omit(as.data.frame(subset(hr_tmean_cor, i)))})
  glo_data <- lapply(1:nlayers(hr_glo_cor), function(i){na.omit(as.data.frame(subset(hr_glo_cor, i)))})
  tmax <- do.call(cbind, tmax_data)
  tmin <- do.call(cbind, tmin_data)
  tmean <- do.call(cbind, tmean_data)
  glo <- do.call(cbind, glo_data)
  # get lat and lon
  lonlat <- as.data.frame(subset(hr_tmax_cor, 1), xy = T)
  latlon <- na.omit(lonlat)[, c(2,1)]
  
  # 7. create files
  tmin_file <- create_phenofit_file(yr, var = "tmn", out_folder)
  tmax_file <- create_phenofit_file(yr, var = "tmx", out_folder)
  tmean_file <- create_phenofit_file(yr, var = "tmp", out_folder)
  glo_file <- create_phenofit_file(yr, var = "glo", out_folder)
  
  # Write files
  write.table(cbind(latlon, tmin), file = tmin_file, append = T, sep= "\t", row.names = F, col.names = F)
  write.table(cbind(latlon, tmax), file = tmax_file, append = T, sep= "\t", row.names = F, col.names = F)
  write.table(cbind(latlon, tmean), file = tmean_file, append = T, sep= "\t", row.names = F, col.names = F)
  write.table(cbind(latlon, glo), file = glo_file, append = T, sep= "\t", row.names = F, col.names = F)
  
  plan(sequential)
  
}