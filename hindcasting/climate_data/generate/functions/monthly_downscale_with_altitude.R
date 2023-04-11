
monthly_downscale_with_altitude <- function(yr_interval, in_folder, out_folder, 
                                            extent, WHC_present,
                                            ncores, foc_extrapol = FALSE){
  
  time_slice <- paste0(yr_interval[2], "_", yr_interval[1], "kyr")
  years_list <- split(yr_interval[2]:yr_interval[1], ceiling(seq_along(yr_interval[2]:yr_interval[1])/500))[1:4]
  if(yr_interval[1] == 0){years_list[["4"]] <- 500:0} # add 0BP
  
  # create downscaled raster
  r_tmin_HR <- rast()
  r_tmax_HR <- rast()
  
  # do the downscaling by 500-year time slice (//ICE-6G-C temporal resolution)
  for(l in years_list){
    
    yrb <- l[length(l)]
    yre <- l[1]
    rmonths <- year_to_months(yrb, yre, yr_interval[2]) 
    
    cat(paste0("Doing time slice ", yre,"-", yrb, " BP"))
    
    cat("Loading and cropping data")
    start_time <- Sys.time()
    tmin_LR <- crop(rast(file.path(in_folder, "tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                             paste0("tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", time_slice, ".nc")),
                   subds = "tempmin_av", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    tmax_LR <- crop(rast(file.path(in_folder, "tempmax_av_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                             paste0("tempmax_av_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", time_slice, ".nc")),
                   subds = "tempmax_av", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    end_time <- Sys.time()
    cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    
    # load ICE-6G-C altitude
    yr_ICE6G <- yre%/%501*0.5
    alt_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "Orog")), extent)
    ldmsk_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftlf")), extent)
    ldmsk_HR[ldmsk_HR == 0] <- NA
    alt_HR <- mask(alt_HR, ldmsk_HR)
    
    # altitude at low resolution
    alt_LR <- aggregate(alt_HR, 3, na.rm = T)
    alt_LR <- resample(alt_LR, subset(tmin_LR,1))
    
    # process WHC
    WHC_natres <- rast(WHC_present[,c(2,1,3)])
    WHC_LR <- resample(WHC_natres, alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    
    # remove cells where we do not have WHC data
    alt_LR <- mask(alt_LR, WHC_LR)
    tmin_LR <- mask(tmin_LR, WHC_LR)
    tmax_LR <- mask(tmax_LR, WHC_LR)
    
    # 0. Create rasters
    tmax_reg_LR <- rast()
    tmin_reg_LR <- rast()
    
    # 0. Extrapolate coastal cells (< 1 hour)
    if(foc_extrapol){
      cat("Extrapolating coastal cells\n")
      start_time <- Sys.time()
      tmax_LR <- mask(focal(tmax_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      tmin_LR <- mask(focal(tmin_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      end_time <- Sys.time()
      cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    }
    
    # 0. Wrap rasters to pass them over a connection that serializes
    .alt_LR <- wrap(alt_LR)
    .tmax_LR <- wrap(tmax_LR)
    .tmin_LR <- wrap(tmin_LR)
    
    # 1. Quantify the change with height at the native coarse resolution 
    plan(multisession, workers = ncores)
    cat("Computing max. temperature regressions\n") # (> 2 hours)
    start_time <- Sys.time()
    tmax_reg_LR <- future_lapply(1:dim(tmax_LR)[3], function(d){
      alt_LRw <- rast(.alt_LR) # load packed altitude
      tmax_LRw <- rast(.tmax_LR) # load packed climate
      rd <- subset(tmax_LRw, d) # layer for day d
      regd <- rd # raster to keep regression coef.
      regd[] <- NA
      regcoef <- sapply(cells(rd), .compute_reg_coef, r = rd, ralt = alt_LRw)
      regd[cells(rd)] <- as.numeric(regcoef)
      .regd <- wrap(regd) # pack raster
      return(.regd)
    }, future.seed=TRUE)
    tmax_reg_LR <- rast(lapply(tmax_reg_LR, rast))
    end_time <- Sys.time()
    cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    cat("Computing min. temperature regressions\n") # (> 2 hours)
    start_time <- Sys.time()
    tmin_reg_LR <- future_lapply(1:dim(tmin_LR)[3], function(d){
      alt_LRw <- rast(.alt_LR) # load packed altitude
      tmin_LRw <- rast(.tmin_LR) # load packed climate
      rd <- subset(tmin_LRw, d) # layer for day d
      regd <- rd # raster to keep regression coef.
      regd[] <- NA
      regcoef <- sapply(cells(rd), .compute_reg_coef, r = rd, ralt = alt_LRw)
      regd[cells(rd)] <- as.numeric(regcoef)
      .regd <- wrap(regd) # pack raster
      return(.regd)
    }, future.seed=TRUE)
    tmin_reg_LR <- rast(lapply(tmin_reg_LR, rast))
    end_time <- Sys.time()
    cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    plan(sequential)
    gc(verbose = FALSE)
    
    # 2. Interpolate the slope to the high-resolution grid
    tmax_reg_LR_HR <- resample(tmax_reg_LR, alt_HR)
    tmin_reg_LR_HR <- resample(tmin_reg_LR, alt_HR)
    
    # 3. Interpolate coarse variables to the high-resolution
    tmax_LR_HR <- resample(tmax_LR, alt_HR)
    tmin_LR_HR <- resample(tmin_LR, alt_HR)
    
    # 4. Interpolate coarse elevation field at high resolution
    alt_LR_HR <- resample(alt_LR, alt_HR)
    
    # 5. Apply height correction 
    cat("Applying height correction\n")
    tmax_HR <- tmax_LR_HR + tmax_reg_LR_HR * (alt_HR - alt_LR_HR)
    tmin_HR <- tmin_LR_HR + tmin_reg_LR_HR * (alt_HR - alt_LR_HR)
    
    # 6. Write data on disk
    cat("Writing final rasters\n")
    start_time <- Sys.time()
    time_slice_l <- paste0(yre, "_", yrb, "kyr")
    writeRaster(tmax_HR, file.path(out_folder, "tempmin_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_24000_0kyr",
                                     paste0("tempmin_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_", time_slice_l, ".nc")),
             overwrite=TRUE)

    writeRaster(tmin_HR, file.path(out_folder, "tempmax_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_24000_0kyr",
                                     paste0("tempmax_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_", time_slice_l, ".nc")),
             overwrite=TRUE)
    end_time <- Sys.time()
    cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    
    
    #r_tmin_HR <- c(r_tmin_HR, tmin_HR)
    #r_tmax_HR <- c(r_tmax_HR, tmax_HR)
    
  }
  
  # cat("Writing final rasters\n")
  # writeRaster(r_tmin_HR, file.path(out_folder, "tempmin_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_24000_0kyr", 
  #                                  paste0("2tempmin_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_", time_slice, ".nc")),
  #          overwrite=TRUE)
  # 
  # writeCDF(r_tmax_HR, file.path(out_folder, "tempmax_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_24000_0kyr", 
  #                                  paste0("tempmax_av_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_", time_slice, ".nc")),
  #          overwrite=TRUE)
              
              
  
  
  
  
  
  
  
  
  
  
  
  
}