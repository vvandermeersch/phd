
monthly_downscale <- function(yr_interval, in_folder, out_folder, 
                                            extent, WHC_present,
                                            ncores, foc_extrapol = FALSE){
  
  time_slice <- paste0(yr_interval[2], "_", yr_interval[1], "kyr")
  years_list <- split(yr_interval[2]:yr_interval[1], ceiling(seq_along(yr_interval[2]:yr_interval[1])/500))[1:4]
  if(yr_interval[1] == 0){years_list[["4"]] <- 500:0} # add 0BP
  
  # do the downscaling by 500-year time slice (//ICE-6G-C temporal resolution)
  for(l in years_list){
    
    yrb <- l[length(l)]
    yre <- l[1]
    rmonths <- year_to_months(yrb, yre, yr_interval[2]) 
    
    cat(paste0("Doing time slice ", yre,"-", yrb, " BP"))
    
    cat("Loading and cropping data")
    start_time <- Sys.time()
    alb_LR <- crop(rast(file.path(in_folder, "albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                   paste0("albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", time_slice, ".nc")),
                         subds = "albedos", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    pre_LR <- crop(rast(file.path(in_folder, "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                                  paste0("precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", time_slice, ".nc")),
                        subds = "precip_mm_srf", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    cld_LR <- crop(rast(file.path(in_folder, "totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                                  paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", time_slice, ".nc")),
                        subds = "totCloud_mm_ua", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    uwind_LR <- crop(rast(file.path(in_folder, "u_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                  paste0("u_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", time_slice, ".nc")),
                        subds = "u_mm_10m", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    vwind_LR <- crop(rast(file.path(in_folder, "v_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                  paste0("v_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", time_slice, ".nc")),
                        subds = "v_mm_10m", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    end_time <- Sys.time()
    cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    
    # load ICE-6G-C altitude (500yr slice)
    yr_ICE6G <- yre%/%501*0.5
    alt_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "Orog")), extent)
    ldmsk_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftlf")), extent)
    ldmsk_HR[ldmsk_HR == 0] <- NA
    alt_HR <- mask(alt_HR, ldmsk_HR)
    
    # altitude at low resolution
    alt_LR <- aggregate(alt_HR, 3, na.rm = T)
    alt_LR <- resample(alt_LR, subset(pre_LR,1))
    
    # process WHC
    WHC_natres <- rast(WHC_present[,c(2,1,3)])
    WHC_LR <- resample(WHC_natres, alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    
    # remove cells where we do not have WHC data
    alt_LR <- mask(alt_LR, WHC_LR)
    alb_LR <- mask(alb_LR, WHC_LR)
    pre_LR <- mask(pre_LR, WHC_LR)
    cld_LR <- mask(cld_LR, WHC_LR)
    uwind_LR <- mask(uwind_LR, WHC_LR)
    vwind_LR <- mask(vwind_LR, WHC_LR)
    
    # 0. Extrapolate coastal cells (< 1 hour)
    if(foc_extrapol){
      cat("Extrapolating coastal cells\n")
      start_time <- Sys.time()
      alb_LR <- mask(focal(alb_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      pre_LR <- mask(focal(pre_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      cld_LR <- mask(focal(cld_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      uwind_LR <- mask(focal(uwind_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      vwind_LR <- mask(focal(vwind_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      end_time <- Sys.time()
      cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    }
    
    # 1. Interpolate coarse variables to the high-resolution
    alb_LR_HR <- resample(alb_LR, alt_HR)
    pre_LR_HR <- resample(pre_LR, alt_HR)
    cld_LR_HR <- resample(cld_LR, alt_HR)
    uwind_LR_HR <- resample(uwind_LR, alt_HR)
    vwind_LR_HR <- resample(vwind_LR, alt_HR)
    
    
    # 2. Write data on disk
    cat("Writing final rasters\n")
    start_time <- Sys.time()
    time_slice_l <- paste0(yre, "_", yrb, "kyr")
    writeRaster(alb_LR_HR, file.path(out_folder, "albedos_old_sims_1yrAvg_monthly_10minRes_noBias_Europe_24000_0kyr",
                                   paste0("albedos_old_sims_1yrAvg_monthly_10minRes_noBias_Europe_", time_slice_l, ".nc")),
                overwrite=TRUE)
    writeRaster(pre_LR_HR, file.path(out_folder, "precip_mm_srf_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_24000_0kyr",
                                   paste0("precip_mm_srf_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_", time_slice_l, ".nc")),
                overwrite=TRUE)
    writeRaster(cld_LR_HR, file.path(out_folder, "totCloud_mm_ua_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_24000_0kyr",
                                   paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_10minRes_CRU_Europe_", time_slice_l, ".nc")),
                overwrite=TRUE)
    writeRaster(uwind_LR_HR, file.path(out_folder, "u_mm_10m_old_sims_1yrAvg_monthly_10minRes_noBias_Europe_24000_0kyr",
                                   paste0("u_mm_10m_old_sims_1yrAvg_monthly_10minRes_noBias_Europe_", time_slice_l, ".nc")),
                overwrite=TRUE)
    writeRaster(vwind_LR_HR, file.path(out_folder, "v_mm_10m_old_sims_1yrAvg_monthly_10minRes_noBias_Europe_24000_0kyr",
                                   paste0("v_mm_10m_old_sims_1yrAvg_monthly_10minRes_noBias_Europe_", time_slice_l, ".nc")),
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