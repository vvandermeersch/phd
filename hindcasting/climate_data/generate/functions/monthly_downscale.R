
monthly_downscale <- function(yr_interval, years_list = NULL,
                              in_folder, out_folder, 
                              extent, WHC_present,
                              foc_extrapol = FALSE){
  
  time_slice <- paste0(yr_interval[2], "_", yr_interval[1], "kyr")
  
  # first version, downscaling by 500yr time slice
  # years_list <- split(yr_interval[2]:yr_interval[1], ceiling(seq_along(yr_interval[2]:yr_interval[1])/500))[1:4]
  # if(yr_interval[1] == 0){years_list[["4"]] <- 500:0} # add 0BP
  
  # do the downscaling
  for(l in years_list){
    
    yrb <- l[length(l)]
    yre <- l[1]
    rmonths <- .year_to_months(yrb, yre, yr_interval[2]) 
    
    cat(paste0("Doing time slice ", yre,"-", yrb, " BP \n"))
    
    cat("Loading and cropping data \n") # ~ 7-8min
    start_time <- Sys.time()
    alb_LR <- crop(rast(file.path(in_folder, "albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                  paste0("albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", time_slice, ".nc")),
                        subds = "albedos", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    alb_LR[alb_LR > 0.8] <- 0.8 # E. Armstrong: rare strange values because of monthly varying mask in HadCM3b: convert anything >0.8 
    cld_LR <- crop(rast(file.path(in_folder, "totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                                  paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", time_slice, ".nc")),
                        subds = "totCloud_mm_ua", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    uwind_LR <- crop(rast(file.path(in_folder, "u_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                    paste0("u_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", time_slice, ".nc")),
                          subds = "u_mm_10m", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    vwind_LR <- crop(rast(file.path(in_folder, "v_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                    paste0("v_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", time_slice, ".nc")),
                          subds = "v_mm_10m", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    rd3_LR <- crop(rast(file.path(in_folder, "rd3_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                  paste0("rd3_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", time_slice, ".nc")),
                        subds = "rd3_mm_srf", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
    end_time <- Sys.time()
    cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    
    # load ICE-6G-C altitude (500yr slice)
    yr_ICE6G <- yre%/%500*0.5
    alt_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "Orog")), extent)
    ldmsk_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftlf")), extent)
    ldmsk_HR[ldmsk_HR == 0] <- NA
    alt_HR <- mask(alt_HR, ldmsk_HR)
    
    # altitude at low resolution
    alt_LR <- aggregate(alt_HR, 3, na.rm = T)
    alt_LR <- resample(alt_LR, subset(cld_LR,1))
    
    # reduce high-resolution
    temp <- alt_HR
    res(temp) <- c(0.25, 0.25)
    alt_HR <- resample(alt_HR, temp)
    
    # process WHC
    WHC_natres <- rast(WHC_present[,c(2,1,3)])
    WHC_LR <- resample(WHC_natres, alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    WHC_LR <- mask(focal(WHC_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    
    # remove cells where we do not have WHC data
    alt_LR <- mask(alt_LR, WHC_LR)
    alb_LR <- mask(alb_LR, WHC_LR)
    cld_LR <- mask(cld_LR, WHC_LR)
    uwind_LR <- mask(uwind_LR, WHC_LR)
    vwind_LR <- mask(vwind_LR, WHC_LR)
    rd3_LR <- mask(rd3_LR, WHC_LR)
    
    # 0. Extrapolate coastal cells
    if(foc_extrapol){
      cat("Extrapolating coastal cells\n")
      start_time <- Sys.time()
      alb_LR <- mask(focal(alb_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      cld_LR <- mask(focal(cld_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      uwind_LR <- mask(focal(uwind_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      vwind_LR <- mask(focal(vwind_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      rd3_LR <- mask(focal(rd3_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
      end_time <- Sys.time()
      # cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    }
    
    # 1. Interpolate coarse variables to the high-resolution
    alb_LR_HR <- mask(resample(alb_LR, alt_HR), alt_HR)
    cld_LR_HR <- mask(resample(cld_LR, alt_HR), alt_HR)
    uwind_LR_HR <- mask(resample(uwind_LR, alt_HR), alt_HR)
    vwind_LR_HR <- mask(resample(vwind_LR, alt_HR), alt_HR)
    rd3_LR_HR <- mask(resample(rd3_LR, alt_HR), alt_HR)
    WHC_LR_HR <- mask(resample(WHC_LR, alt_HR), alt_HR)
    
    # 1bis. Removing useless cells
    alt_HR <- mask(alt_HR, subset(cld_LR_HR,1))
    WHC_LR_HR <- mask(WHC_LR_HR, subset(cld_LR_HR,1))
    
    # 2. Write data on disk
    cat("Writing final rasters\n")
    start_time <- Sys.time()
    time_slice_l <- paste0(yre, "_", yrb, "kyr")
    writeCDF(alb_LR_HR, file.path(out_folder, "albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                  paste0("albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", time_slice_l, ".nc")),
             varname = "albedos", overwrite=TRUE)
    writeCDF(cld_LR_HR, file.path(out_folder, "totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr",
                                  paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", time_slice_l, ".nc")),
             varname = "totCloud_mm_ua", overwrite=TRUE)
    writeCDF(uwind_LR_HR, file.path(out_folder, "u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                    paste0("u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", time_slice_l, ".nc")),
             varname = "u_mm_10m", overwrite=TRUE)
    writeCDF(vwind_LR_HR, file.path(out_folder, "v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                    paste0("v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", time_slice_l, ".nc")),
             varname = "v_mm_10m", overwrite=TRUE)
    writeCDF(rd3_LR_HR, file.path(out_folder, "rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                  paste0("rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", time_slice_l, ".nc")),
             varname = "rd3_mm_srf", overwrite=TRUE)
    writeCDF(alt_HR, file.path(out_folder, "altitude_ICE6GC_15minRes_Europe_24000_0kyr",
                               paste0("altitude_ICE6GC_15minRes_Europe_", time_slice_l, ".nc")),
             varname = "altitude", overwrite=TRUE)
    writeCDF(WHC_LR_HR, file.path(out_folder, "WHC_present_15minRes_Europe_24000_0kyr",
                                  paste0("WHC_present_15minRes_Europe_", time_slice_l, ".nc")),
             varname = "whc", overwrite=TRUE)
    end_time <- Sys.time()
    cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
    
  }
  
}

#extract months
.year_to_months <- function(yr_beg, yr_end, max_year){
  
  min_mn <- ((max_year-yr_end)+1)*12-11
  max_mn <- ((max_year-yr_beg)+1)*12
  
  return(list(min = min_mn, max = max_mn))
  
}

