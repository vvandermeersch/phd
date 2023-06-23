
merge_rasters <- function(periods, folder){
  
  alb_1 <- rast(file.path(folder, "albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[1], ".nc")))
  cld_1 <- rast(file.path(folder, "totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[1], ".nc")))
  uwind_1 <- rast(file.path(folder, "u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[1], ".nc")))
  vwind_1 <- rast(file.path(folder, "v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[1], ".nc")))
  rd3_1 <- rast(file.path(folder, "rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[1], ".nc")))
  tmin_1 <- rast(file.path(folder, "tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[1], ".nc")))
  tmax_1 <- rast(file.path(folder, "tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[1], ".nc")))
  pre_1 <- rast(file.path(folder, "precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[1], ".nc")))
  alt_1 <- rast(file.path(folder, "altitude_ICE6GC_15minRes_Europe_24000_0kyr",
                          paste0("altitude_ICE6GC_15minRes_Europe_", periods[1], ".nc")))
  whc_1 <- rast(file.path(folder, "WHC_present_15minRes_Europe_24000_0kyr",
                          paste0("WHC_present_15minRes_Europe_", periods[1], ".nc")))
  
  
  alb_2 <- mask(rast(file.path(folder, "albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[2], ".nc"))), alt_1)
  cld_2 <- mask(rast(file.path(folder, "totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[2], ".nc"))), alt_1)
  uwind_2 <- mask(rast(file.path(folder, "u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[2], ".nc"))), alt_1)
  vwind_2 <- mask(rast(file.path(folder, "v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[2], ".nc"))), alt_1)
  rd3_2 <- mask(rast(file.path(folder, "rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr", 
                          paste0("rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[2], ".nc"))), alt_1)
  tmin_2 <- mask(rast(file.path(folder, "tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[2], ".nc"))), alt_1)
  tmax_2 <- mask(rast(file.path(folder, "tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[2], ".nc"))), alt_1)
  pre_2 <- mask(rast(file.path(folder, "precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr", 
                          paste0("precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[2], ".nc"))), alt_1)
  alt_2 <- mask(rast(file.path(folder, "altitude_ICE6GC_15minRes_Europe_24000_0kyr",
                          paste0("altitude_ICE6GC_15minRes_Europe_", periods[2], ".nc"))), alt_1)
  whc_2 <- mask(rast(file.path(folder, "WHC_present_15minRes_Europe_24000_0kyr",
                          paste0("WHC_present_15minRes_Europe_", periods[2], ".nc"))), alt_1)
  
  alb_1 <- mask(alb_1, alt_2)
  cld_1 <- mask(cld_1, alt_2)
  uwind_1 <- mask(uwind_1, alt_2)
  vwind_1 <- mask(vwind_1, alt_2)
  rd3_1 <- mask(rd3_1, alt_2)
  tmin_1 <- mask(tmin_1, alt_2)
  tmax_1 <- mask(tmax_1, alt_2)
  pre_1 <- mask(pre_1, alt_2)
  alt_1 <- mask(alt_1, alt_2)
  whc_1 <- mask(whc_1, alt_2)
  
  alb <- c(alb_1, alb_2)
  cld <- c(cld_1, cld_2)
  uwind <- c(uwind_1, uwind_2)
  vwind <- c(vwind_1, vwind_2)
  rd3 <- c(rd3_1, rd3_2)
  tmin <- c(tmin_1, tmin_2)
  tmax <- c(tmax_1, tmax_2)
  pre <- c(pre_1, pre_2)
  alt <- mean(alt_1, alt_2)
  whc <- mean(whc_1, whc_2)
  
  writeCDF(alb, file.path(folder, "albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                paste0("albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[3], ".nc")),
           varname = "albedos", overwrite=TRUE)
  writeCDF(cld, file.path(folder, "totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr",
                                paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[3], ".nc")),
           varname = "totCloud_mm_ua", overwrite=TRUE)
  writeCDF(uwind, file.path(folder, "u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                  paste0("u_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[3], ".nc")),
           varname = "u_mm_10m", overwrite=TRUE)
  writeCDF(vwind, file.path(folder, "v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                  paste0("v_mm_10m_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[3], ".nc")),
           varname = "v_mm_10m", overwrite=TRUE)
  writeCDF(rd3, file.path(folder, "rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                                paste0("rd3_mm_srf_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", periods[3], ".nc")),
           varname = "rd3_mm_srf", overwrite=TRUE)
  writeCDF(alt, file.path(folder, "altitude_ICE6GC_15minRes_Europe_24000_0kyr",
                             paste0("altitude_ICE6GC_15minRes_Europe_", periods[3], ".nc")),
           varname = "altitude", overwrite=TRUE)
  writeCDF(whc, file.path(folder, "WHC_present_15minRes_Europe_24000_0kyr",
                                paste0("WHC_present_15minRes_Europe_", periods[3], ".nc")),
           varname = "whc", overwrite=TRUE)
  writeCDF(tmin, file.path(folder, "tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr",
                              paste0("tempmin_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[3], ".nc")),
           varname = "tempmin_av", overwrite=TRUE)
  
  writeCDF(tmax, file.path(folder, "tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr",
                              paste0("tempmax_av_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[3], ".nc")),
           varname = "tempmax_av", overwrite=TRUE)
  writeCDF(pre, file.path(folder, "precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_24000_0kyr",
                             paste0("precip_mm_srf_old_sims_1yrAvg_monthly_15minRes_CRU_Europe_", periods[3], ".nc")),
           varname = "precip_mm_srf", overwrite=TRUE)
  
}
