load_raster <- function(yr, var, dir, wnd = 15){
  
  years <- c(yr - wnd, yr + wnd)
  
  if(var == "pre"){
    var_fd <- "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"
    var_suf <- "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_"
    var <- "precip_mm_srf"
  }else if(var == "temp"){
    var_fd <- "temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"
    var_suf <- "temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_"
    var <- "temp_mm_1_5m"
  }
  
  if(yr %in% seq(20000, 2000, -2000)){
    # data in 2 netcdf files
    file_spec <- years_to_file(years[1])
    rmonths <- year_to_months(years[1], file_spec$max, file_spec$max)
    r_var1 <- rast(file.path(dir, var_fd, 
                            paste0(var_suf, file_spec$name, ".nc")),
                  subds = var, lyrs = rmonths$min:rmonths$max, 
                  opts="HONOUR_VALID_RANGE=NO")
    file_spec <- years_to_file(years[2])
    rmonths <- year_to_months(file_spec$min, years[2], file_spec$max)
    r_var2 <- rast(file.path(dir, var_fd, 
                             paste0(var_suf, file_spec$name, ".nc")),
                   subds = var, lyrs = rmonths$min:rmonths$max, 
                   opts="HONOUR_VALID_RANGE=NO")
    r_var <- c(r_var2, r_var1)
  }else{
    # data in one netcdf file
    file_spec <- years_to_file(years)
    rmonths <- year_to_months(years[1], years[2], file_spec$max)
    r_var <- rast(file.path(data_dir, var_fd, 
                             paste0(var_suf, file_spec$name, ".nc")),
                   subds = var, lyrs = rmonths$min:rmonths$max, 
                   opts="HONOUR_VALID_RANGE=NO")
  }
  
  return(r_var)
}
