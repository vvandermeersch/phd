# OLD FUNCTION - VEEEEEEEERY SLOW - NOT USED ANYMORE

# function to write CSV in the format required by GWGEN
# return output file name

# author : V. Van der Meersch - 10/10/2022

# debug option (strange wet days without precipitation ?)

write_gwgen_csv <- function(years, extent, source_dir, output_dir, debug_wet = F, debug_wind = F, ncores = 1){
  
  # get file spec
  file_spec <- years_to_file(years)
  outname <-   ifelse(!is.na(years [2]) ,paste0(years[1], "_",years [2],"BP"), paste0(years[1], "_BP"))
  
  # write header as require by gwgen
  header <- c("station id", "lon", "lat", "year", "month", "min.temperature", "max.temperature",
              "cloud fraction", "wind speed", "precipitation", "wet")
  write.table(t(header), file = file.path(output_dir, paste0(outname, "_gwgen.csv")), col.names = FALSE, row.names = FALSE, sep=",")
  
  # get months (ie subset of layers, or "bands")
  rmonths <- year_to_months(years[1], years[2], file_spec$max)
  
  # load raster files
  cat("Loading raster files \n")
  r_tmin <- rast(file.path(source_dir, "tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                                    paste0("tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file_spec$name, ".nc")),
                 subds = "tempmin_av", lyrs = rmonths$min:rmonths$max)
  r_tmax <- rast(file.path(source_dir, "tempmax_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                           paste0("tempmax_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file_spec$name, ".nc")),
                 subds = "tempmax_av", lyrs = rmonths$min:rmonths$max)
  r_pre <- rast(file.path(source_dir, "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                          paste0("precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
                subds = "precip_mm_srf", lyrs = rmonths$min:rmonths$max)
  r_wet <- rast(file.path(source_dir, "rd3_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                          paste0("rd3_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file_spec$name, ".nc")),
                subds = "rd3_mm_srf", lyrs = rmonths$min:rmonths$max)
  r_cloud <- rast(file.path(source_dir, "totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr", 
                            paste0("totCloud_mm_ua_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
                  subds = "totCloud_mm_ua", lyrs = rmonths$min:rmonths$max)
  r_uwind <- rast(file.path(source_dir, "u_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                            paste0("u_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file_spec$name, ".nc")),
                  subds = "u_mm_10m", lyrs = rmonths$min:rmonths$max)
  r_vwind <- rast(file.path(source_dir, "v_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                            paste0("v_mm_10m_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file_spec$name, ".nc")),
                  subds = "v_mm_10m", lyrs = rmonths$min:rmonths$max)

  # calculate wind
  # r_u2 <- r_uwind * r_uwind
  # r_wind <- sqrt(r_uwind^2 + r_vwind^2)
  
  # crop
  cat("Cropping raster files \n")
  r_tmin <- crop(r_tmin, extent)
  r_tmax <- crop(r_tmax, extent)
  r_pre <- crop(r_pre, extent)
  r_wet <- crop(r_wet, extent)
  r_cloud <- crop(r_cloud, extent)
  r_uwind <- crop(r_uwind, extent)
  r_vwind <- crop(r_uwind, extent)
  
  # mn beginning from 1
  rmn_min <- rmonths$min-(rmonths$min-1)
  rmn_max <- rmonths$max-(rmonths$min-1)
  
  # wrapping objects to be passed over a connection that serializes
  .r_tmin <- wrap(r_tmin)
  .r_tmax <- wrap(r_tmax)
  .r_pre <- wrap(r_pre)
  .r_wet <- wrap(r_wet)
  .r_cloud <- wrap(r_cloud)
  .r_uwind <- wrap(r_uwind)
  .r_vwind <- wrap(r_vwind)
  
  ncells_with_data <- freq(r_tmin, value=NA)$count[1]
  cat(paste0("Number of cells: ", ncell(r_tmin), "\n"))
  cat(paste0("Number of NA cells: ", freq(r_tmin, value=NA)$count[1], "\n"))
  
  p <- progressor(ncells_with_data)
  
  for(id in 1:ncell(r_tmin)){
    
    if(!any(is.na(subset(r_tmin, rmn_min:rmn_max)[id]))){
      
      p(paste0("Cell ", id, "\n"), amount = 0)
      
      # in case of parallel computing
      plan(multisession, workers = ncores)
      
      data <- future_sapply(rmn_min:rmn_max, function(i){
        
        #unwrap!
        r_tmin <- rast(.r_tmin)
        r_tmax <- rast(.r_tmax)
        r_pre <- rast(.r_pre)
        r_wet <- rast(.r_wet)
        r_cloud <- rast(.r_cloud)
        r_uwind <- rast(.r_uwind)
        r_vwind <- rast(.r_vwind)
        
        tmin <- subset(r_tmin, i)
        tmax <- subset(r_tmax, i)
        pre <- subset(r_pre, i)
        wet <- subset(r_wet, i)
        cloud <- subset(r_cloud, i)
        uwind  <- subset(r_uwind, i)
        vwind <- subset(r_vwind, i)
        
        yr <- 1950 - (i-1)%/%12 - file_spec$min 
        
        mn <- ifelse(i%%12 == 0, 12,  i%%12)
        
        wetd <- as.integer(round(wet[id]))
        pre <- pre[id]*30
        if(debug_wet){
          if(pre > 0.1 & pre < 2){
            wetd <- 1
          }else if(pre < 0.1){
            wetd <- 0
            pre <- 0
          }else if(pre > 2 & wetd == 0){
            wetd <- 1
          }
         
        }
        
        
        data <- c(id, xFromCell(tmin, id), yFromCell(tmin, id), yr, mn, 
                  tmin[id], tmax[id], cloud[id], sqrt(uwind[id]^2 + vwind[id]^2), pre, wetd)
        data  <- unlist(data)
        
      })
      
      plan(sequential)
      gc()
      
      table <- as.data.frame(t(data))
      
      if(debug_wind){
        table <- na.locf(table, na.rm=FALSE) # carry the last observation forward to replace rare missing wind speed values
      }
      
      write.table(table, file = file.path(output_dir, paste0(outname, "_gwgen.csv")), append = T, col.names = FALSE, row.names = FALSE, sep=",")
      
      p()

    }
    
  }
  
  
  
  message("Input csv file created !")
  
  return(file.path(output_dir, paste0(outname, "_gwgen.csv")))
  
}
