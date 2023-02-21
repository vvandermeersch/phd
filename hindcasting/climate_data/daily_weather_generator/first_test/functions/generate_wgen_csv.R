generate_gwgen_csv <- function(years, extent, source_dir, output_dir){
  
  header <- c("station id", "lon", "lat", "year", "month", "min.temperature", "max.temperature",
              "cloud fraction", "wind speed", "precipitation", "wet")
  
  write.table(t(header), file = file.path(output_dir, paste0(years, "_gwgen.csv")), col.names = FALSE, row.names = FALSE, sep=",")
  
  limits <- years_to_limits(years)
  
  # load raster files
  r_tas <- raster::stack(file.path(source_dir,  paste0("bias_regrid_tas_highres_", years, ".nc")))
  r_tmin <- raster::stack(file.path(source_dir,  paste0("bias_regrid_tempmonmin_abs_highres_", years, ".nc")))
  r_pre <- raster::stack(file.path(source_dir,  paste0("bias_regrid_pr_highres_", years, ".nc")))
  r_wet <- raster::stack(file.path(source_dir,  paste0("regrid_rd3_mm_srf_highres_", years, ".nc")))
  
  r_cells <- crop(subset(r_tas, 1), extent)
  
  for(id in 1:ncell(r_cells)){
    
    data <- sapply(1:120, function(i){
      
      tmin <- crop(subset(r_tmin, i), extent)
      pre <- crop(subset(r_pre, i), extent)
      wet <- crop(subset(r_wet, i), extent)
      
      yr <- (i-1)%/%12 + limits$min + 1950
      
      mn <- ifelse(i%%12 == 0, 12,  i%%12)
      wetd <- ifelse(pre[id] == 0, 0, as.integer(round(wet[id])))
      
      data <- c(id, xFromCell(tmin, id), yFromCell(tmin, id), yr, mn, tmin[id], tmin[id]+20, 0.5, 7, pre[id]*30, wetd)

    })
    
    table <- as.data.frame(t(data))
    
    write.table(table, file = file.path(output_dir, paste0(years, "_gwgen.csv")), append = T, col.names = FALSE, row.names = FALSE, sep=",")
    
  }
  
}
