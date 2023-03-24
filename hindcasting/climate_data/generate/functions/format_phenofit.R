
# Function to format GWGEN csv format to PHENOFIT format

# return yr (according to Phenofit base-year)

format_gwgen_to_phenofit <- function(gwgen_in_file, climate_file, 
                                     pd_folder,
                                     debug_first_row = T, 
                                     alt, WHC_present){
  
  # Load files
  id_loc <- unique(fread(gwgen_in_file, select = c("station id", "lon", "lat")))
  # gwgen <- fread(gwgen_out_file)
  climate_data <- fread(climate_file)
  
  # problem with GWGEN - first row of each generator run is strange
  if(debug_first_row){
    stations <- unique(id_loc$`station id`)
    ncores <- 20
    nstat_per_core <- ceiling(length(stations)/1000) * 1000%/%ncores
    split_gwgen <- split(stations, ceiling(seq_along(stations)/nstat_per_core))
    first_stations <- sapply(1:length(split_gwgen), function(i) split_gwgen[[i]][1])
  }
  
  
  
  years <- unique(climate_data$year)
  year_center <- max(years) - 15
  
  pd_folder_yr <- file.path(pd_folder, paste0(year_center, "BP"))
  dir.create(pd_folder_yr, showWarnings = FALSE)
  
  for(yr in years){
    # cat("Doing year", yr, "\n")
    climate <- climate_data[climate_data$year == yr,]
    
    # debug GWGEN first row, replace 1st January with data of 2nd January (temporary)
    if(debug_first_row){
      for(s in first_stations){
        climate[climate$id == s & climate$month == 1 & climate$day == 1, ] <- climate[climate$id == s & climate$month == 1 & climate$day == 2, ]
      }
    }
    
    # (temporary) // Phenofit getClimateDay
    climate[climate$tmin == 0, "tmin"] <- 0.01
    
    
    # Process data     
    tmin <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$tmin, climate$id))))
    
    # WHC data
    WHC_present_r <- rast(WHC_present[,c("lon", "lat", "whc")])
    res_r <- rast(tmin[,c(2,1,3)])
    # resample WHC
    WHC_present_r <- aggregate(WHC_present_r, 5, na.rm = T)
    WHC_present_r <- terra::resample(WHC_present_r, res_r, method = "average")
    WHC_present_r <- mask(WHC_present_r, res_r)
    whc <- as.data.frame(WHC_present_r, xy = T)
    names(whc)[1:2] <- c("lon", "lat")
    
    tmax <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$tmax, climate$id))))
    wind <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$wind, climate$id)))) 
    pre <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$prcp, climate$id))))
    glo <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$glo, climate$id))))
    pet <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$petPM, climate$id))))
    tmean <- cbind(lat = id_loc$lat, lon = id_loc$lon, (tmin[, -c(1,2)] + tmax[, -c(1,2)])/2) # approximation of Tmean 
    
    
    # match with WHC data (available only in Europe)
    tmin <- inner_join(tmin, whc[,c("lon", "lat")], by = join_by(lat, lon))
    tmax <- inner_join(tmax, whc[,c("lon", "lat")], by = join_by(lat, lon))
    wind <- inner_join(wind, whc[,c("lon", "lat")], by = join_by(lat, lon))
    pre <- inner_join(pre, whc[,c("lon", "lat")], by = join_by(lat, lon))
    glo <- inner_join(glo, whc[,c("lon", "lat")], by = join_by(lat, lon))
    pet <- inner_join(pet, whc[,c("lon", "lat")], by = join_by(lat, lon))
    tmean <- inner_join(tmean, whc[,c("lon", "lat")], by = join_by(lat, lon))
    
    
    
    # Create files
    #yrb <- 1950 - yr # if we want true date
    yrb <- - yr
    tmin_file <- create_phenofit_file(yrb, var = "tmn", pd_folder_yr)
    tmax_file <- create_phenofit_file(yrb, var = "tmx", pd_folder_yr)
    tmean_file <- create_phenofit_file(yrb, var = "tmp", pd_folder_yr)
    wind_file <- create_phenofit_file(yrb, var = "wnd", pd_folder_yr)
    pre_file <- create_phenofit_file(yrb, var = "pre", pd_folder_yr)
    glo_file <- create_phenofit_file(yrb, var = "glo", pd_folder_yr)
    pet_file <- create_phenofit_file(yrb, var = "pet", pd_folder_yr)
    
    # Write files
    fwrite(tmin, file = tmin_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(tmax, file = tmax_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(tmean, file = tmean_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(wind, file = wind_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(pre, file = pre_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(glo, file = glo_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(pet, file = pet_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    
    # Free memory
    gc(verbose = FALSE)
    
  }
  
  # Altitude data
  alt <- alt[, c("lat", "lon", "alt")]
  alt$alt <- round(alt$alt, 1)
  alt <- inner_join(alt, whc[,c("lon", "lat")]) # match with WHC data
  alt_file <- create_phenofit_file(yrb, var = "Altitude", pd_folder_yr)
  fwrite(alt, file = alt_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  
  # WHC data
  whc_file <- create_phenofit_file(yrb, var = "WHC", pd_folder_yr)
  whc$whc <- round(whc$whc, 0)
  fwrite(whc[,c("lat", "lon", "whc")], file = whc_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)

  message("Conversion to PHENOFIT format done !")
  
}







format_gwgen_to_phenofit_addvar <- function(gwgen_in_file, climate_file, 
                                            addvar, pd_folder,
                                            debug_first_row = T, 
                                            WHC_present){
  
  # Load files
  id_loc <- unique(fread(gwgen_in_file, select = c("station id", "lon", "lat")))
  # gwgen <- fread(gwgen_out_file)
  climate_data <- fread(climate_file)
  
  # problem with GWGEN - first row of each generator run is strange
  if(debug_first_row){
    stations <- unique(id_loc$`station id`)
    ncores <- 20
    nstat_per_core <- ceiling(length(stations)/1000) * 1000%/%ncores
    split_gwgen <- split(stations, ceiling(seq_along(stations)/nstat_per_core))
    first_stations <- sapply(1:length(split_gwgen), function(i) split_gwgen[[i]][1])
  }
  
  
  
  years <- unique(climate_data$year)
  year_center <- max(years) - 15
  
  pd_folder_yr <- file.path(pd_folder, paste0(year_center, "BP"))
  dir.create(pd_folder_yr, showWarnings = FALSE)
  
  for(yr in years){
    cat("Doing year", yr, "\n")
    climate <- climate_data[climate_data$year == yr,]
    
    # debug GWGEN first row, replace 1st January with data of 2nd January (temporary)
    if(debug_first_row){
      for(s in first_stations){
        climate[climate$id == s & climate$month == 1 & climate$day == 1, ] <- climate[climate$id == s & climate$month == 1 & climate$day == 2, ]
      }
    }
    
    # (temporary) // Phenofit getClimateDay
    # climate[climate$tmin == 0, "tmin"] <- 0.01
    
    
    # Process data 
    if(addvar == "TOA"){
      data <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$toa, climate$id))))
    }else if(addvar == "cld"){
      data <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$mean_cloud, climate$id))))
    }else if(addvar == "mbar"){
      data <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$mbar, climate$id))))
    }else if(addvar == "wind"){
      data <- cbind(lat = id_loc$lat, lon = id_loc$lon, as.data.frame(do.call(rbind, split(climate$wind, climate$id))))
    }
    
    # WHC data
    WHC_present_r <- rast(WHC_present[,c("lon", "lat", "whc")])
    res_r <- rast(data[,c(2,1,3)])
    # resample WHC
    WHC_present_r <- aggregate(WHC_present_r, 5, na.rm = T)
    WHC_present_r <- terra::resample(WHC_present_r, res_r, method = "average")
    WHC_present_r <- mask(WHC_present_r, res_r)
    whc <- as.data.frame(WHC_present_r, xy = T)
    names(whc)[1:2] <- c("lon", "lat")
    
    # match with WHC data (available only in Europe)
    data <- inner_join(data, whc[,c("lon", "lat")], by = join_by(lat, lon))
    
    
    # Create files
    #yrb <- 1950 - yr # if we want true date
    yrb <- - yr
    file <- create_phenofit_file(yrb, var = addvar, pd_folder_yr)
    
    # Write files
    fwrite(data, file = file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    
    # Free memory
    gc(verbose = FALSE)
    
  }

  
  message("Conversion to PHENOFIT format done !")
  
}
