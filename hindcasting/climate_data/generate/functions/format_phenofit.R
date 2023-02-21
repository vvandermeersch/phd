
# Function to format GWGEN csv format to PHENOFIT format

# return yr (according to Phenofit base-year)

format_gwgen_to_phenofit <- function(gwgen_in_file, climate_file, pd_folder, yrbase = 22000, debug_first_row = T){
  
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
    climate[climate$tmin == 0, "tmin"] <- 0.01
    
    
    # Process data     
    tmin <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$tmin, climate$id))))
    tmax <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$tmax, climate$id))))
    wind <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$wind, climate$id)))) 
    pre <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$prcp, climate$id))))
    glo <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$glo, climate$id))))
    pet <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$pet, climate$id))))
    tmean <- cbind(id_loc$lat, id_loc$lon, (tmin[, -c(1,2)] + tmax[, -c(1,2)])/2) # approximation of Tmean 
    
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
  
  # false data
  alt <- tmin[,1:2]
  alt$alt <- 200
  alt_file <- create_phenofit_file(yrb, var = "Altitude", pd_folder_yr)
  whc_file <- create_phenofit_file(yrb, var = "WHC", pd_folder_yr)
  fwrite(alt, file = alt_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(alt, file = whc_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)

  message("Conversion to PHENOFIT format done !")
  
}







format_gwgen_to_phenofit_addvar <- function(gwgen_in_file, climate_file, addvar, pd_folder, yrbase = 22000, debug_first_row = T){
  
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
      data <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$toa, climate$id))))
    }else if(addvar == "cld"){
      data <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$mean_cloud, climate$id))))
    }else if(addvar == "mbar"){
      data <- cbind(id_loc$lat, id_loc$lon, as.data.frame(do.call(rbind, split(climate$mbar, climate$id))))
    }
    
    
    
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
