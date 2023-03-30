# function to downscale temperatures and radiation

# inspired by a Copernicus Climate Change Service (CCCS) method
# see: https://datastore.copernicus-climate.eu/documents/sis-biodiversity/C3S_D427.3.1.1_Product_user_guide-Bioclimatic_indicators_ERA5-1km_v2.3.pdf
# use the 8 neighbor cells to apply a height correction 

# author : V. Van der Meersch - 30/03/2023
# improved v2 version, much faster !

downscale_with_altitude2 <- function(yr, alt_HR, in_folder, out_folder, ncores, foc_extrapol = FALSE){
  
  # 0. Load climate data
  cat("Loading data\n")
  tmax_LR <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_tmx_", yr, "_dly.fit"))))
  tmin_LR <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_tmn_", yr, "_dly.fit"))))
  tmean_LR <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_tmp_", yr, "_dly.fit"))))
  glo_LR <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_glo_", yr, "_dly.fit"))))
  tmax_LR <- rast(lapply(3:ncol(tmax_LR), function(i) rast(tmax_LR[, c(2,1,i)])))
  tmin_LR <- rast(lapply(3:ncol(tmin_LR), function(i) rast(tmin_LR[, c(2,1,i)])))
  tmean_LR <- rast(lapply(3:ncol(tmean_LR), function(i) rast(tmean_LR[, c(2,1,i)])))
  glo_LR <- rast(lapply(3:ncol(glo_LR), function(i) rast(glo_LR[, c(2,1,i)])))
  
  # 0. Load altitude data
  if(foc_extrapol){
    alt_LR <- aggregate(alt_HR, 3, na.rm = T)
    alt_LR <- resample(alt_LR, subset(tmax_LR,1))
  }else{
    alt_LR <- fread(file.path(in_folder, "HadCM3B_Altitude.fit"))
    alt_LR <- rast(alt[, c(2,1,3)])
  }
  
  # 0. Create rasters to save regression coefficients
  tmax_reg_LR <- rast()
  tmin_reg_LR <- rast()
  tmean_reg_LR <- rast()
  glo_reg_LR <- rast()
  
  # 0. Extrapolate coastal cells
  if(foc_extrapol){
    tmax_LR <- mask(focal(tmax_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    tmin_LR <- mask(focal(tmin_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    tmean_LR <- mask(focal(tmean_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    glo_LR <- mask(focal(glo_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
  }
  
  # 0. Wrap rasters to pass them over a connection that serializes
  .alt_LR <- wrap(alt_LR)
  .tmax_LR <- wrap(tmax_LR)
  .tmin_LR <- wrap(tmin_LR)
  .tmean_LR <- wrap(tmean_LR)
  .glo_LR <- wrap(glo_LR)
  
  # 1. Quantify the change with height at the native coarse resolution
  plan(multisession, workers = ncores)
  
  cat("Computing max. temperature regressions\n")
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
  
  cat("Computing min. temperature regressions\n")
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
  
  cat("Computing mean temperature regressions\n")
  start_time <- Sys.time()
  tmean_reg_LR <- future_lapply(1:dim(tmean_LR)[3], function(d){
    alt_LRw <- rast(.alt_LR) # load packed altitude
    tmean_LRw <- rast(.tmean_LR) # load packed climate
    rd <- subset(tmean_LRw, d) # layer for day d
    regd <- rd # raster to keep regression coef.
    regd[] <- NA 
    regcoef <- sapply(cells(rd), .compute_reg_coef, r = rd, ralt = alt_LRw)
    regd[cells(rd)] <- as.numeric(regcoef)
    .regd <- wrap(regd) # pack raster
    return(.regd)
  }, future.seed=TRUE)
  tmean_reg_LR <- rast(lapply(tmean_reg_LR, rast))
  end_time <- Sys.time()
  cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
  
  cat("Computing global radiation regressions\n")
  start_time <- Sys.time()
  glo_reg_LR <- future_lapply(1:dim(glo_LR)[3], function(d){
    alt_LRw <- rast(.alt_LR) # load packed altitude
    glo_LRw <- rast(.glo_LR) # load packed climate
    rd <- subset(glo_LRw, d) # layer for day d
    regd <- rd # raster to keep regression coef.
    regd[] <- NA 
    regcoef <- sapply(cells(rd), .compute_reg_coef, r = rd, ralt = alt_LRw)
    regd[cells(rd)] <- as.numeric(regcoef)
    .regd <- wrap(regd) # pack raster
    return(.regd)
  }, future.seed=TRUE)
  glo_reg_LR <- rast(lapply(glo_reg_LR, rast))
  end_time <- Sys.time()
  cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
  
  plan(sequential)
  gc(verbose = FALSE)
  
  # 2. Interpolate the slope to the high-resolution grid
  tmax_reg_LR_HR <- resample(tmax_reg_LR, alt_HR)
  tmin_reg_LR_HR <- resample(tmin_reg_LR, alt_HR)
  tmean_reg_LR_HR <- resample(tmean_reg_LR, alt_HR)
  glo_reg_LR_HR <- resample(glo_reg_LR, alt_HR)
  
  # 3. Interpolate coarse variables to the high-resolution
  tmax_LR_HR <- resample(tmax_LR, alt_HR)
  tmin_LR_HR <- resample(tmin_LR, alt_HR)
  tmean_LR_HR <- resample(tmean_LR, alt_HR)
  glo_LR_HR <- resample(tmax_LR, alt_HR)
  
  # 4. Interpolate coarse elevation field at high resolution
  alt_LR_HR <- resample(alt_LR, alt_HR)
  
  # 5. Apply height correction 
  cat("Applying height correction\n")
  tmax_HR <- tmax_LR_HR + tmax_reg_LR_HR * (alt_HR - alt_LR_HR)
  tmin_HR <- tmin_LR_HR + tmin_reg_LR_HR * (alt_HR - alt_LR_HR)
  tmean_HR <- tmean_LR_HR + tmean_reg_LR_HR * (alt_HR - alt_LR_HR)
  glo_HR <- glo_LR_HR + glo_reg_LR_HR * (alt_HR - alt_LR_HR)
  
  # 6. Transform to matrix
  tmax_data <- lapply(1:dim(tmax_HR)[3], function(i){na.omit(as.data.frame(subset(tmax_HR, i)))})
  tmin_data <- lapply(1:dim(tmin_HR)[3], function(i){na.omit(as.data.frame(subset(tmin_HR, i)))})
  tmean_data <- lapply(1:dim(tmean_HR)[3], function(i){na.omit(as.data.frame(subset(tmean_HR, i)))})
  glo_data <- lapply(1:dim(glo_HR)[3], function(i){na.omit(as.data.frame(subset(glo_HR, i)))})
  tmax <- do.call(cbind, tmax_data)
  tmin <- do.call(cbind, tmin_data)
  tmean <- do.call(cbind, tmean_data)
  glo <- do.call(cbind, glo_data)
  # get lat and lon
  lonlat <- as.data.frame(subset(tmax_HR, 1), xy = T)
  latlon <- na.omit(lonlat)[, c(2,1)]
  
  # 7. Create files
  tmin_file <- create_phenofit_file(yr, var = "tmn", out_folder)
  tmax_file <- create_phenofit_file(yr, var = "tmx", out_folder)
  tmean_file <- create_phenofit_file(yr, var = "tmp", out_folder)
  glo_file <- create_phenofit_file(yr, var = "glo", out_folder)
  
  # 8. Write files
  cat("Writing files\n")
  write.table(cbind(latlon, tmin), file = tmin_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  write.table(cbind(latlon, tmax), file = tmax_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  write.table(cbind(latlon, tmean), file = tmean_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  write.table(cbind(latlon, glo), file = glo_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  
  gc(verbose = FALSE)
  
}

.compute_reg_coef <- function(i, r, ralt){
  adj_cells <- as.numeric(terra::adjacent(r, i, 8, include = T))
  lin_reg <- lm(unlist(r[adj_cells]) ~ unlist(ralt[adj_cells]))
  return(as.numeric(lin_reg$coefficients[2]))
}
