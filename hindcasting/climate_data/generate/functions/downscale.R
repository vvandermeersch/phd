# function to downscale temperatures and radiation

# inspired by a Copernicus Climate Change Service (CCCS) method
# see: https://datastore.copernicus-climate.eu/documents/sis-biodiversity/C3S_D427.3.1.1_Product_user_guide-Bioclimatic_indicators_ERA5-1km_v2.3.pdf
# use the 8 neighbor cells to apply a height correction 

# author : V. Van der Meersch - 30/03/2023
# improved v2 version, much faster !

downscale <- function(yr, alt_HR, in_folder, out_folder, foc_extrapol = FALSE){
  
  # 0. Load climate data
  cat("Loading data\n")
  pre_LR <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_pre_", yr, "_dly.fit"))))
  wnd_LR <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_wnd_", yr, "_dly.fit"))))
  cld_LR <- data.frame(fread(file.path(in_folder, paste0("HadCM3B_cld_", yr, "_dly.fit"))))
  pre_LR <- rast(lapply(3:ncol(pre_LR), function(i) rast(pre_LR[, c(2,1,i)])))
  wnd_LR <- rast(lapply(3:ncol(wnd_LR), function(i) rast(wnd_LR[, c(2,1,i)])))
  cld_LR <- rast(lapply(3:ncol(cld_LR), function(i) rast(cld_LR[, c(2,1,i)])))
  
  
  # 0. Load altitude data
  if(foc_extrapol){
    alt_LR <- aggregate(alt_HR, 3, na.rm = T)
    alt_LR <- resample(alt_LR, subset(pre_LR,1))
  }else{
    alt_LR <- fread(file.path(in_folder, "HadCM3B_Altitude.fit"))
    alt_LR <- rast(alt[, c(2,1,3)])
  }
  
  # 0. Extrapolate coastal cells
  if(foc_extrapol){
    pre_LR <- mask(focal(pre_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    wnd_LR <- mask(focal(wnd_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
    cld_LR <- mask(focal(cld_LR, w = 3, fun = "mean", na.policy ="only"), alt_LR)
  }
  
  # 1. Interpolate coarse variables to the high-resolution
  pre_LR_HR <- resample(pre_LR, alt_HR)
  wnd_LR_HR <- resample(wnd_LR, alt_HR)
  cld_LR_HR <- resample(cld_LR, alt_HR)
  
  # 2. Transform to matrix
  pre_data <- lapply(1:dim(pre_LR_HR)[3], function(i){na.omit(as.data.frame(subset(pre_LR_HR, i)))})
  wnd_data <- lapply(1:dim(wnd_LR_HR)[3], function(i){na.omit(as.data.frame(subset(wnd_LR_HR, i)))})
  cld_data <- lapply(1:dim(cld_LR_HR)[3], function(i){na.omit(as.data.frame(subset(cld_LR_HR, i)))})
  pre <- do.call(cbind, pre_data)
  wnd <- do.call(cbind, wnd_data)
  cld <- do.call(cbind, cld_data)
  
  # get lat and lon
  lonlat <- as.data.frame(subset(cld_LR_HR, 1), xy = T)
  latlon <- na.omit(lonlat)[, c(2,1)]
  
  # 3. Create files
  pre_file <- create_phenofit_file(yr, var = "pre", out_folder)
  wnd_file <- create_phenofit_file(yr, var = "wnd", out_folder)
  cld_file <- create_phenofit_file(yr, var = "cld", out_folder)
  
  # 8. Write files
  cat("Writing files\n")
  write.table(cbind(latlon, pre), file = pre_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  write.table(cbind(latlon, wnd), file = wnd_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  write.table(cbind(latlon, cld), file = cld_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  
  gc(verbose = FALSE)
  
}


