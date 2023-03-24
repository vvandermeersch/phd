

load_altitude <- function(year, folder, extent){
  
  file <- years_to_file(year)
  alt_file <- file.path(folder, "ht_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_24000_0kyr",
                        paste0("ht_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_", file$name, ".nc"))
  years <- file$max:file$min # time goes forward
  alt_sheet <- rast(alt_file, subds = "ht", lyrs = which(years == year))
  alt_sheet <- crop(alt_sheet, extent)
  
  # data was interpolated from lower native resolution to high resolution
  # thus, coastlands and islands may not exist in the land sea mask
  # "you should set any missing (altitude) values like this to something small" (E. Armstrong)
  # no missing values in climate data => use this raster to cover rare missing cells
  cover_file <- file.path(folder, "tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr",
                           paste0("tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc"))
  cover <- rast(cover_file, subds = "tempmin_av", lyrs = which(years == year)*12-6)
  cover<- crop(cover, extent)
  print(ncell(cover)-freq(cover, value=NA)$count[1])
  cover<- 0*cover # set altitude to the minimum (~ islands, coastlines)
  alt_sheet <- terra::cover(alt_sheet, cover)
  
  alt_sheet_df <- as.data.frame(alt_sheet, xy = TRUE)
  alt_sheet_df$id <- as.numeric(rownames(alt_sheet_df))
  names(alt_sheet_df) <- c("lon", "lat", "alt", "id")
  return(alt_sheet_df)
}

load_altitude_ICE6GC <- function(year, folder = "D:/climate/ICE-6G-C", folder_hadcm3b = "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw", extent){
  
  fileyear <- round(year/500) * 500 / 1000 # round to the nearest 5k year
  alt_file <- file.path(folder, paste0("I6_C.VM5a_10min.", fileyear, ".nc"))
  alt_10min <- rotate(rast(alt_file, subds = "Orog"))
  ldmsk_10min <- rotate(rast(alt_file, subds = "sftlf"))
  ldmsk_10min[ldmsk_10min == 0] <- NA
  alt_10min <- mask(alt_10min, ldmsk_10min)
  alt_30min <- aggregate(alt_10min, 3, na.rm = T)
  # upscale and mask with HadCM3B file
  file <- years_to_file(year)
  years <- file$max:file$min # time goes forward
  cover_file <- file.path(folder_hadcm3b, "tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr",
                          paste0("tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc"))
  cover <- rast(cover_file, subds = "tempmin_av", lyrs = which(years == year)*12-6)
  cover <- crop(cover, extent)
  alt_05deg <- resample(alt_30min, cover)
  alt_05deg <- mask(alt_05deg, cover)
  alt_05deg <- crop(alt_05deg, extent)
  
  alt_df <- as.data.frame(alt_05deg, xy = TRUE)
  alt_df$id <- as.numeric(rownames(alt_df))
  names(alt_df) <- c("lon", "lat", "alt", "id")
  return(alt_df)
}

load_albedo <- function(year, folder, extent){
  
  file <- years_to_file(year)
  albedo_file <- file.path(folder, "albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr",
                        paste0("albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc"))
  years <- file$max:file$min # time goes forward
  mn_e <- which(years == year)*12
  mn_b <- mn_e-11
  albedo <- rast(albedo_file, subds = "albedos", lyrs = mn_b:mn_e)
  albedo <- crop(albedo, extent)
  names(albedo) <- as.character(1:12)
  data <- as.data.frame(albedo, xy = TRUE, cells = T)
  data_temp <- stack(data[, 4:15])
  albedo_df <- data.frame(data[rep(seq_len(nrow(data)), 12), 1:3], data_temp)
  names(albedo_df) <- c("id", "lon", "lat", "alb", "month")
  
  # missing values or errors (>1) because of polar night and interpolation (see E. Armstrong mail)
  albedo_df[is.na(albedo_df)] <- 0.8
  albedo_df[albedo_df$alb > 0.8, "alb"] <- 0.8
  albedo_df[albedo_df$alb == 0, "alb"] <- 0.8

  return(albedo_df)
}
