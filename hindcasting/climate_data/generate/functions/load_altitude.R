

load_altitude <- function(year, folder, extent){
  
  file <- years_to_file(year)
  alt_file <- file.path(folder, "ht_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_24000_0kyr",
                        paste0("ht_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_", file$name, ".nc"))
  years <- file$max:file$min # time goes forward
  print(which(years == year))
  alt_sheet <- rast(alt_file, subds = "ht", lyrs = which(years == year))
  alt_sheet <- crop(alt_sheet, extent)
  
  # data was interpolated from lower native resolution to high resolution
  # thus, coastlands and islands may not exist in the land sea mask
  # "you should set any missing (altitude) values like this to something small" (E. Armstrong)
  # no missing values in albedo => use this raster to cover rare missing cells
  albedo_file <- file.path(folder, "albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr",
                           paste0("albedos_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc"))
  albedo <- rast(albedo_file, subds = "albedos", lyrs = which(years == year)*12-6)
  albedo <- crop(albedo, extent)
  print(ncell(albedo)-freq(albedo, value=NA)$count[1])
  albedo <- 0*albedo # set altitude to the minimum (~ islands, coastlines)
  alt_sheet <- terra::cover(alt_sheet, albedo)
  
  alt_sheet_df <- as.data.frame(alt_sheet, xy = TRUE)
  alt_sheet_df$id <- as.numeric(rownames(alt_sheet_df))
  names(alt_sheet_df) <- c("lon", "lat", "alt", "id")
  return(alt_sheet_df)
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
