
# load ice sheet for a specific year

load_icesheet <- function(year, folder, sea_ice = FALSE, extent){
  file <- years_to_file(year)
  ice_file <- file.path(folder, "field1391_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_24000_0kyr",
                        paste0("field1391_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_", file$name, ".nc"))
  years <- file$max:file$min # time go forward in the files
  ice_sheet <- rast(ice_file, subds = "field1391", lyrs = which(years == year))
  ice_sheet <- crop(ice_sheet, extent)
  
  if(sea_ice){
    sea_ice <- file.path(folder, "iceconc_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr",
                         paste0("iceconc_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", file$name, ".nc"))
    sea_ice <- rast(sea_ice, subds = "iceconc_mm_srf", lyrs = which(years == year))
    sea_ice <- crop(sea_ice, extent)
    sea_ice[is.na(sea_ice)] <- 0
    ice_sheet[is.na(ice_sheet)] <- 0
    ice_sheet <- ice_sheet + sea_ice
  }

  ice_sheet_df <- as.data.frame(ice_sheet, xy = TRUE)
  names(ice_sheet_df) <- c("lon", "lat", "ice")
  return(ice_sheet_df)
}

load_altitude <- function(year, folder, extent, dscale = F) {
  
  file <- years_to_file(year)
  alt_file <- file.path(folder, "ht_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_24000_0kyr",
                        paste0("ht_old_sims_1yrAvg_ann_0.5degRes_noBias_Europe_", file$name, ".nc"))
  years <- file$min:file$max
  alt_sheet <- rast(alt_file, subds = "ht", lyrs = which(years == year))
  alt_sheet <- crop(alt_sheet, extent)
  
  if(dscale){
    temp <- alt_sheet
    res(temp) <- c(0.1, 0.1)
    alt_sheet <- resample(alt_sheet, temp, method = "bilinear")
  }
  
  alt_sheet_df <- as.data.frame(alt_sheet, xy = TRUE)
  alt_sheet_df$id <- as.numeric(rownames(alt_sheet_df))
  names(alt_sheet_df) <- c("lon", "lat", "alt", "id")
  return(alt_sheet_df)
}



# extract file specification
years_to_file <- function(years){
  if(max(years) < 2001){
    return(list(name = "2000_0kyr", min = 0, max = 2000))
  }
  else if(max(years) < 4001){
    return(list(name = "4000_2001kyr", min = 2001, max = 4000))
  }
  else if(max(years) < 6001){
    return(list(name = "6000_4001kyr", min = 4001, max = 6000))
  }
  else if(max(years) < 8001){
    return(list(name = "8000_6001kyr", min = 6001, max = 8000))
  }
  else if(max(years) < 10001){
    return(list(name = "10000_8001kyr", min = 8001, max = 10000))
  }
  else if(max(years) < 12001){
    return(list(name = "12000_10001kyr", min = 10001, max = 12000))
  }
  else if(max(years) < 14001){
    return(list(name = "14000_12001kyr", min = 12001, max = 14000))
  }
  else if(max(years) < 16001){
    return(list(name = "16000_14001kyr", min = 14001, max = 16000))
  }
  else if(max(years) < 18001){
    return(list(name = "18000_16001kyr", min = 16001, max = 18000))
  }
  else if(max(years) < 20001){
    return(list(name = "20000_18001kyr", min = 18001, max = 20000))
  }
  else if(max(years) < 22001){
    return(list(name = "22000_20001kyr", min = 20001, max = 22000))
  }
  else if(max(years) < 24001){
    return(list(name = "24000_22001kyr", min = 22001, max = 24000))
  }
}
