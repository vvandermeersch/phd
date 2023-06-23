

format_phenofit <- function(yr, tmin, tmax, pre, pet, glo, wind, tdew, alt, whc, out_folder){
  
  cat("Formating data for PHENOFIT\n")
  
  tmax <- round(tmax, 2)
  tmin <- round(tmin, 2)
  tmean <- (tmin+tmax)/2
  tmean <- round(tmean, 2)
  pre <- round(pre, 2)
  wind <- round(wind, 2)
  tdew <- round(tdew, 2)
  glo <- round(glo, 2)
  
  tmax <- ifel(tmin == 0 & tmax == 0 & tmean == 0 & pre == 0, 0.01, tmax) # necessary check for FitlibClimateLoader in Capsis
  
  tmin_df <- as.data.frame(tmin, xy = T)
  tmin_df <- round(tmin_df[,c(2,1,3:ncol(tmin_df))],2) #lat/lon order + change decimal precision
  tmax_df <- as.data.frame(tmax, xy = T)
  tmax_df <- round(tmax_df[,c(2,1,3:ncol(tmax_df))],2)
  tmean_df <- as.data.frame(tmean, xy = T)
  tmean_df <- round(tmean_df[,c(2,1,3:ncol(tmean_df))],2)
  pre_df <- as.data.frame(pre, xy = T)
  pre_df <- round(pre_df[,c(2,1,3:ncol(pre_df))],2)
  pet_df <- as.data.frame(pet, xy = T)
  pet_df <- round(pet_df[,c(2,1,3:ncol(pet_df))],2)
  pet_df[is.na(pet_df)] <- 0
  glo_df <- as.data.frame(glo, xy = T)
  glo_df <- round(glo_df[,c(2,1,3:ncol(glo_df))],2)
  glo_df[is.na(glo_df)] <- 0
  wind_df <- as.data.frame(wind, xy = T)
  wind_df <- round(wind_df[,c(2,1,3:ncol(wind_df))],2)
  tdew_df <- as.data.frame(tdew, xy = T)
  tdew_df <- round(tdew_df[,c(2,1,3:ncol(tdew_df))],2)
  alt_df <- as.data.frame(alt, xy = T)
  alt_df <- round(alt_df[,c(2,1,3:ncol(alt_df))],2)
  whc_df <- as.data.frame(whc, xy = T)
  whc_df <- round(whc_df[,c(2,1,3:ncol(whc_df))],2)
  
  # Create files
  tmin_file <- .create_phenofit_file(-yr, var = "tmn", out_folder)
  tmax_file <- .create_phenofit_file(-yr, var = "tmx", out_folder)
  tmean_file <- .create_phenofit_file(-yr, var = "tmp", out_folder)
  pre_file <- .create_phenofit_file(-yr, var = "pre", out_folder)
  pet_file <- .create_phenofit_file(-yr, var = "pet", out_folder)
  glo_file <- .create_phenofit_file(-yr, var = "glo", out_folder)
  wind_file <- .create_phenofit_file(-yr, var = "wnd", out_folder)
  tdew_file <- .create_phenofit_file(-yr, var = "dtm", out_folder)
  alt_file <- .create_phenofit_file(-yr, var = "Altitude", out_folder)
  whc_file <- .create_phenofit_file(-yr, var = "WHC", out_folder)
  
  # Write files
  fwrite(tmin_df, file = tmin_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(tmax_df, file = tmax_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(tmean_df, file = tmean_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(pre_df, file = pre_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(pet_df, file = pet_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(glo_df, file = glo_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(wind_df, file = wind_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(tdew_df, file = tdew_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(alt_df, file = alt_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  fwrite(whc_df, file = whc_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
  
  # Free memory
  gc(verbose = FALSE)
  
}









.create_phenofit_file <- function(yr, var, pd_folder){
  
  processed_file <- file.path(pd_folder, paste0("HadCM3B_", var, "_", yr, "_dly.fit"))
  if(var == "Altitude" | var == "WHC"){
    processed_file <- file.path(pd_folder, paste0("HadCM3B_", var, ".fit"))
  }
  
  con <- file(processed_file, open="wt")
  writeLines("Climate datafile for Phenofit model", con)
  writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
  comments <- .get_comments(var = var)
  writeLines(comments, con)
  writeLines(" ", con)
  close(con)
  
  return(processed_file)
  
}


# Function to get Phenofit input variable definitions

.get_comments <- function(var){
  if(var=='glo'){
    return("Variable : daily global radiation (MJ/m²)")
  }
  else if(var=='pre'){
    return("Variable : daily precipitation, comprising rain and snow (mm)")
  }
  else if(var=='prs'){
    return("Variable : daily mean surface pressure (kPa)")
  }
  else if(var=='RH'){
    return("Variable : daily mean 2m relative humidity (%) calculated with vapor pressure ratio (Clausius-Clapeyron relation)")
  }
  else if(var=='tmn'){
    return("Variable : daily mimimal 2m temperature (°C)")
  }
  else if(var=='dtm'){
    return("Variable : daily mean 2m dewpoint temperature (°C)")
  }
  else if(var=='tmp'){
    return("Variable : daily mean 2m temperature (°C)")
  }
  else if(var=='tmx'){
    return("Variable : daily maximal 2m temperature (°C)")
  }
  else if(var=='wnd'){
    return("Variable : daily mean 10m wind speed (m/s) - used only to compute evapotranspiration")
  }
  else if(var=='RHmin'){
    return("Variable : daily miminal 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='RHmax'){
    return("Variable : daily maximal 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='Altitude'){
    return("Variable : altitude (m) calculated from geopotential height")
  }
  else if(var=='pet'){
    return("Variable : potential evapotranspiration (mm) calculated with Penman-Monteith formulation (FAO-56 hypothetical short grass method)")
  }
  else if(var=='WHC'){
    return("Variable : water holding capacity (mm)")
  }
  else if(var=='TOA'){
    return("TOA radiation (kJ/m²)")
  }
  else if(var=='cld'){
    return("Cloudiness factor")
  }
  else if(var=="mbar"){
    return("Daytime mean optical air mass")
  }
  else if(var=="wind"){
    return("Wind speed")
  }
  
  
  
}
