
format_phenofit <- function(years, extent, model, scenario, folder){
  
  cat("Formating data for PHENOFIT\n")
  
  files_to_read <- .find_filenames(from = years[1], to = years[2])
  
  pre <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, scenario, model, "raw"), "/", "prAdjust_day_", model, "_", scenario, "_r1i1p1f1_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  
  tmp <- rast(lapply(files_to_read, function (f)
    rast(paste0(file.path(folder, scenario, model, "raw"), "/", "tasAdjust_day_", model, "_", scenario, "_r1i1p1f1_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  
  
  out_folder <- file.path(folder, scenario, model, "phenofit_format")
  dir.create(out_folder, showWarnings = FALSE)
  
  for(yr in years[1]:years[2]){
    
    cat(paste0("   Year ", yr, "\n"))
    
    precrop_extent <- ext(c(0,360,33,72))
    
    cat("    - processing rasters...\n")
    
    indices <- which(time(pre, format ="years") == yr)
    pre_yr <- subset(pre, indices)
    tmp_yr <- subset(tmp, indices)
    
    # pre-cropping (save computation time)
    pre_yr <- crop(pre_yr, precrop_extent)
    tmp_yr <- crop(tmp_yr, precrop_extent)
    
    # rotate data along longitude
    pre_yr <- rotate(pre_yr)
    tmp_yr <- rotate(tmp_yr)
    
    # crop around Europe
    pre_yr <- crop(pre_yr, extent)
    tmp_yr <- crop(tmp_yr, extent)
    
    # convert units, change decimal precision
    pre_yr <- pre_yr*86400 # in mm
    tmp_yr <- tmp_yr-273.15 # in celsius
    pre_yr <- round(pre_yr,2)
    tmp_yr <- round(tmp_yr,2)
    
    cat("    - writings files...\n")
    
    # convert to dataframe
    pre_df <- as.data.frame(pre_yr, xy = T)
    pre_df <- round(pre_df[,c(2,1,3:ncol(pre_df))],2)
    tmp_df <- as.data.frame(tmp_yr, xy = T)
    tmp_df <- round(tmp_df[,c(2,1,3:ncol(tmp_df))],2)
    
    # create files
    pre_file <- .create_phenofit_file(yr, var = "pre", model, out_folder)
    tmp_file <- .create_phenofit_file(yr, var = "tmp", model, out_folder)
    
    # write data
    fwrite(pre_df, file = pre_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    fwrite(tmp_df, file = tmp_file, append = T, sep= "\t", row.names = FALSE, col.names = FALSE)
    
  }
  
}



.create_phenofit_file <- function(yr, var, model,  pd_folder){
  
  processed_file <- file.path(pd_folder, paste0(model, "_", var, "_", yr, "_dly.fit"))
  if(var == "Altitude" | var == "WHC"){
    processed_file <- file.path(pd_folder, paste0(model, "_", var, ".fit"))
  }
  
  con <- file(processed_file, open="wt")
  writeLines("Climate datafile for Phenofit model", con)
  writeLines(paste0("Created on RStudio, by user ", Sys.getenv("USERNAME") ,", on the ", Sys.Date()), con)
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


.find_filenames <- function(from, to){
  
  if(to > 2101){stop("Error")}
  
  start_years <- c(seq(1966,2086,10),2101)
  years <- c()
  
  for(i in from:to){
    if(i < 1966 & !(1951 %in% years)){years <- c(1951)}
    else{
      for(j in 1:length(start_years)){
        if(i > start_years[j] & i < start_years[j+1] & !(start_years[j] %in% years)){years <- c(years, start_years[j])}
      }
    }
  }
  
  files <- sapply(years, function(i) ifelse(i == 1951 | i == 2086, paste0(i,"0101-", i+14, "1231"), paste0(i,"0101-", i+9, "1231")))
  
  return(files)
}
