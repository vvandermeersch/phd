

compute_future_variables <- function(from, to, model, scenario,
                                     extent, data_dir){
  
  files_to_read <- .find_filenames(from, to)
  
  pre <- rast(lapply(files_to_read, function (f)
    rast(paste0(data_dir, "/", "prAdjust_day_", model, "_", scenario, "_r1i1p1f1_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  indices <- which(time(pre, format ="years") %in% (from:to))
  pre <- subset(pre, indices)
  
  tmp <- rast(lapply(files_to_read, function (f)
    rast(paste0(data_dir, "/", "tasAdjust_day_", model, "_", scenario, "_r1i1p1f1_gr010_TCDF-CDFT23-ERA5Land-1981-2010_", f, ".nc"))))
  indices <- which(time(tmp, format ="years") %in% (from:to))
  tmp <- subset(tmp, indices)
  
  # average three-month means
  indices <- factor(ifelse(time(pre, format ="months") %in% c(12,1,2), "tmp_DJF",
                           ifelse(time(pre, format ="months") %in% c(3,4,5), "tmp_MAM",
                                  ifelse(time(pre, format ="months") %in% c(6,7,8), "tmp_JJA",
                                         "tmp_SON"))))
  tmp_3months <- tapp(pre, index = indices, fun = mean, 
                                  filename = "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/temp/tmp.tif",
                                  overwrite = TRUE)
  tmp_3months <- tmp_3months-273.15 # convert to Celsius
  tmp_3months <- rotate(tmp_3months) # change longitude
  tmp_3months <- crop(tmp_3months, extent)
  
  # average three-month sums
  indices <- factor(ifelse(time(pre, format ="months") %in% c(12,1,2), "pre_DJF",
                           ifelse(time(pre, format ="months") %in% c(3,4,5), "pre_MAM",
                                  ifelse(time(pre, format ="months") %in% c(6,7,8), "pre_JJA",
                                         "pre_SON"))))
  pre_3months <- tapp(pre, index = indices, fun = sum, 
                      filename = "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/temp/pre.tif",
                      overwrite = TRUE)
  pre_3months <- pre_3months*86400/length(from:to) # convert to mm and yearly mean
  pre_3months <- rotate(pre_3months) # change longitude
  pre_3months <- crop(pre_3months, extent)
  
  
  return(c(tmp_3months, pre_3months))
  
}





.find_filenames <- function(from, to){
  
  if(to > 2101){stop("Error")}
  
  start_years <- c(seq(1966,2086,10),2101)
  index <- 1
  
  years <- c()
  
  for(i in from:to){
    if(i < 1966 & !(1951 %in% years)){years <- c(1951)}
    else if(i > start_years[index] & i < start_years[index+1]){
      years <- c(years, start_years[index])
      index <- index + 1
    }
  }
  
  files <- sapply(years, function(i) ifelse(i == 1951 | i == 2086, paste0(i,"0101-", i+14, "1231"), paste0(i,"0101-", i+9, "1231")))

  return(files)
}
