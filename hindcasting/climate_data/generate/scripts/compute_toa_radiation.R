# Script to compute TOA radiation
# Much more faster to use a script rather than to use a function... Don't know why !

output_file <- file.path(output_dir,
                         paste0(strsplit(basename(gwgen_file), "_")[[1]][1], 
                                "_", strsplit(basename(gwgen_file), "_")[[1]][2], "_toarad.csv"))

# read data
gwgen_data <- fread(gwgen_file)
input_data <- fread(input_file)

# get latitude of station
lat_data <- unique(input_data[,c("station id", "lat")])
names(lat_data) <- c("id", "lat")

# prepare output
out_data <- left_join(data.frame(id = gwgen_data[, id]), lat_data)
out_data <- cbind(out_data, gwgen_data[, c("year", "month", "day")])
# Calcultate DOY
# out_data$doy <- yday(as.Date(paste(1950-out_data$year, out_data$month, out_data$day, sep ="-"), format = "%y-%m-%d")) # does not work BCE
temp <- out_data[out_data$id == 2333,]
doy_temp <- sapply(1:nrow(temp), function(i){
  if(temp[i, "month"] == 1){
    return(temp[i, "day"])
  }else{
    return(temp[i, "day"] + sum(sapply(1:(temp[i, "month"]-1), function(m) max(temp[temp$year == temp[i, "year"] & temp$month == m, "day"]))))
  }
})
out_data$doy <- rep(doy_temp, length(unique(out_data$id)))
rm(temp, doy_temp)
gc()

# compute
for(yr in unique(out_data$year)){
  cat(paste0("Year ", yr, "\n"))
  
  orbit <- load_orbit_parameters(yr, orbit_data)
  
  out_data_temp <- out_data[out_data$year == yr,]
  
  plan(multisession, workers = ncores)
  
  runt <- system.time(toa <- future_lapply(1:nrow(out_data_temp), function(i){
    calculate_toa(orbit, out_data_temp[i, "doy"], out_data_temp[i, "lat"])
  }))
  
  plan(sequential)
  
  cat(paste0("Runtime: ", runt[3], "s \n"))
  
  toa <- do.call(rbind, toa)
  
  out_data[out_data$year == yr, "toa"] <- unlist(toa[,1])
  out_data[out_data$year == yr, "dayl"] <- unlist(toa[,2])
  out_data[out_data$year == yr, "delta"] <- unlist(toa[,3])
  
  gc()
  
}

fwrite(out_data, file = output_file, col.names = T, row.names = FALSE, sep=",")

message("Daily TOA radiation computed !")

rm(gwgen_data, out_data, input_data, out_data_temp)

toa_file <- output_file
