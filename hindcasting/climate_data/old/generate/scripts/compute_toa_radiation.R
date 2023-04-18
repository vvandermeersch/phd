# Script to compute TOA radiation
# Much more faster to use a script rather than to use a function... Don't know why !

output_file <- file.path(output_dir,
                         paste0(strsplit(basename(gwgen_file), "_")[[1]][1], 
                                "_", strsplit(basename(gwgen_file), "_")[[1]][2], "_toarad.csv"))

# read data
gwgen_data <- fread(gwgen_file)
input_data <- fread(input_file)

# get latitude of station
latlon_data <- unique(input_data[,c("station id", "lat", "lon")])
names(latlon_data) <- c("id", "lat", "lon")

# prepare output
out_data <- left_join(data.frame(id = gwgen_data[, id]), latlon_data)
out_data <- cbind(out_data, gwgen_data[, c("year", "month", "day")])




# Calcultate DOY
# out_data$doy <- yday(as.Date(paste(1950-out_data$year, out_data$month, out_data$day, sep ="-"), format = "%y-%m-%d")) # does not work BCE
list_ids <- unique(out_data$id)
temp <- out_data[out_data$id == list_ids[1],]
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
message("Computing TOA radiation...")
start_time <- Sys.time()
for(yr in unique(out_data$year)){
  # cat(paste0("Year ", yr, "\n"))
  
  orbit <- load_orbit_parameters(yr, orbit_data)
  
  out_data_temp <- out_data[out_data$year == yr,]
  
  plan(multisession, workers = ncores)
  
  toa <- future_lapply(1:nrow(out_data_temp), function(i){
    calculate_toa(orbit, out_data_temp[i, "doy"], out_data_temp[i, "lat"])
  })
  
  plan(sequential)
  
  toa <- do.call(rbind, toa)
  
  out_data[out_data$year == yr, "toa"] <- unlist(toa[,1])
  out_data[out_data$year == yr, "dayl"] <- unlist(toa[,2])
  out_data[out_data$year == yr, "delta"] <- unlist(toa[,3])
  
  gc()
  
}
end_time <- Sys.time()
cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))

fwrite(out_data, file = output_file, col.names = T, row.names = FALSE, sep=",")

# message("Daily TOA radiation computed !")

rm(gwgen_data, out_data, input_data, out_data_temp)

toa_file <- output_file
