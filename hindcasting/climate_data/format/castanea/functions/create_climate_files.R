
create_climate_files <- function(ncells, folder){
  
  header <- c("# Castanea daily climate data",
              paste0('# Generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date()),
              "# y year", "# m month", "# d day",
              "# gr global radiation (mJ/d)", "# rh relative humidity (%)",
              "# ws wind speed (m/s)", "# p precipitation (mm)", 
              "# tmax max temperature (celcius degree)", "# tmin min temperature (celcius degree)",
              "# tj average temperature (celcius degree)",
              paste('# y', 'm', 'd', 'gr', 'rh', 'ws', 'p', 'tmax', 'tmin', 'tj', sep="\t"))
  
  for(i in 1:ncells){
    climate_file <- paste0(folder, i, ".txt")
    write(header, climate_file)
  }
  
}