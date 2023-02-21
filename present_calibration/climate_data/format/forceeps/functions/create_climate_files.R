
create_climate_files <- function(ncells, folder){
  
  header <- paste0('# Forceps site file generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date())
  
  for(i in 1:ncells){
    climate_file <- paste0(folder, i, ".climate")
    write(header, climate_file)
  }
  
}


