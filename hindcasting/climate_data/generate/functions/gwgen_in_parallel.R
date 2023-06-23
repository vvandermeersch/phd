# function to run GWGEN in parallel
# see gwgen() function

gwgen_in_parallel <- function(input_file, wd, ncores, dir_gwgen){
  start_time <- Sys.time()
  
  # Split GWGEN csv file in several chunks
  my_file <- fread(input_file)
  stations <- unique(my_file$`station id`)
  
  dir.create(file.path(wd, "inputs", "temp"), showWarnings = F)
  
  nstat_per_core <- ceiling(length(stations)/1000) * 1000%/%ncores
  split_files <- lapply(split(stations, ceiling(seq_along(stations)/nstat_per_core)), function(x){
    data_split <- my_file[my_file$`station id` %in% x,]
    
    # problem with GWGEN first station, need to add one artificially (we will remove it later)
    false_station <- my_file[my_file$`station id` %in% x[1],]
    false_station$`station id` <- 0 # false id
    data_split <- rbind(false_station, data_split)
    
    fwrite(data_split, file.path(wd, "inputs", "temp", paste0(x[1], ".csv")))
    return(file.path(wd, "inputs", "temp", paste0(x[1], ".csv")))
    
  })
  
  dir.create(file.path(wd, "outputs", "temp"), showWarnings = F)
  
  # GWGEN in parallel
  plan(multisession, workers = length(split_files))
  
  gwgen_split_files <- future_lapply(split_files, function(x){
    
    .gwgen(x, output_dir = file.path(wd, "outputs", "temp"), dir_gwgen)
    
  })
  
  plan(sequential)
  gc()
  
  # Merge GWGEN output files
  data_all <- gwgen_split_files %>% 
    lapply(fread) %>% 
    bind_rows()
  
  data_all <- data_all[data_all$id != 0,] #remove false station
  
  output_file <- file.path(file.path(wd, "outputs"), paste0(strsplit(basename(input_file), "\\.")[[1]][1], "_out.csv"))
  fwrite(data_all, file = output_file, 
         append = FALSE, col.names = TRUE, row.names = FALSE, sep=",")
  
  # delete temporary files
  unlink(file.path(wd, "inputs", "temp"), recursive=TRUE)
  unlink(file.path(wd, "outputs", "temp"), recursive=TRUE)
  
  message("Daily weather generated !")
  
  end_time <- Sys.time()
  cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))
  
  return(output_file)

}



# function to launch GWGEN (Sommer & Kaplan, 2017)

# it uses with a bat script which uses MinGW64 shell (must be installed)
# see mingw_w64_gwgen.bat for details

# author : V. Van der Meersch - 13/10/2022

.gwgen <- function(input_file, output_dir, dir_gwgen){
  
  if(file.exists(file.path(output_dir, "error.temp"))){
    rem <- file.remove(file.path(output_dir, "error.temp"))
  }
  
  output_file <- file.path(output_dir, paste0(strsplit(basename(input_file), "\\.")[[1]][1], "_out.csv"))
  
  cmd_line <- paste("cd", dir_gwgen, "&&", "mingw_w64_gwgen.bat", input_file, output_file, output_dir, sep = " ")
  
  shell(cmd_line, mustWork = TRUE)
  
  start_time <- Sys.time()
  while(!file.exists(file.path(output_dir, "check.temp")) & round(as.double(Sys.time()-start_time, units = "mins"), 1)< 16){
    Sys.sleep(30)
    if(file.exists(file.path(output_dir, "error.temp"))){
      stop(paste0("Could not converge during daily weather generation with ", input_file, " !"))
    }
  }
  
  rem <- file.remove(file.path(output_dir, "check.temp"))
  
  cat("GWGEN run is over !\n")
  
  return(output_file)
  
}

