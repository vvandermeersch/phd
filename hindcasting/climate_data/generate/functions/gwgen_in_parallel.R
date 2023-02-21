# function to run GWGEN in parallel
# see gwgen() function

gwgen_in_parallel <- function(input_file, wd, ncores){
  
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
    
    gwgen(x, output_dir = file.path(wd, "outputs", "temp"))
    
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
  
  return(output_file)

}