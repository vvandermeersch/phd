# function to launch GWGEN (Sommer & Kaplan, 2017)

# it uses with a bat script which uses MinGW64 shell (must be installed)
# see mingw_w64_gwgen.bat for details

# author : V. Van der Meersch - 13/10/2022


gwgen <- function(input_file, output_dir, dir_wgen){
  
  if(file.exists(file.path(output_dir, "error.temp"))){
    rem <- file.remove(file.path(output_dir, "error.temp"))
  }
  
  output_file <- file.path(output_dir, paste0(strsplit(basename(input_file), "\\.")[[1]][1], "_out.csv"))
  
  cmd_line <- paste("cd", dir_wgen, "&&", "mingw_w64_gwgen.bat", input_file, output_file, output_dir, sep = " ")
  
  shell(cmd_line, mustWork = TRUE)
  
  while(!file.exists(file.path(output_dir, "check.temp"))){
    Sys.sleep(5)
    if(file.exists(file.path(output_dir, "error.temp"))){
      stop(paste0("Could not converge during daily weather generation with ", input_file, " !"))
    }
  }
  
  rem <- file.remove(file.path(output_dir, "check.temp"))
  
  cat("GWGEN run is over !\n")
  
  return(output_file)
  
}

