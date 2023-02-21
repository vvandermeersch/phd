
create_commandfile <- function(file, cf_options, species_file = NULL){
  
  if(is.null(species_file)){
    species_file <- cf_options$speciesFileName
  }
  
  header <- c("# Forceps script command file, format victor.SimulationCommandReaderVictor", 
              paste0('# Generated on Rstudio by user ', Sys.info()["user"], ", on the ", Sys.Date()), "")
  write(header, file)
  
  lines_to_write <- c(paste("setupFileName","=", cf_options$setupFileName, sep=" "), 
                      paste("climateFolder","=", cf_options$climateFolder, sep=" "),
                      paste("numberOfYearsToBeJumped","=", cf_options$numberOfYearsToBeJumped, sep=" "), 
                      paste("exportTimeStep","=", cf_options$exportTimeStep, sep=" "), 
                      paste("numberOfYears","=", cf_options$numberOfYears, sep=" "), 
                      paste("speciesFileName","=", species_file, sep=" "), 
                      "",
                      paste("#siteFileName", "climateFileName", sep="\t")
                      )
  
  write(lines_to_write , file, append = TRUE)
  
  sim_lines <- sapply(cf_options$grid, function(i){
    paste(paste0(i, ".site"), paste0(i, ".climate"), sep="\t")
  })
  
  write(sim_lines, file, append = TRUE)

  
}
