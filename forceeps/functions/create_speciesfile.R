library(dplyr)

create_speciesfile <- function(file, structure_file, species_name = NULL){
  
  species_parameters <- read.table(file = structure_file, skip=1, sep='\t', comment.char = "", header = TRUE)
  names(species_parameters)[1] <- "#kID"
  
  header <- paste0("# Species file for Forceeps, generated on Rstudio with CMA-ES algorithm, on the ", Sys.Date())
  write(header, file)
  
  if(is.null(species_name)){
    suppressWarnings(write.table(species_parameters, file, append=TRUE, row.names=FALSE, sep="\t", quote = FALSE))
  }else{
    suppressWarnings(write.table(species_parameters[species_parameters$kName == species_name,], file, 
                                 append=TRUE, row.names=FALSE, sep="\t", quote = FALSE))
  }
  
}
