library(dplyr)

create_speciesfile <- function(file, structure_file, species_name = NULL){
  
  common_parameters <- read.table(file = structure_file, skip=1, comment.char = "", header = FALSE, nrows = 38)
  # names(common_parameters) <- c("name", "value")
  
  species_parameters <- read.table(file = structure_file, skip=40, sep='\t', comment.char = "", header = TRUE)
  names(species_parameters)[1] <- "# speciesCode"
  
  header <- paste0("# Species file for Castanea, generated on Rstudio with CMA-ES algorithm, on the ", Sys.Date())
  write(header, file)
  
  write.table(common_parameters, file, append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t", quote = FALSE)
  write("", file, append=TRUE)
  
  if(is.null(species_name)){
    suppressWarnings(write.table(species_parameters, file, append=TRUE, row.names=FALSE, sep="\t", quote = FALSE))
  }else{
    suppressWarnings(write.table(species_parameters[species_parameters$speciesName == species_name,], file, 
                                 append=TRUE, row.names=FALSE, sep="\t", quote = FALSE))
  }
  
}

