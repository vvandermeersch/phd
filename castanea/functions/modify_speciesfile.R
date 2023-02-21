
# param_fixed <- c("tronviv")

modify_speciesfile <- function(new_params, param_fixed=NULL, file){
  
  param_fixed <- c("# speciesCode", "speciesName", param_fixed)
  
  species_parameters <- read.table(file = file, skip=40, sep='\t', comment.char = "", header = TRUE)
  names(species_parameters)[1] <- "# speciesCode"
  
  species_parameters[, !(names(species_parameters) %in% param_fixed)] <- new_params
  
  # Copy first lines
  header <- read.table(file = file, skip=0, comment.char = "", header = FALSE, nrows = 1, sep="\t")
  common_parameters <- read.table(file = file, skip=1, comment.char = "", header = FALSE, nrows = 38, sep="\t")
  write.table(header, file, row.names=FALSE, col.names=FALSE, sep="\t", quote = FALSE)
  write.table(common_parameters, file, append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t", quote = FALSE)
  write("", file, append=TRUE)
  
  # Write new parameters
  suppressWarnings(write.table(species_parameters, file, append=TRUE, row.names=FALSE, sep="\t", quote = FALSE))
  
}