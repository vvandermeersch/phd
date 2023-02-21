
modify_speciesfile <- function(new_params, param_fixed=NULL, file){
  
  param_fixed_m <- c("#kID", "kName", "kSName", param_fixed, "colorRGB")
  
  species_parameters <- read.table(file = file, skip=1, sep='\t', comment.char = "", header = TRUE)
  names(species_parameters)[1] <- "#kID"
  
  kType_prefix <- substr(species_parameters$kType, 1, 1) # save type Evergreen or Deciduous 
  
  species_parameters[, !(names(species_parameters) %in% param_fixed_m)] <- new_params
  
  # if kType is not fixed, we need to add the E/D letter and convert to integer
  if(!is.null(param_fixed) & !("kType" %in% param_fixed)){
    species_parameters$kType <- paste0(kType_prefix, as.integer(round(species_parameters$kType)))
  }
  
  # convert to integer kNtol
  if(!is.null(param_fixed) & !("kNtol" %in% param_fixed)){
    species_parameters$kNTol <- as.integer(round(species_parameters$kNTol))
  }
  
  # Copy first lines
  header <- read.table(file = file, skip=0, comment.char = "", header = FALSE, nrows = 1, sep="\t")
  write.table(header, file, row.names=FALSE, col.names=FALSE, sep="\t", quote = FALSE)
  
  # Write new parameters
  suppressWarnings(write.table(species_parameters, file, append=TRUE, row.names=FALSE, sep="\t", quote = FALSE))
  
}
