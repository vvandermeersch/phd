library(dplyr)

load_parameter_values <- function(type, species_name, wd){
    values_file <- file.path(wd, "calibration/input_files", paste0("CastaneaSpecies_",type,".txt"))
    values <- read.table(file = values_file, skip=1, sep='\t', comment.char = "", header = TRUE) %>%
      filter(speciesName == species_name) %>%
      dplyr::select(-c(1,2))
    
    return(values)

}