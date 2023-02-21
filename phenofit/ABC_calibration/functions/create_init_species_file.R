source(paste0(wd, "/functions/create_species_file.R"))
source(paste0(wd, "/functions/modify_species_file.R"))

create_init_species_file <- function(name, params_init, structure_file){
  species_files <- c()
  species_files <- paste0(wd, name, ".species")
  create_species_file(species_files, structure_file)
  modify_species_file(params_init, param_fixed=NULL, species_files)
  return(species_files)
}

