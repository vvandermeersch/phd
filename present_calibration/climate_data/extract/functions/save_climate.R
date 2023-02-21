source("C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/climate_data/format/phenofit/functions/get_comments.R")

save_climate <- function(climate_data, yr, output_folder, p_var, species, method=NULL){
  
  # File name
  if(p_var %in% c("Altitude", "WHC")){
    output_file <- paste0(output_folder, "ERA5LAND_", p_var, ".fit")
  }else{
    output_file <- paste0(output_folder, "ERA5LAND_", p_var, "_", yr, "_dly.fit")
  }
  
  
  # Fill file
  con <- file(output_file, open="wt")
  writeLines(paste0("Climate datafile for Phenofit model - extracted for ", species), con)
  writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
  if(!is.null(method)){
    p_var <-paste0(p_var, method)
  }
  comments <- get_comments(var = p_var)
  writeLines(comments, con)
  writeLines(" ", con)
  write.table(climate_data, file = con, sep = "\t",
              row.names = FALSE, col.names= FALSE)
  close(con)
  
}