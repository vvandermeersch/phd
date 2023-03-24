

extract_climate <- function(species_data, climate_data){
  
  climate_ext <- inner_join(species_data, climate_data, by = c("lat", "lon")) #species_data first to keep the same order (important for AUC computation)
  
  return(climate_ext)
  
}


