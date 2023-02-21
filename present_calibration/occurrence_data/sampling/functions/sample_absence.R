
# env_data is needed because ERA5 land does not cover exactly all points (coastal points)

sample_absence <- function(species_data, EUForest, ERA5land, nb_samples, env_data){
  
  # prepare EUForest data
  EUForest_sf <- st_as_sf(EUForest, coords = c("X","Y"), crs=st_crs(3035))
  colnames(EUForest_sf)[2] <- "species"
  EUForest_rast <- st_transform(EUForest_sf, crs = st_crs(4326)) %>% 
    dplyr::select(species, geometry) %>% 
    rasterize(ERA5land, field=1, fun='count')
  EUForest_rast[EUForest_rast > 0] <- 1
  EUForest_rast[is.na(EUForest_rast)] <- 0
  
  # prepare specues_data
  species_data[species_data > 0] <- 1
  species_data[is.na(species_data)] <- 0
  
  EUForest_not_species_rast <- EUForest_rast-species_data
  EUForest_not_species <- as.data.frame(EUForest_not_species_rast, xy=T) %>%
    dplyr::mutate(lon = round(x,1),
                  lat = round(y,1)) %>% 
    dplyr::filter(layer>0) %>% 
    dplyr::select(-c(layer, x,y)) 
  
  # drop points not covered by climate data
  env_data$lat <- round(env_data$lat,1)
  env_data$lon <- round(env_data$lon,1)
  EUForest_not_species <- inner_join(EUForest_not_species, env_data, by = c("lon", "lat")) %>%
    dplyr::select(c(lon, lat))
  
  if(!is.null(nb_samples)){
    absence_data <- EUForest_not_species[sample(1:nrow(EUForest_not_species),nb_samples,replace=FALSE),]
  }else{
    absence_data <- EUForest_not_species #if no sample size given by the user, return all the absence points
  }
  
  
  return(absence_data)
  
}