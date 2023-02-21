find_consensus <- function(shp){
  consensus <- shp %>%
    group_by(id_cell) %>% 
    summarise(origin = list(unique(origin))) %>%
    dplyr::select(origin)
  
  return(consensus)
}





  