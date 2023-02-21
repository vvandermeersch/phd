calculate_grid_density <- function(shp, grid){
  intersection <- st_intersection(x = grid, y = shp)
  int_result <- intersection %>% 
    st_drop_geometry() %>%
    group_by(id_cell) %>% 
    summarise(n_occ = n())
  grid_density <- sp::merge(x = grid, y = int_result, by="id_cell", all=F)
  #grid_density[is.na(grid_density$n_occ), "n_occ"] <- 0 #(if all=T ?)
  return(grid_density)
}
