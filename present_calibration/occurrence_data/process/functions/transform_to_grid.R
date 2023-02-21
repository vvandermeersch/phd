transform_to_grid <- function(shp, grid, species, origin){
  shp_grid <- grid[unique(unlist(st_intersects(shp[shp$species==species,], grid))),]
  shp_grid$origin <- origin
  return(shp_grid)
}