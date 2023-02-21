ceiling_density_by_stage <- function (stages = NULL, c_caps = NULL) {
  
  if (length(stages) != length(c_caps)) {
    stop ("you must provide one carrying capacity for each specified stage !",
          call. = FALSE)
  }
  
  pop_dynamics <- function (landscape, timestep) {
    
    #browser()
    
    population_raster <- landscape$population
    
    # Get non-NA cells
    idx <- which(!is.na(raster::getValues(population_raster[[1]])))
    
    # get population as a matrix
    population_matrix <- raster::extract(population_raster, idx)
    #carrying_capacity <- raster::extract(cc, idx)
    
    # get degree of overpopulation, and shrink accordingly
    # if (is.null(stages)) {
    #   stages <- seq_len(ncol(population_matrix))
    # }
    
    # loop for each stage
    for(s in stages){
      
      cc <- population_matrix[ , s, drop = FALSE]
      cc[] <- c_caps[s]
      
      overpopulation <- as.vector(cc) / rowSums(population_matrix[ , s, drop = FALSE])
      
      overpopulation[is.nan(overpopulation)] <- 0
      overpopulation <- pmin(overpopulation, 1)
      population_matrix[, s] <- sweep(population_matrix[, s, drop = FALSE], 1, overpopulation, "*")
 
    }
    
    # get whole integers
    population_matrix <- steps:::round_pop(population_matrix)
    
    # put back in the raster
    population_raster[idx] <- population_matrix
    
    landscape$population <- population_raster
    
    landscape
  }
  
  result <- steps:::as.population_density_dependence(pop_dynamics)
  
  attr(result, "density_dependence_stages") <- stages
  return(result)
}

