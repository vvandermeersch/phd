library(dplyr)

# Function to estimate climatic dissimilarity (novelty)

# goal: evaluating climatic dissimilarity between each cell in focal raster to all cells in baseline raster
# climatic dissimilarity is the minimum distance between the focal cell and all other baseline cells

# two methods: std_euclidean (as in Ordonnez et al.), or mahalanobis (as in Burke et al.)

# for std_euclidean, need interannual variability for baseline conditions
# as in Ordonnez et al., climate conditions are summarized as 20-year means


climatic_dissimilarity <- function(focal_stack, baseline_stack, method, interannual_var_stack = NULL){
  
  # create the output raster
  rout <- subset(focal_stack, 1)
  rout[] <- NA
  
  # baseline values
  baseline_values <- sapply(1:nlayers(baseline_stack), function(x){
    return(as.matrix(na.omit(subset(baseline_stack, x)[])))
  })
  
  # baseline interannual variability
  interannual_var <- sapply(1:nlayers(interannual_var_stack), function(x){
    return(as.matrix(na.omit(subset(interannual_var_stack, x)[])))
  })
  
  # loop on focal cells
  for(i in 1:ncell(focal_stack)){
    
    focal_cell_val <- extract(focal_stack, i)
    
    if(!any(is.na(focal_cell_val) == TRUE)){
      
      M <- as.data.frame(rbind(focal_cell_val, baseline_values)) # matrix, first row = focal cell, then baseline cells
      
      # method of Ordonnez et al.
      if(method == "std_euclidean"){
        
        sqr_diff <- M %>% 
          mutate_all(.funs=list(function(x) (first(x) - x)^2))
        sqr_diff <- sqr_diff[-1,]/interannual_var # drop first line (focal cell), standardized by interannual variability
        
        dist <- sqr_diff %>% 
          mutate(SED = sqrt(rowSums(.))) %>%
          dplyr::select(SED)
        
        rout[i] <- min(dist[-1,]) # select minimum SED
        
      }
      
    }
    
  }
  
  gc()
  return(rout)
    
}



# dist <- as.data.frame(t(t(sqr_diff) / unlist(interannual_var))) %>% # standardized by interannual variability of baseline conditions
#   mutate(SED = sqrt(rowSums(.))) %>%
#   dplyr::select(SED)

  