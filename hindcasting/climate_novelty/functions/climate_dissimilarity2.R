
# Function to estimate climatic dissimilarity (novelty)

# goal: evaluating climatic dissimilarity between each cell in focal raster to all cells in baseline raster
# climatic dissimilarity is the minimum distance between the focal cell and all other baseline cells

# two methods: std_euclidean (as in Ordonnez et al. 2016), or mahalanobis (as in Burke et al. 2019)

# for std_euclidean, need interannual variability for baseline conditions


climatic_dissimilarity <- function(focal, baseline, method, interannual_sd = NULL){
  
  # create the output raster
  rout <- subset(focal, 1)
  rout[] <- NA
  
  if(method == "std_euclidean"){
    
    # baseline values
    baseline_values <- na.omit(as.matrix(baseline, wide=FALSE))

    # baseline interannual variability
    interannual_sd_values <- sapply(1:nlyr(interannual_sd), function(x){
      return(as.matrix(na.omit(subset(interannual_sd, x)[])))
    })
    
    # loop on focal cells
    for(i in 1:ncell(focal)){
      
      focal_cell_val <- extract(focal, i)
      
      if(!any(is.na(focal_cell_val) == TRUE)){
        
        colnames(baseline_values) <- colnames(focal_cell_val) <- c("temp", "pre")
        M <- as.data.frame(rbind(focal_cell_val, baseline_values)) # matrix, first row = focal cell, then baseline cells
          
        sqr_diff <- M %>% 
          mutate_all(.funs=list(function(x) (first(x) - x)^2))
        sqr_diff <- sqr_diff[-1,]/interannual_sd_values^2 # drop first line (focal cell), standardized by interannual variability
          
        dist <- sqr_diff %>% 
          mutate(SED = sqrt(rowSums(.))) %>%
          dplyr::select(SED)
          
        rout[i] <- min(dist[-1,]) # select minimum SE distance
        
      }
      
    }
    
  }else if(method == "mahalanobis"){
    
    # coerce to a matrix
    tar_data <- as.matrix(focal, wide=FALSE)
    ref_data <- as.matrix(baseline, wide=FALSE)
    
    #combine tar and ref data to estimate covariance matrix from data
    reftardat = rbind(na.omit(tar_data), na.omit(ref_data))
    S = cov(reftardat)
    
    dimrefdat = dim(ref_data);
    
    # loop on focal cells
    for(i in 1:nrow(tar_data)){
      
      tardatsub = tar_data[i,]
      
      if(!any(is.na(tardatsub) == TRUE)){
        
        anomref = ref_data - repmat(tardatsub,dimrefdat[1],1); 
        covmat = inv(S); 
        mahal_dist = sqrt(rowSums(anomref  %*% covmat * anomref)); 
        
        rout[i] <- min(mahal_dist, na.rm = T) # select minimum Mahal. distance
        
      }
        
    }
      
  }
  
  gc()
  return(rout)
  
}


