
# Function to estimate climatic dissimilarity (novelty)

# goal: evaluating climatic dissimilarity between each cell in focal raster to all cells in baseline raster
# climatic dissimilarity is the minimum distance between the focal cell and all other baseline cells

# two methods: std_euclidean (as in Ordonnez et al. 2016), or Mahalanobis (as in Burke et al. 2019)

# for std_euclidean, need interannual variability for baseline conditions


climatic_dissimilarity <- function(focal, baseline, method, interannual_sd = NULL){
  
  # create the output distance raster
  rdist <- subset(focal, 1)
  rdist[] <- NA
  
  
  
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
          
        rdist[i] <- min(dist[-1,]) # select minimum SE distance
        
      }
      
    }
    
  }else if(method == "mahalanobis"){
    
    names(rdist) <- "mahalanobis_distance"
    
    # create the output contribution rasters
    rcontrib <- focal
    rcontrib[] <- NA
    names(rcontrib) <- paste0("contribution_", names(focal))
    
    # coerce to a matrix
    tar_data <- as.matrix(focal, wide=FALSE)
    ref_data <- as.matrix(baseline, wide=FALSE)
    
    #combine tar and ref data to estimate covariance matrix from data 
    #thus, cov. matrix is "estimated from the inference and reference climatologies" as in Burke et al. (2019)
    reftardat = rbind(na.omit(tar_data), na.omit(ref_data))
    S = cov(reftardat)
    
    dimrefdat = dim(ref_data);
    
    # the following lines are required for the Garthwaite-Koch partition (Garthwaite and Koch, 2015)
    invsd <- rep(0,dimrefdat[2])
    for(i in 1:dimrefdat[2]){invsd[i] <- 1/sqrt(S[i,i])}
    d <-diag(invsd)
    dsd <- d %*% S %*% d
    eig <- eigen(dsd)
    invRootEig <- rep(0,dimrefdat[2])
    for(i in 1:dimrefdat[2]){invRootEig[i] <- 1/sqrt(eig$val[i])}
    invdsdhalf <-  eig$vec %*% diag(invRootEig) %*% t(eig$vec)
    
    # loop on focal cells
    for(i in 1:nrow(tar_data)){
      
      tardatsub = tar_data[i,]
      
      if(!any(is.na(tardatsub) == TRUE)){
        
        anomref = ref_data - repmat(tardatsub,dimrefdat[1],1)
        covmat = inv(S) 
        mahal_dist = sqrt(rowSums(anomref  %*% covmat * anomref))
        
        rdist[i] <- min(mahal_dist, na.rm = T) # select minimum Mahal. distance
        
        # contributions of each variable to this minimal distance (Garthwaite-Koch partition)
        diff <- anomref[which.min(mahal_dist),]
        W <- invdsdhalf %*% d %*% diff
        GK_contrib <- diag(W %*% t(W))
        rcontrib[i] <- round(100*GK_contrib /sum(diag(GK_contrib)), 1)
        
      }
        
    }
      
  }
  
  gc()
  return(c(rdist, rcontrib))
  
}


