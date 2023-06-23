spatial_gradient <- function(focal_raster, res){
  
  # create rasters
  rlat <- subset(focal_raster, 1)
  rlat[] <- NA
  rlon <- rlat
  rnorm <- rlat
  rangle <- rlat
  
  # transform into data.frame
  focal_mat <- as.matrix(focal_raster)
  
  # loop on focal cells
  for(i in 1:nrow(focal_mat)){
    
    for(j in 1:ncol(focal_mat)){
      
      if(!is.na(focal_mat[i,j])){
        
        ad <- ifelse(i>1 & j>1, focal_mat[i-1,j-1]-2*focal_mat[i,j-1], NA) #a-2d
        be <- ifelse(i>1, 2*focal_mat[i-1,j]-focal_mat[i,j], NA) #2b-e
        cf <- ifelse(j<ncol(focal_mat) & i>1, focal_mat[i-1,j+1]-2*focal_mat[i,j+1], NA) #c-2f
        dg <- ifelse(j>1 & i<nrow(focal_mat), 2*focal_mat[i,j-1]-focal_mat[i+1,j-1], NA) #2d-g
        eh <- ifelse(i<nrow(focal_mat), focal_mat[i,j]-2*focal_mat[i+1,j], NA) #e-2h
        fi <- ifelse(j<ncol(focal_mat) & i<nrow(focal_mat), 2*focal_mat[i,j+1]-focal_mat[i+1,j+1], NA) #2f-i
        lat_component <- sum(c(ad, be, cf, dg, eh, fi), na.rm = F)/8
        
        ba <- ifelse(i>1 & j>1, 2*focal_mat[i-1,j]-focal_mat[i-1,j-1], NA) #2b-a
        cb <- ifelse(i>1 & j<ncol(focal_mat), focal_mat[i-1,j+1]-2*focal_mat[i-1,j], NA) #c-2b
        ed <- ifelse(j>1, focal_mat[i,j]-2*focal_mat[i,j-1], NA) #e-2d
        fe <- ifelse(j<ncol(focal_mat), 2*focal_mat[i,j+1]-focal_mat[i,j], NA) #2f-e
        hg <- ifelse(j>1 & i<nrow(focal_mat), 2*focal_mat[i+1,j]-focal_mat[i+1,j-1], NA) #2h-g
        ih <- ifelse(i<nrow(focal_mat) & j<ncol(focal_mat), focal_mat[i+1,j+1]-2*focal_mat[i+1,j], NA) #i-2h
        lon_component <- sum(c(ba, cb, ed, fe, hg, ih), na.rm = F)/8
        
        rlat[i, j] <- lat_component/res
        rlon[i, j] <- lon_component/res
        norm_ij <- norm(c(lon_component/res,lat_component/res), type="2")
        rnorm[i, j] <- norm_ij
        rangle[i, j] <- atan(lat_component/lon_component)*180/pi
        
      }
    
    }
    
  }
  rout <- stack(rlat, rlon, rnorm, rangle)
  names(rout) <- c("lat_component", "lon_component", "norm", "angle")
  gc()
  
  
  return(rout)
  
}
