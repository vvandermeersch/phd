
compute_CRU_baseline <- function(from = 1921, to = 1980, extent, data_dir){
  
  pre <- rast(file.path(data_dir, "cru_ts4.07.1901.2022.pre.dat.nc"), subds = "pre")
  indices <- which(time(pre, format ="years") %in% (from:to))
  pre <- crop(subset(pre, indices), extent)
  
  tmp <- rast(file.path(data_dir, "cru_ts4.07.1901.2022.tmp.dat.nc"), subds = "tmp")
  indices <- which(time(tmp, format ="years") %in% (from:to))
  tmp <- crop(subset(tmp, indices), extent)
  
  # average three-month means
  tmp_DJF <- mean(subset(tmp, which(time(tmp, format ="months") %in% c(12,1,2))))
  tmp_MAM <- mean(subset(tmp, which(time(tmp, format ="months") %in% c(3,4,5))))
  tmp_JJA <- mean(subset(tmp, which(time(tmp, format ="months") %in% c(6,7,8))))
  tmp_SON <- mean(subset(tmp, which(time(tmp, format ="months") %in% c(9,10,11))))
  
  # average three-month sums
  indices <- sapply(from:to, function(i) which(time(tmp, format ="years") == i & time(tmp, format ="months") %in% c(12,1,2)))
  pre_DJF <- mean(rast(lapply(1:ncol(indices), function(i) sum(subset(pre, indices[,i])))))
  indices <- sapply(from:to, function(i) which(time(tmp, format ="years") == i & time(tmp, format ="months") %in% c(3,4,5)))
  pre_MAM <- mean(rast(lapply(1:ncol(indices), function(i) sum(subset(pre, indices[,i])))))
  indices <- sapply(from:to, function(i) which(time(tmp, format ="years") == i & time(tmp, format ="months") %in% c(6,7,8)))
  pre_JJA <- mean(rast(lapply(1:ncol(indices), function(i) sum(subset(pre, indices[,i])))))
  indices <- sapply(from:to, function(i) which(time(tmp, format ="years") == i & time(tmp, format ="months") %in% c(9,10,11)))
  pre_SON <- mean(rast(lapply(1:ncol(indices), function(i) sum(subset(pre, indices[,i])))))
    
  rout <- c(tmp_DJF, tmp_MAM, tmp_JJA, tmp_SON, pre_DJF, pre_MAM, pre_JJA, pre_SON)
  names(rout) <- c("tmp_DJF", "tmp_MAM", "tmp_JJA", "tmp_SON", "pre_DJF", "pre_MAM", "pre_JJA", "pre_SON")
    
  return(rout)
  
}
