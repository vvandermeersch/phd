
compute_peqin <- function(input_df){
  
  cat("Computing precipitation equitability index...\n")
  
  input_df$temp <- (input_df$min.temperature + input_df$max.temperature)/2
  temp_mn <- rast(lapply(1:12, function(m) rast(input_df[input_df$month == m, c("lon", "lat", "temp")])))
  pre_mn <- rast(lapply(1:12, function(m) rast(input_df[input_df$month == m, c("lon", "lat", "precipitation")])))
  
  # find coldest and warmest months
  coldest_index <- app(temp_mn, which.min)
  warmest_index <- app(temp_mn, which.max)
  
  # extract temperature and precipitation of these months
  t_cm <- .extract_values_from_index(temp_mn, coldest_index)
  t_wm <- .extract_values_from_index(temp_mn, warmest_index)
  p_cm <- .extract_values_from_index(pre_mn, coldest_index)
  p_wm <- .extract_values_from_index(pre_mn, warmest_index)
  
  # calculate the index
  peqin <- ifel(p_wm + p_cm > 0, max(2 * (p_wm - p_cm) / (p_wm + p_cm), 0), 0)
  
  return(list(peqin = peqin, t_cm = t_cm))
  
}



.extract_values_from_index <- function(rvalues, rindex){
  r <- rindex
  r[] <- NA
  for(i in 1:dim(rvalues)[3]){
    r_i <- rindex
    r_i[r_i != i] <- NA
    rvalues_i <- mask(subset(rvalues, i), r_i)
    rvalues_i[is.na(rvalues_i)] <- NA
    r <- sum(r, rvalues_i, na.rm = T)
  }
  return(r)
}
