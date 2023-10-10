
average_to_three_month_means <- function(years, name_sim, data_dir, verbose = F){
  
  cat("Computing three-month means...\n")
  
  temp_DJF <- rast()
  temp_MAM <- rast()
  temp_JJA <- rast()
  temp_SON <- rast()
  
  pre_DJF <- rast()
  pre_MAM <- rast()
  pre_JJA <- rast()
  pre_SON <- rast()
  
  for(yr in years){
    
    if(verbose){cat(paste0("   Year ", yr,"\n"))}
    
    temp <- data.frame(fread(file.path(data_dir, paste0(name_sim, "_tmp_", yr, "_dly.fit"))))
    leap_year <- ncol(temp)-2 == 366 # leap year ?
    
    temp_DJF_yr <- rast(data.frame(lon = temp[,2], lat = temp[,1], tas = rowMeans(temp[,c((1+2):(59+2+leap_year),(335+2+leap_year):(365+2+leap_year))])))
    temp_MAM_yr <- rast(data.frame(lon = temp[,2], lat = temp[,1], tas = rowMeans(temp[,(60+2+leap_year):(151+2+leap_year)])))
    temp_JJA_yr <- rast(data.frame(lon = temp[,2], lat = temp[,1], tas = rowMeans(temp[,(152+2+leap_year):(243+2+leap_year)])))
    temp_SON_yr <- rast(data.frame(lon = temp[,2], lat = temp[,1], tas = rowMeans(temp[,(244+2+leap_year):(334+2+leap_year)])))
    
    temp_DJF <- c(temp_DJF, temp_DJF_yr)
    temp_MAM <- c(temp_MAM, temp_MAM_yr)
    temp_JJA <- c(temp_JJA, temp_JJA_yr)
    temp_SON <- c(temp_SON, temp_SON_yr)
    
    pre <- data.frame(fread(file.path(data_dir, paste0(name_sim, "_pre_", yr, "_dly.fit"))))
    pre_DJF_yr <- rast(data.frame(lon = pre[,2], lat = pre[,1], tas = rowSums(pre[,c((1+2):(59+2+leap_year),(335+2+leap_year):(365+2+leap_year))])))
    pre_MAM_yr <- rast(data.frame(lon = pre[,2], lat = pre[,1], tas = rowSums(pre[,(60+2+leap_year):(151+2+leap_year)])))
    pre_JJA_yr <- rast(data.frame(lon = pre[,2], lat = pre[,1], tas = rowSums(pre[,(152+2+leap_year):(243+2+leap_year)])))
    pre_SON_yr <- rast(data.frame(lon = pre[,2], lat = pre[,1], tas = rowSums(pre[,(244+2+leap_year):(334+2+leap_year)])))
    
    pre_DJF <- c(pre_DJF, pre_DJF_yr)
    pre_MAM <- c(pre_MAM, pre_MAM_yr)
    pre_JJA <- c(pre_JJA, pre_JJA_yr)
    pre_SON <- c(pre_SON, pre_SON_yr)
    
  }
  
  temp_DJF <- mean(temp_DJF)
  temp_MAM <- mean(temp_MAM)
  temp_JJA <- mean(temp_JJA)
  temp_SON <- mean(temp_SON)
  
  pre_DJF <- mean(pre_DJF)
  pre_MAM <- mean(pre_MAM)
  pre_JJA <- mean(pre_JJA)
  pre_SON <- mean(pre_SON)
  
  rout <- c(temp_DJF, temp_MAM, temp_JJA, temp_SON, pre_DJF, pre_MAM, pre_JJA, pre_SON)
  names(rout) <- c("tmp_DJF", "tmp_MAM", "tmp_JJA", "tmp_SON", "pre_DJF", "pre_MAM", "pre_JJA", "pre_SON")
  
  return(rout)
}
