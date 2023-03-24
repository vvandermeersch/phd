
monthly_calc <- function(climate_data, nbdays, stat = "mean"){
  
  # Leap year condition
  if(nbdays == 365){
    nbdays_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }else if(nbdays == 366){
    nbdays_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
  id_month <- t(as.matrix(rep(1:12, nbdays_month)))
  
  
  clim_data_mn <- as.data.frame(t(rbind(id_month,climate_data[,-c(1,2)], use.names=FALSE)))
  colnames(clim_data_mn)[1] <- "month"
  clim_data_mn <- clim_data_mn %>% 
    group_by(month) %>%
    summarise(across( c(1:nrow(climate_data)), get(stat)))
  clim_data_mn <- t(clim_data_mn[,-1]) # drop months
  
  return(clim_data_mn)
  
}
