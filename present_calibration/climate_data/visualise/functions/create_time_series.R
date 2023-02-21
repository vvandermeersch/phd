
create_time_series <- function(var, year, pd_folder){
  
  if(var == "pet"){
    pd_folder_method <- paste0(pd_folder, "pet_", "PenmanMonteith", "/")
    petPM_file <- paste0(pd_folder_method, "ERA5LAND_", var, "_", year, "_dly.fit")
    petPM <- fread(petPM_file, showProgress=F)
    pet_file <- paste0(processeddata_folder, "ERA5LAND_", var, "_", year, "_dly.fit")
    pet <- fread(pet_file, showProgress=F)
    
    colnames(petPM)[1:2] <- c("lat", "lon")
    petPM$lat <- round(petPM$lat, 1)
    petPM$lon <- round(petPM$lon, 1)
    petPM_montpellier <- petPM[petPM$lat==43.6 & petPM$lon==3.8, -c(1,2)]
    petPM_seville <- petPM[petPM$lat==37.4 & petPM$lon==-5.9, -c(1,2)]
    petPM_kiev <- petPM[petPM$lat==50.5 & petPM$lon==30.5, -c(1,2)]
    petPM_oslo <- petPM[petPM$lat==59.9 & petPM$lon==10.7, -c(1,2)]
    
    colnames(pet)[1:2] <- c("lat", "lon")
    pet$lat <- round(pet$lat, 1)
    pet$lon <- round(pet$lon, 1)
    pet_montpellier <- pet[pet$lat==43.6 & pet$lon==3.8, -c(1,2)]
    pet_seville <- pet[pet$lat==37.4 & pet$lon==-5.9, -c(1,2)]
    pet_kiev <- pet[pet$lat==50.5 & pet$lon==30.5, -c(1,2)]
    pet_oslo <- pet[pet$lat==59.9 & pet$lon==10.7, -c(1,2)]
    
    petPM_cities <- as.data.frame(t(rbind(t(1:(ncol(pet)-2)),petPM_montpellier, petPM_seville, petPM_kiev, petPM_oslo, use.names=FALSE)))
    names(petPM_cities) <- c("day", "Montpellier", "Seville", "Kiev", "Oslo")
    petPM_cities <- melt(petPM_cities, id.vars="day")
    petPM_cities$method <- "Penman Monteith"
    
    pet_cities <- as.data.frame(t(rbind(t(1:(ncol(pet)-2)),pet_montpellier, pet_seville, pet_kiev, pet_oslo, use.names=FALSE)))
    names(pet_cities) <- c("day", "Montpellier", "Seville", "Kiev", "Oslo")
    pet_cities <- melt(pet_cities, id.vars="day")
    pet_cities$method <- "ERA5-Land"
    
    pet_data <- rbind(petPM_cities, pet_cities)
    
    clim_plot <- ggplot(data=pet_data, aes(day,value, col=method)) + 
      geom_line() + 
      facet_wrap(~variable) +
      theme_minimal() +
      theme(legend.position="bottom") +
      theme(legend.title=element_blank()) +
      #ggtitle("Daily potential evapotranspiration") +
      ylab("Evapotranspiration (mm)") +
      xlab("Day")
    
    return(clim_plot)
 
  }else{
    clim_file <- paste0(processeddata_folder, "ERA5LAND_", var, "_", year, "_dly.fit")
    clim <- fread(clim_file, showProgress=F)
    
    colnames(clim)[1:2] <- c("lat", "lon")
    clim$lat <- round(clim$lat, 1)
    clim$lon <- round(clim$lon, 1)
    
    clim_montpellier <- clim[clim$lat==43.6 & clim$lon==3.8, -c(1,2)]
    clim_seville <- clim[clim$lat==37.4 & clim$lon==-5.9, -c(1,2)]
    clim_kiev <- clim[clim$lat==50.5 & clim$lon==30.5, -c(1,2)]
    clim_oslo <- clim[clim$lat==59.9 & clim$lon==10.7, -c(1,2)]
    
    clim_cities <- as.data.frame(t(rbind(t(1:(ncol(clim)-2)),clim_montpellier, clim_seville, clim_kiev, clim_oslo, use.names=FALSE)))
    names(clim_cities) <- c("day", "Montpellier", "Seville", "Kiev", "Oslo")
    clim_cities <- melt(clim_cities, id.vars="day")
    
    clim_plot <- ggplot(clim_cities, aes(day,value)) + 
      geom_line() + 
      facet_wrap(~variable) +
      theme_minimal()
    
    if(var == "tmp"){
      clim_plot <- clim_plot +
        #ggtitle("Daily mean temperature") +
        ylab("Temperature (°C)") +
        xlab("Day")
    }else if(var =="glo"){
      clim_plot <- clim_plot +
        #ggtitle("Daily global radiation") +
        ylab("Global radiation (MJ/m²)") +
        xlab("Day")
    }else if(var =="pre"){
      clim_plot <- clim_plot +
        #ggtitle("Daily precipitation") +
        ylab("Precipitation (mm)") +
        xlab("Day")
    }
  
    return(clim_plot)
    
  }
  
}
