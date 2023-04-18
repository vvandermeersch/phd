# function to interpolate daily clear-sky radiations from monthly values

# author : V. Van der Meersch - 17/10/2022

interpolate_cs_radiation <- function(years, extent, raw_clim_dir, output_dir){
  
  # get file spec
  file_spec <- years_to_file(years)
  outname <- ifelse(!is.na(years [2]) ,paste0(years[1], "_",years [2],"BP"), paste0(years[1], "_BP"))
  
  # write header 
  header <- c("id", "year", "month", "day", "cs_radiation")
  write.table(t(header), file = file.path(output_dir, paste0(outname, "_csrad.csv")), col.names = FALSE, row.names = FALSE, sep=",")
  
  # load raster files
  # r_rad <- raster::stack(file.path(source_dir,  paste0("NAME_", file_spec$name, ".nc")))
  
  # r_cells <- crop(subset(r_rad, 1), extent)
  
  # get months (ie raster stack range)
  rmonths <- year_to_months(years[1], years[2], file_spec$min)
  
  # keep only months we need, and crop
  # r_rad <- crop(subset(r_rad, rmonths$min:rmonths$max), extent)
  
  # mn beginning from 1
  rmn_min <- rmonths$min-(rmonths$min-1)
  rmn_max <- rmonths$max-(rmonths$min-1)
  seq_mn <- seq(rmn_min, rmn_max, 12)
  seq_dmn <- c(17, 47, 75, 105, 135, 162, 198, 228, 258, 288, 318, 344)/30
  
  if(!is.na(years[2])){
    list_of_years <- 1950-seq(years[1], years[2], 1)
  }else{
    list_of_years <- 1950-years[1]
  }
 
  
  # dummy data
  seq_rad <- c(9,13, 18, 23, 25, 27, 25, 21, 19, 15, 12, 9)
   
  for(id in 1:4){ # loop on cells
    
    diff_yr <- ifelse(!is.na(years [2]), diff(years), 0)
    
    for(yr in 1:(diff_yr+1)){ # loop on years
      
      year <- list_of_years[yr]
      ndaymonths <- leap_year(year)
      
      # prepare monthly radiation values
      rad <- sapply(seq_mn[yr]:(seq_mn[yr]+11), function(i){ 
        
        #cs_rad <- subset(r_rad, i)
        
        i_bis <- ifelse(i%%12 == 0, 12,  i%%12)
        cs_rad <- seq_rad[i_bis]
        
        data <- c(cs_rad)
        
      })
      data <- data.frame(rad = rad, t = seq_dmn)
      
      # calculate period
      ssp <- spectrum(data$rad)  
      per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
      
      # fitting a sinusoidal with 1st and 2nd harmonics
      fit <- lm(rad ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t), data = data)
      
      # interpolate daily values
      newdata <- data.frame(t = seq(1, sum(ndaymonths), 1)/30)
      newdata$rad <- predict(fit, newdata = newdata, type = "response")
      
      # daily values
      table <- lapply(1:12, function(mn){
        
        dayb <- ifelse(mn == 1, 1, sum(ndaymonths[1:mn-1])+1)
        dayf <- sum(ndaymonths[1:mn])
        
        data <- data.frame(id, year, mn, 1:ndaymonths[mn], round(newdata[dayb:dayf,]$rad, 3))
        
      })
      
      table <- do.call(rbind, table)
      
      write.table(table, file = file.path(output_dir, paste0(outname, "_csrad.csv")), append = T, col.names = FALSE, row.names = FALSE, sep=",")
      
    }
    
  }
  
  gc()
  
  message("Csv file of interpolated daily values created !")
  
  return(file.path(output_dir, paste0(outname, "_csrad.csv")))
  
  
  
  
  
}
