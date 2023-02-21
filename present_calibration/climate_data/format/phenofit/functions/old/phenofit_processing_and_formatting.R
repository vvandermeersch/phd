source(paste0(wd, 'functions/get_var_name.R'))
source(paste0(wd, 'functions/get_comments.R'))
source(paste0(wd, 'functions/convert_units.R'))

# Package to calculate relative humidity with the ratio of saturation vapor pressure
library(humidity)

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

phenofit_processing_and_formatting <- function(years, var, stat, rd_folder, pd_folder, ncores=1){
  
  if(var %in% c("2m_temperature", "total_precipitation", "surface_solar_radiation_downwards")){
    stop(paste0("For ", var, ", you need to use phenofit_formatting function !"))
  }
  else if( var != "wind" & var != "relative_humidity" ){
    stop("Cannot recognize the variable you asked for !")
  }
  
  # To follow the progress
  p <- progressor(length(years)*12)
  
  months <- c("01", "02", "03", "04", "05", "06",
              "07", "08", "09", "10", "11", "12")
  
  # Get variable names as coded in NC file
  if(var == "wind"){
    var_1 <- "10m_u_component_of_wind"
    var_name_1 <- get_var_name(var_1, type = "ERA5")
    var_2 <- "10m_v_component_of_wind"
    var_name_2 <- get_var_name(var_2, type = "ERA5")
  }
  if(var == "relative_humidity"){
    var_1 <- "2m_temperature"
    var_name_1 <- get_var_name(var_1, type = "ERA5")
    var_2 <- "2m_dewpoint_temperature"
    var_name_2 <- get_var_name(var_2, type = "ERA5")
  }
  
  
  plan(multisession, workers = ncores)
  
  # Parallel loop in case of parallel computing
  out <- future_apply(array(1:ncores), 1, function(i){
    
    # To determine number of years processed by core in case of parallel computing
    quot <- length(years)%/%ncores
    rem <- length(years)%%ncores
    if(i<ncores){ years_i <- years[(1+(i-1)*quot):(quot*i)] }
    else{ years_i <- years[(1+(i-1)*quot):((quot*i)+rem)] }
    
    
    # Loop on years
    for(yr in years_i){
      
      p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
      
      init <- TRUE
      
      # Loop on months
      for(mn in months){
        
        # Names of the files
        file_1 <- paste0(var_1, "_", yr, "_", mn, ".nc")
        file_2 <- paste0(var_2, "_", yr, "_", mn, ".nc")
        
        # Load NC files
        data_nc_1 <- nc_open(paste0(rd_folder, file_1))
        data_nc_2 <- nc_open(paste0(rd_folder, file_2))
        
        # Initialisation needed only one time by year
        if(init){
          # Get longitude
          lon <- ncvar_get(data_nc_1,"lon")
          nlon <- length(lon)
          
          # Get latitude
          lat <- ncvar_get(data_nc_1,"lat")
          nlat <- length(lat)
          
          # Create dataframe
          latlon <- as.matrix(expand.grid(lat = round(lat,1), lon = round(lon,1)))
          phenofit_format_df <- data.frame(latlon)
          
          init <- FALSE
        }
        
        # Get time
        time <- ncvar_get(data_nc_1,"time")
        ntime <- length(time)
        
        # Load data
        array_1 <- ncvar_get(data_nc_1, var_name_1)
        array_2 <- ncvar_get(data_nc_2, var_name_2)
        
        # Matrices of values
        vec_long_1 <- as.vector(array_1)
        mat_1 <- matrix(vec_long_1, nrow=nlon*nlat, ncol=ntime)
        vec_long_2 <- as.vector(array_2)
        mat_2 <- matrix(vec_long_2, nrow=nlon*nlat, ncol=ntime)
        
        # Loop on days
        for(day in 1:(ntime/24)){
          
          start <- 1+(day-1)*24
          end <- 24*day
          
          if(var == "wind"){
            # Calculate wind speed from u and v components
            mat <- sqrt( mat_1[,start:end]^2 * mat_2[,start:end]^2 )
            
            daily_value <- apply(mat,1,stat)
            
            # Fill dataframe (no conversion needed, already in m/s)
            phenofit_format_df <- cbind(phenofit_format_df, daily_value)
          }
          
          
          if(var == "relative_humidity"){
            # Calculate relative humidity from temperature and dewpoint temperature
            
            # Magnus approximation
            #b <- 17.625
            #c <- 243.04
            #daily_value <- 100*exp( (c*b*(mat_2[,start:end]-mat_1[,start:end])) / ((c+mat_1[,start:end])*(c+mat_2[,start:end])) )
            
            # To calculate relative humidity with the ratio of saturation vapor pressure (Clausius-Clapeyron equation)
            mat <- humidity::RH(mat_1[,start:end], mat_2[,start:end])
            
            daily_value <- apply(mat,1,stat)
            
            # Fill dataframe 
            phenofit_format_df <- cbind(phenofit_format_df, daily_value)
          }
          
        }
        
        # Update progression
        p()
        
      }
      
      # Remove NA values (not land ?)
      phenofit_format_df <- na.omit(phenofit_format_df)
      
      # Get variable name as needed by Phenofit
      p_var <- get_var_name(var, stat = stat, type = "PHENOFIT")
      
      # File name
      processed_file <- paste0(pd_folder, "ERA5LAND_", p_var, "_", yr, "_dly.fit")
      
      # Fill file
      con <- file(processed_file, open="wt")
      writeLines("Climate datafile for Phenofit model", con)
      writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
      comments <- get_comments(var = p_var)
      writeLines(comments, con)
      writeLines(" ", con)
      write.table(phenofit_format_df, file = con, sep = "\t",
                  row.names = FALSE, col.names= FALSE)
      close(con)
      
    }
    
  })
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
  message("Process completed !")
  
}
