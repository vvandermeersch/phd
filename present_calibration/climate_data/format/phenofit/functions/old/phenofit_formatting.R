source(paste0(wd, 'functions/get_var_name.R'))
source(paste0(wd, 'functions/get_comments.R'))
source(paste0(wd, 'functions/convert_units.R'))

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

phenofit_formatting <- function(years, var, stat, rd_folder, pd_folder, ncores=1){
  
  if(var == "wind" | var == "relative_humidity"){
    stop("For wind or relative humidity, you need to use phenofit_processing_and_formatting function !")
  }
  else if( !(var %in% c("2m_temperature", "total_precipitation", "surface_solar_radiation_downwards")) ){
    stop("Cannot recognize the variable you asked for !")
  }
  
  # To follow the progress
  p <- progressor(length(years)*12)
  
  # Get variable name as coded in NC file
  var_name <- get_var_name(var, type = "ERA5")
  
  months <- c("01", "02", "03", "04", "05", "06",
              "07", "08", "09", "10", "11", "12")
  
  plan(multisession, workers = ncores)
  
  # Parallel loop (in case of parallel computing)
  out <- future_apply(array(1:ncores), 1, function(i){
    
    # To determine number of years processed by core (in case of parallel computing)
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
        
        # Name of the file
        file <- paste0(var, "_", yr, "_", mn, ".nc")
        
        # Load NC file
        data_nc <- nc_open(paste0(rd_folder, file))
        
        # Initialisation needed only one time by year
        if(init){
          # Get longitude
          lon <- ncvar_get(data_nc,"lon")
          nlon <- length(lon)
          
          # Get latitude
          lat <- ncvar_get(data_nc,"lat")
          nlat <- length(lat)
          
          # Create dataframe
          latlon <- as.matrix(expand.grid(lat = round(lat,1), lon = round(lon,1)))
          phenofit_format_df <- data.frame(latlon)
          
          init <- FALSE
        }
        
        # Get time
        time <- ncvar_get(data_nc,"time")
        ntime <- length(time)
        
        # Load data
        array <- ncvar_get(data_nc, var_name)
        
        # Matrix of values
        vec_long <- as.vector(array)
        mat <- matrix(vec_long, nrow=nlon*nlat, ncol=ntime)
        
        # Loop on days
        for(day in 1:(ntime/24)){
          
          start <- 1+(day-1)*24
          end <- 24*day
          
          daily_value <- apply(mat[,start:end],1,stat)
          
          # Convert units (e.g. Kelvin to Celsius)
          daily_value_c <- convert_units(daily_value, var)
          
          # Fill dataframe
          phenofit_format_df <- cbind(phenofit_format_df, daily_value_c)
          
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
