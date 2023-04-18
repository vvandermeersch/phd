# author : V. Van der Meersch

# Loading custom functions
source(paste0(wd, 'functions/compute_PET.R')) 
source(paste0(wd, 'functions/get_comments.R'))


# Faster and more convenient way to read table
library(data.table)
setDTthreads(1)


# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")


# Needed constants
PET_constants <- data.frame(lambda = 2.45, Gsc = 0.082, sigma = 4.903e-09, alphaPT = 1.26, G = 0, z = 10)
  # lambda : latent heat of vaporisation - Gsc : solar constant - sigma : Stefan-Boltzmann constan
  # alphaPT : Priestley-Taylor coefficient - G : soil heat flux (= 0 with daily time step)
  # z : height of wind instrument in m (= 10 in ERA5-Land data)


# Function parameters :
  # - pd_folder : folder where climate data (Phenofit format) are located
  # this pd_folder needs two subfolders : "/pet_PenmanMonteith" and "/pet_PriestleyTaylor"
  #
  # - method = PenmanMonteith or PriestleyTaylor (see compute_PET function)
  #
  # - years : e.g. 1990:2000
  #
  # - ncores : for parallelisation over years
 

phenofit_compute_PET <- function(years, pd_folder, method, ncores, transform_NA_and_zero=FALSE){
  
  plan(multisession, workers = ncores)
  
  # Get altitude
  alt_file <- paste0(pd_folder, "HadCM3B_Altitude.fit")
  alt <- fread(alt_file, showProgress=F)
  nrows <- nrow(alt)
  
  p <- progressor(length(years))
  
  # Parallel loop in case of parallel computing
  out <- future_apply(array(1:ncores), 1, function(i){
    
    # To determine number of years processed by core in case of parallel computing
    quot <- length(years)%/%ncores
    rem <- length(years)%%ncores
    if(i<ncores){ years_i <- years[(1+(i-1)*quot):(quot*i)] 
    }else{ years_i <- years[(1+(i-1)*quot):((quot*i)+rem)] }
    
    
    for(yr in years_i){
      
      p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
      
      # Read variables
      tmin_file <- paste0(pd_folder, "HadCM3B_", "tmn", "_", yr, "_dly.fit")
      tmin <- fread(tmin_file, showProgress=F)
      
      tmax_file <- paste0(pd_folder, "HadCM3B_", "tmx", "_", yr, "_dly.fit")
      tmax <- fread(tmax_file, showProgress=F)
      
      cld_file <- paste0(pd_folder, "HadCM3B_", "cld", "_", yr, "_dly.fit")
      cld <- fread(cld_file, showProgress=F)
      
      rs_file <- paste0(pd_folder, "HadCM3B_", "glo", "_", yr, "_dly.fit")
      rs <- fread(rs_file, showProgress=F)
      
      uz_file <- paste0(pd_folder, "HadCM3B_", "wnd", "_", yr, "_dly.fit")
      uz <- fread(uz_file, showProgress=F)
      
      p(message = paste("Climate files loaded !"), class = "sticky", amount = 0)
      
      # Compute PET
      alt_k <- as.numeric(unlist(alt[,3]))
      lat_rad_k = as.numeric(unlist(alt[,1]*pi/180)) # convert in radian
      tmin_k <- as.matrix(tmin[, -c(1,2)])
      tmax_k <- as.matrix(tmax[, -c(1,2)])
      cld_k <- as.matrix(cld[, -c(1,2)])
      rs_k <- as.matrix(rs[, -c(1,2)])
      uz_k <-as.matrix( uz[, -c(1,2)])
      J = matrix(rep(1:ncol(tmin_k), nrows), nrow=nrows, byrow = TRUE)
      
      data <- list(J = J,
                   Tmin = tmin_k, Tmax = tmax_k, 
                   cld = cld_k, 
                   Rs= rs_k, uz = uz_k)
      
      ET <- compute_PET(data, elev = alt_k, lat_rad = lat_rad_k, constants = PET_constants, alpha = 0.23, method = "PenmanMonteith")
      
      # Add lat and lon
      latlon <- as.matrix(tmin[,1:2])
      ET <- cbind(latlon, ET)
      
      # File name
      processed_file <- paste0(pd_folder, "HadCM3B_pet_", yr, "_dly.fit")
      
      # Write header
      con <- file(processed_file, open="wt")
      writeLines("Climate datafile for Phenofit model", con)
      writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
      comments <- get_comments(var = "pet")
      writeLines(comments, con)
      writeLines(" ", con)
      
      if(transform_NA_and_zero){
        ET[is.na(ET) | ET < 0] <- 0
      }
      
      write.table(ET, file = con, sep = "\t",
                  row.names = FALSE, col.names= FALSE)
      
      close(con)
      
      p(message = paste("Year", yr, "completed !"), class = "sticky")
      
    }
    
  })
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
  message("Process completed !")
  
}  
  










