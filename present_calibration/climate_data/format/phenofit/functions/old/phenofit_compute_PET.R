source(paste0(wd, 'functions/compute_PET.R'))
source(paste0(wd, 'functions/get_comments.R'))

# Faster and more convenient way to read table
library(data.table)
setDTthreads(24)
library(vroom)

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")


# Needed constants

PET_constants <- data.frame(lambda = 2.45, Gsc = 0.082, sigma = 4.903e-09, alphaPT = 1.26, G = 0, z = 10)
# lambda : latent heat of vaporisation - Gsc : solar constant - sigma : Stefan-Boltzmann constan
# alphaPT : Priestley-Taylor coefficient - G : soil heat flux (= 0 with daily time step)
# z : height of wind instrument in m (= 10 in ERA5-Land data)


# method = PenmanMonteith or PriestleyTaylor (see compute_PET function)

phenofit_compute_PET <- function(years, pd_folder, method, ncores){
  
  pd_folder_method <- paste0(pd_folder, "pet_", method, "/")
  
  # To follow the progress (get nrows)
  file_nrows <-  paste0(pd_folder, "ERA5LAND_", "tmn", "_", years[1], "_dly.fit")
  nrows <- length(vroom::vroom_lines(file_nrows, altrep = TRUE, progress = FALSE, skip = 4))
  p <- progressor(length(years)*nrows)
  
  plan(multisession, workers = ncores)
  
  # Get altitude
  alt_file <- paste0(pd_folder, "ERA5LAND_Altitude.fit")
  alt <- fread(alt_file, showProgress=F)
  
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
      tmin_file <- paste0(pd_folder, "ERA5LAND_", "tmn", "_", yr, "_dly.fit")
      tmin <- fread(tmin_file, showProgress=F)
      
      tmax_file <- paste0(pd_folder, "ERA5LAND_", "tmx", "_", yr, "_dly.fit")
      tmax <- fread(tmax_file, showProgress=F)
      
      rhmin_file <- paste0(pd_folder, "ERA5LAND_", "RHmin", "_", yr, "_dly.fit")
      rhmin <- fread(rhmin_file, showProgress=F)
      
      rhmax_file <- paste0(pd_folder, "ERA5LAND_", "RHmax", "_", yr, "_dly.fit")
      rhmax <- fread(rhmax_file, showProgress=F)
      
      rs_file <- paste0(pd_folder, "ERA5LAND_", "glo", "_", yr, "_dly.fit")
      rs <- fread(rs_file, showProgress=F)
      
      uz_file <- paste0(pd_folder, "ERA5LAND_", "wnd", "_", yr, "_dly.fit")
      uz <- fread(uz_file, showProgress=F)
      
      
      
      # File name
      processed_file <- paste0(pd_folder_method, "ERA5LAND_pet_", yr, "_dly.fit")
      
      # Write header
      con <- file(processed_file, open="wt")
      writeLines("Climate datafile for Phenofit model", con)
      writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
      comments <- get_comments(var = paste0("pet", method))
      writeLines(comments, con)
      writeLines(" ", con)
      
      
      # Compute and write PET for each point
      for(j in 1:nrows){
        # Get altitude
        alt_j = as.numeric(alt[j,3])
        # Get latitude in radians
        lat_rad_j = as.numeric(tmin[j,1]*pi/180)
        
        data <- list(J = ncol(tmin)-2,
                     Tmin = tmin[j, -c(1,2)], Tmax = tmax[j, -c(1,2)], 
                     RHmin = rhmin[j, -c(1,2)], RHmax = rhmax[j, -c(1,2)], 
                     Rs= rs[j, -c(1,2)], uz = uz[j, -c(1,2)])
        
        ET <- compute_PET(data, elev = alt_j, lat_rad = lat_rad_j, constants = PET_constants, alpha = 0.23, method = "PenmanMonteith")
        ET <- c(as.numeric(tmin[j,1]), as.numeric(tmin[j,2]), ET)
        ET <- t(matrix(ET))
        
        
        write.table(ET, file = con, sep = "\t",
                    row.names = FALSE, col.names= FALSE)
        
        p()
        
      }
      
      close(con)
      
    }
    
  })
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
  message("Process completed !")
  
}




