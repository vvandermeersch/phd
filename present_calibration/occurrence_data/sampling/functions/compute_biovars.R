library(dismo, include.only = 'biovars')
options(future.globals.maxSize= 1000*1024^2)

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

compute_biovars <- function(years, pd_folder, bio_folder, ncores=1){
  
  init <- TRUE

  for(yr in years){
    
    # Load data 
    tmin_file <- paste0(pd_folder, "ERA5LAND_", "tmn", "_", yr, "_dly.fit")
    tmin <- fread(tmin_file, showProgress=F)
    nrows <- nrow(tmin)
    nbdays <- ncol(tmin) - 2
    
    # initialise progressor (only one time)
    if(init){
      p <- progressor(length(years)*nrows*12)
      init <- FALSE
    }
    
    p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
    
    tmax_file <- paste0(pd_folder, "ERA5LAND_", "tmx", "_", yr, "_dly.fit")
    tmax <- fread(tmax_file, showProgress=F)
    
    pre_file <- paste0(pd_folder, "ERA5LAND_", "pre", "_", yr, "_dly.fit")
    pre <- fread(pre_file, showProgress=F)
    
    p(message = paste("Climate files loaded !"), class = "sticky", amount = 0)
    
    # Leap year condition
    if(nbdays == 365){
      nbdays_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }else if(nbdays == 366){
      nbdays_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }
    
    
    plan(multisession, workers = ncores)
    
    
    # Parallel loop in case of parallel computing
    system.time(out <- future_lapply(1:ncores, function(j){
      
      # To determine number of rows processed by each core in case of parallel computing
      quot <- nrows%/%ncores
      rem <-  nrows%%ncores
      if(j<ncores){ nrows_j <- (1+(j-1)*quot):(quot*j) 
      }else{ nrows_j <- (1+(j-1)*quot):((quot*j)+rem) }
      
      # Initialise matrices
      tmin_mn_j <- matrix(nrow = length(nrows_j), ncol = 12)
      tmax_mn_j <- matrix(nrow = length(nrows_j), ncol = 12)
      pre_mn_j <- matrix(nrow = length(nrows_j), ncol = 12)
      
      
      for(i in 1:length(nrows_j)){
        
        # Loop on months
        for(mn in 1:12){
          
          # Get the indices of days where to start and end
          if(mn == 1){
            firstd <- 1
            lastd <- nbdays_month[mn]
          }else{
            firstd <- sum(nbdays_month[1:(mn-1)])+1
            lastd <- sum(nbdays_month[1:(mn)])
          }
          
          # Add 2 to avoid lat et lon columns
          firstd <- firstd +2
          lastd <- lastd + 2
          
          tmin_mn_j[i,mn] <- min(tmin[i,firstd:lastd])
          tmax_mn_j[i,mn] <- max(tmax[i,firstd:lastd])
          pre_mn_j[i,mn] <- sum(pre[i,firstd:lastd])
          
          p()
        }
        
      }
      
      return(list(tmin = tmin_mn_j, tmax = tmax_mn_j, pre = pre_mn_j))
       
    }))
    
    # Close background workers (in case of parallel computing)
    plan(sequential)
    
    # Free memory
    gc(verbose = FALSE)
    
    # Bind lists
    tmin_mn <- c()
    tmax_mn <- c()
    pre_mn <- c()
    for(i in 1:ncores){
      tmin_mn <- rbind(tmin_mn, out[[i]][["tmin"]])
      tmax_mn <- rbind(tmax_mn, out[[i]][["tmax"]])
      pre_mn <- rbind(pre_mn, out[[i]][["pre"]])
      
    }
    
    biovars <- matrix(nrow = nrows, ncol = 21) # lat + lon + 19 biovars
    biovars[,1:2] <- as.matrix(tmin[,1:2])
    p(message = paste("Computing bioclim varibles..."), class = "sticky", amount = 0)
    biovars_values <- biovars(pre_mn, tmin_mn, tmax_mn)
    biovars[,3:21] <- as.matrix(biovars_values)
    colnames(biovars) <- c("lat", "lon", colnames(biovars_values))
    
    save_file <- paste0(bio_folder, "biovars_", yr,".Rdata")
    save(as.data.frame(biovars), file = save_file)
    
    p(message = paste("Year", yr, "completed !"), class = "sticky", amount=0)
    
  }

}