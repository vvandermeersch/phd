library(dismo, include.only = 'biovars')
options(future.globals.maxSize= 1000*1024^2)

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

source(file.path(wd, 'functions','monthly_calc.R'))



compute_biovars <- function(years, pd_folder, bio_folder, ncores){
  
  # initialise progressor
  p <- progressor(length(years)*5)
  
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
      
      # p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
      
      # Load data 
      tmin_file <- file.path(pd_folder, paste0("HadCM3B_", "tmn", "_", yr, "_dly.fit"))
      tmin <- fread(tmin_file, showProgress=F)
      nbdays <- ncol(tmin) - 2
      
      tmax_file <- file.path(pd_folder, paste0("HadCM3B_", "tmx", "_", yr, "_dly.fit"))
      tmax <- fread(tmax_file, showProgress=F)
      
      pre_file <- file.path(pd_folder, paste0("HadCM3B_", "pre", "_", yr, "_dly.fit"))
      pre <- fread(pre_file, showProgress=F)
      
      # p(message = paste("Climate files loaded !"), class = "sticky")
      p()
      
      # Monthly means
      # p(message = paste("Computing monthly values for year", yr, "..."), class = "sticky", amount = 0)
      tmin_mn <- monthly_calc(tmin, nbdays, stat = "mean") # average monthly minimum temperature
      p()
      tmax_mn <- monthly_calc(tmax, nbdays, stat = "mean") # average monthly maximum temperature
      p()
      pre_mn <- monthly_calc(pre, nbdays, stat = "sum")
      p()
      
      biovars <- matrix(nrow = nrow(tmin_mn), ncol = 21) # lat + lon + 19 biovars
      biovars[,1:2] <- as.matrix(tmin[,1:2])
      # p(message = paste("Computing bioclim variables for year", yr, "..."), class = "sticky", amount = 0)
      biovars_values <- dismo::biovars(pre_mn, tmin_mn, tmax_mn)
      biovars[,3:21] <- as.matrix(biovars_values)
      colnames(biovars) <- c("lat", "lon", colnames(biovars_values))
      biovars <- as.data.frame(biovars)
      save_file <- file.path(bio_folder, paste0("biovars_", yr,".Rdata"))
      save(biovars, file = save_file)
      
      # p(message = paste("Year", yr, "completed !"), class = "sticky")
      p()
      
    }
    
  }, future.seed = TRUE)
  
  # Free memory
  gc(verbose = FALSE)
  
  # Close background workers (in case of parallel computing)
  plan(sequential)
  
}