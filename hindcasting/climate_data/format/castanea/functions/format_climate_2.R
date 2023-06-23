source(paste0(wd, 'functions/load_phenofit_climate_files.R'))
source(paste0(wd, 'functions/create_climate_files.R'))

handlers(global = TRUE)
handlers("txtprogressbar")

# new version, might be better !

format_climate_2 <- function(years, ncells, out_folder, source_folder, ncores, create_files = TRUE){
  
  init <- TRUE
  
  # loop on years
  for(yr in years){
    
    if(init){
      p <- progressor(length(years)*ncells)
      if(create_files){
        p(message = paste("Creating climate files..."), class = "sticky",amount=0)
        create_climate_files(ncells, folder = out_folder)
      }
      init <- FALSE
    }
    
    p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
    
    # only to get number of days in the year
    tmp_file <- paste0(source_folder, "HadCM3B_", "tmp", "_", yr, "_dly.fit")
    temp <- fread(tmp_file, showProgress=F, skip = 5, nrows = 1)
    ndays <- length(temp)-2
    
    # Leap year condition
    if(ndays == 365){
      ndays_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }else if(ndays == 366){
      ndays_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }
    months <- (rep(1:12, ndays_month))
    
    # number of cells per core (parallel computing)
    if(ncores > 1){
      list_cells <- split(1:ncells, cut(seq_along(1:ncells), ncores, labels = FALSE))
    }else{
      list_cells <- list(c(1:ncells))
    }
    
    p(message = paste("List_cells for last core : ", list_cells[ncores]), class = "sticky", amount = 0)
    
    # parallel settings
    plan(multisession, workers = ncores)
    
    out <- future_apply(array(1:ncores), 1, function(j){
      ncells_j <- unlist(list_cells[j])
      
      first_line <- 4 + ncells_j[[1]]
      nlines <- length(ncells_j)
      phenofit_climate <- load_phenofit_climate_files(year = yr, folder = source_folder, skip = first_line-1, nrows = nlines)
      
      for(i in 1:nlines){
        tmp <- t(phenofit_climate$tmp[i,])
        tdew <- t(phenofit_climate$tmn[i,]) # approximate dewpoint temperature with minimum temperature
        ea <- 0.6108*exp(17.27*tdew/(tdew+237.3))
        es <- 0.6108*exp(17.27*tmp/(tmp+237.3))
        rh <- round(100 * ea/es,2)
        rh <- ifelse(rh>100, 100, rh)
        
        if(anyNA(rh)){stop(paste0("Error with j ", j, " and i ", i))}
        
        cell_data <- data.frame(y = yr, m = months, d = (1:ndays),
                                gr = t(phenofit_climate$glo[i,]), rh = rh,
                                ws = t(phenofit_climate$wnd[i,]), p = t(phenofit_climate$pre[i,]),
                                tmax = t(phenofit_climate$tmx[i,]), tmin = t(phenofit_climate$tmn[i,]),
                                tj = tmp)
        #names(cell_data) <- c('# y', 'm', 'd', 'gr', 'rh', 'ws', 'p', 'tmax', 'tmin', 'tj')
        climate_file <- paste0(out_folder, ncells_j[i], ".txt")
        suppressWarnings(write.table(cell_data, climate_file, 
                                     append=TRUE, row.names=FALSE, sep="\t", quote = FALSE, col.names = FALSE))
        
        p()
        
      }
      
    })
    
    plan(sequential)
    gc()
    
  }
  
}

