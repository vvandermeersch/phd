source(paste0(wd, 'functions/load_phenofit_climate_files.R'))
source(paste0(wd, 'functions/create_climate_files.R'))

handlers(global = TRUE)
handlers("txtprogressbar")

format_climate <- function(years, out_folder, source_folder, ncores){
  
  init <- TRUE
  
  # parallel settings
  plan(multisession, workers = ncores)
  
  # loop on years
  for(yr in years){
    
    phenofit_climate <- load_phenofit_climate_files(year = yr, folder = source_folder)
    
    if(init){
      ncells <- nrow(phenofit_climate$tmp)
      #ncells <- 10000
      p <- progressor(length(years)*ncells+1)
      p(message = paste("Creating climate files..."), class = "sticky")
      create_climate_files(ncells, folder = out_folder)
      init <- FALSE
    }
    
    p(message = paste("Processing year", yr, "..."), class = "sticky", amount = 0)
    
    ndays <- length(phenofit_climate$tmp)
    
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
    
    out <- future_apply(array(1:ncores), 1, function(j){
      ncells_j <- unlist(list_cells[j])
      
      for(i in ncells_j){
        cell_data <- data.frame(y = yr, m = months, d = (1:ndays),
                                gr = t(phenofit_climate$glo[i,]), rh = t(phenofit_climate$rh[i,]),
                                ws = t(phenofit_climate$wnd[i,]), p = t(phenofit_climate$pre[i,]),
                                tmax = t(phenofit_climate$tmx[i,]), tmin = t(phenofit_climate$tmn[i,]),
                                tj = t(phenofit_climate$tmp[i,]))
        #names(cell_data) <- c('# y', 'm', 'd', 'gr', 'rh', 'ws', 'p', 'tmax', 'tmin', 'tj')
        climate_file <- paste0(out_folder, i, ".txt")
        suppressWarnings(write.table(cell_data, climate_file, 
                                     append=TRUE, row.names=FALSE, sep="\t", quote = FALSE, col.names = FALSE))
        
        p()
        
      }
      
    })
    
    rm(phenofit_climate)
    gc()
    
  }
  
  plan(sequential)
  gc()
  
}

