
load_phenofit_climate_files <- function(year, folder, skip = 4, nrows = Inf){
  
  tmp_file <- paste0(folder, "HadCM3B_", "tmp", "_", year, "_dly.fit")
  tmp <- fread(tmp_file, showProgress=F, skip = skip, nrows = nrows)
  
  tmn_file <- paste0(folder, "HadCM3B_", "tmn", "_", year, "_dly.fit")
  tmn <- fread(tmn_file, showProgress=F, skip = skip, nrows = nrows)
  
  tmx_file <- paste0(folder, "HadCM3B_", "tmx", "_", year, "_dly.fit")
  tmx <- fread(tmx_file, showProgress=F, skip = skip, nrows = nrows)
  
  tdew_file <- paste0(folder, "HadCM3B_", "dtm", "_", year, "_dly.fit")
  tdew <- fread(tdew_file, showProgress=F, skip = skip, nrows = nrows)
  
  glo_file <- paste0(folder, "HadCM3B_", "glo", "_", year, "_dly.fit")
  glo <- fread(glo_file, showProgress=F, skip = skip, nrows = nrows)
  
  pre_file <- paste0(folder, "HadCM3B_", "pre", "_", year, "_dly.fit")
  pre <- fread(pre_file, showProgress=F, skip = skip, nrows = nrows)
  
  wnd_file <- paste0(folder, "HadCM3B_", "wnd", "_", year, "_dly.fit")
  wnd <- fread(wnd_file, showProgress=F, skip = skip, nrows = nrows)
  
  return(list(tmp = tmp[, -c(1,2)], tmn = tmn[, -c(1,2)], tmx = tmx[, -c(1,2)], 
              tdew = tdew[, -c(1,2)], glo = glo[, -c(1,2)], pre = pre[, -c(1,2)], 
              wnd = wnd[, -c(1,2)]))
  
}