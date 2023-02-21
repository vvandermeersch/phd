
load_phenofit_climate_files <- function(year, folder, skip = 4, nrows = Inf){
  
  tmp_file <- paste0(folder, "ERA5LAND_", "tmp", "_", year, "_dly.fit")
  tmp <- fread(tmp_file, showProgress=F, skip = skip, nrows = nrows)
  
  tmn_file <- paste0(folder, "ERA5LAND_", "tmn", "_", year, "_dly.fit")
  tmn <- fread(tmn_file, showProgress=F, skip = skip, nrows = nrows)
  
  tmx_file <- paste0(folder, "ERA5LAND_", "tmx", "_", year, "_dly.fit")
  tmx <- fread(tmx_file, showProgress=F, skip = skip, nrows = nrows)
  
  pre_file <- paste0(folder, "ERA5LAND_", "pre", "_", year, "_dly.fit")
  pre <- fread(pre_file, showProgress=F, skip = skip, nrows = nrows)
  
  return(list(tmp = tmp[, -c(1,2)], tmn = tmn[, -c(1,2)], tmx = tmx[, -c(1,2)], pre = pre[, -c(1,2)]))
  
}