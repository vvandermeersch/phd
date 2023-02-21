grid <- 1:101510

climate_path <- "D:/climate/ERA5-Land/forceeps_format"

filenames <- mixedsort(list.files(path = climate_path, pattern = "\\.climate$", recursive = T, full.names = TRUE))

plan(multisession, workers = 10)

system.time(res <- future_lapply(split(grid, ceiling(seq_along(grid)/1000)), function(x){
  
  start <- x[1]
  end <- x[length(x)]
  print(end)
  
  # yearly_results <- rbindlist(lapply(filenames[start:end], function(x){
  #   fread(x, skip = 1, select = c(3), col.names = "tmean")})) 
  
  yearly_results <- rbindlist(lapply(filenames[start:end], function(x){
    fread(x, skip = 1, select = c(6), col.names = "sumprec")})) 
  
  names(yearly_results) <- "val"
  
  n <- 32*12
  
  yearly_results$cell <- list(rep(1:(nrow(yearly_results) %/% n + 1), each = n, len = nrow(yearly_results)))
  
  mean_results <- yearly_results[, .(mean_val = mean(val)), by = cell]
  
  
  rm(yearly_results)
  gc()
  
  mean_results$mean_val
  
  
  
}))

output <- unname(unlist(res))

meteo <- as.data.frame(cbind(alt$lat[grid], alt$lon[grid], output))

colnames(meteo) <- c("lat", "lon", "sumprec")

ggplot(data=meteo, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = sumprec), color = NA)
