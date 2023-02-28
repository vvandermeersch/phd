
# Function to return a vector of TRUE/FALSE to filter taxa
detect_taxa <- function(samples, taxa){
  
  detected_taxa <- stringr::str_detect(samples$MHVar.2, paste0(taxa[1], "*"))
  if(length(taxa) == 1){return(detected_taxa)}
  for(i in 2:length(taxa)){
    detected_taxa <- detected_taxa + stringr::str_detect(samples$MHVar.2, paste0(taxa[i], "*"))
  }
  return(detected_taxa>0)
  
}

filter_taxa <- function(samples, taxa){
  
  detected_taxa <- detect_taxa(samples,taxa)
  
  return(samples[detected_taxa == TRUE,])
  
}

# find the name of the file to read, and position of the specific year in the raster stack
find_file_name <- function(year){
  
  file_name <- c("0_2.5", "2.5_5", "5_7.5", "7.5_10", "10_12.5", "12.5_15", "15_17.5", "17.5_20", "20_22.5")
  min_year <- c(0, 2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000)
  max_year <- c(2499, 4999, 7499, 9999, 12499, 14999, 17499, 19999, 22499)
  
  ind <- which(max_year >= year & min_year <= year)
  
  return(list(name = file_name[ind], yr_pos = max_year[ind]-year+1))
  
}
