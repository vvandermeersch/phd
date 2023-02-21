

if(download_data){
  
  # Create Europe bounding box
  europe <- '{"type": "Polygon", 
            "coordinates": [[
                [-14, 72],
                [-14, 34],
                [40, 34],
                [40, 72],
                [-14, 72]
              ]]}'
  europe_sf <- geojsonsf::geojson_sf(europe)
  
  # Extract datasets
  europe_pollen_data <- get_datasets(loc = europe_sf, all_data = T, 
                                     datasettype = 'pollen')
  # europe_charcoal_data <- get_datasets(loc = europe_sf, all_data = T,   # no data of interest here
  #                                         datasettype = 'charcoal')
  # europe_microcharcoal_data <- get_datasets(loc = europe_sf, all_data = T,   # no data of interest here
  #                                      datasettype = 'microcharcoal')
  # europe_macrocharcoal_data <- get_datasets(loc = europe_sf, all_data = T,   # no data of interest here
  #                                           datasettype = 'macrocharcoal')
  europe_macrofossil_data <- get_datasets(loc = europe_sf, all_data = T, 
                                          datasettype = 'plant macrofossil')
  
  
  # Download macrofossil dataset
  macrofossil_data <- get_downloads(europe_macrofossil_data, all_data = T)
  macrofossil_samples <- samples(macrofossil_data) %>% 
    dplyr::filter(ecologicalgroup %in% c("TRSH", "UPHE", "VACR")) # we keep only "Trees and Shrubs", "Upland Herbs" and "Terrest. Vasc. Cryptogams"
  
  # Download pollen dataset
  ## seem too heavy to download it in one single task (Internal Server Error)
  ## we thus need to split in several requests, run in parallel on 15 cores
  nsubsets <- 15
  len <- length(europe_pollen_data)
  split_ind <- split(1:len,
                     cut(1:len, nsubsets , labels = FALSE))
  plan(multisession, workers = nsubsets)
  system.time(pollen_samples <- future_lapply(1:nsubsets, function(i){
    print(i)
    subset_data <- europe_pollen_data
    subset_data@sites<- europe_pollen_data@sites[split_ind[[i]]]
    subset_data <- get_downloads(subset_data, all_data = T)  
    subset_samples <- samples(subset_data) %>% 
      dplyr::filter(ecologicalgroup %in% c("TRSH", "UPHE", "VACR")) # we keep only "Trees and Shrubs", "Upland Herbs" and "Terrest. Vasc. Cryptogams"
    # pollen_samples <- rbind(pollen_samples, subset_samples)
    return(subset_samples)
  }))
  plan(sequential)
  gc()
  pollen_samples <- do.call(rbind.data.frame, pollen_samples)
  
  saveRDS(pollen_samples, file.path(wd, "data", "pollen_samples.rds"))
  saveRDS(macrofossil_samples, file.path(wd, "data", "macrofossil_samples.rds"))
  
}else{
  
  pollen_samples <- readRDS(file.path(wd, "data", "pollen_samples.rds"))
  macrofossil_samples <- readRDS(file.path(wd, "data", "macrofossil_samples.rds"))
  
}