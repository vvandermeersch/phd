# Function to extract climate for specific presence/absence cells

library(dplyr)
library(sf)
library(data.table)
library(raster)
library(tidyr)

source(paste0(wd, 'functions/extract_climate.R'))
source(paste0(wd, 'functions/save_climate.R'))

# example : load("D:/species/processed/consensus_abialb_ERA5.Rdata")
# wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/calibration/climate_data/extract/"
# out_folder <- paste0(pd_folder, "abies_alba_extraction/")

phenofit_extract_climate <- function(years, species, species_data, pd_folder, out_folder, method_pet){

  # transform species data
  # species_data <- st_centroid(species_data) %>%
  #   dplyr::mutate(lon = round(sf::st_coordinates(.)[,1],1),
  #                 lat = round(sf::st_coordinates(.)[,2],1)) %>%
  #   st_drop_geometry()
  
  species_data <- species_data %>% dplyr::select(-pres)
  
  for(yr in years){
    
    tmp_file <- paste0(pd_folder, "ERA5LAND_", "tmp", "_", yr, "_dly.fit")
    tmp <- fread(tmp_file, showProgress=F)
    colnames(tmp)[1:2] <- c("lat", "lon")
    tmp$lat <- round(tmp$lat, 1)
    tmp$lon <- round(tmp$lon, 1)
    tmp_ext <- extract_climate(species_data, tmp)
    save_climate(tmp_ext, yr, out_folder, p_var = "tmp", species)
    
    tmn_file <- paste0(pd_folder, "ERA5LAND_", "tmn", "_", yr, "_dly.fit")
    tmn <- fread(tmn_file, showProgress=F)
    colnames(tmn)[1:2] <- c("lat", "lon")
    tmn$lat <- round(tmn$lat, 1)
    tmn$lon <- round(tmn$lon, 1)
    tmn_ext <- extract_climate(species_data, tmn)
    save_climate(tmn_ext, yr, out_folder, p_var = "tmn", species)
    
    tmx_file <- paste0(pd_folder, "ERA5LAND_", "tmx", "_", yr, "_dly.fit")
    tmx <- fread(tmx_file, showProgress=F)
    colnames(tmx)[1:2] <- c("lat", "lon")
    tmx$lat <- round(tmx$lat, 1)
    tmx$lon <- round(tmx$lon, 1)
    tmx_ext <- extract_climate(species_data, tmx)
    save_climate(tmx_ext, yr, out_folder, p_var = "tmx", species)
    
    rh_file <- paste0(pd_folder, "ERA5LAND_", "RH", "_", yr, "_dly.fit")
    rh <- fread(rh_file, showProgress=F)
    colnames(rh)[1:2] <- c("lat", "lon")
    rh$lat <- round(rh$lat, 1)
    rh$lon <- round(rh$lon, 1)
    rh_ext <- extract_climate(species_data, rh)
    save_climate(rh_ext, yr, out_folder, p_var = "RH", species)
    
    glo_file <- paste0(pd_folder, "ERA5LAND_", "glo", "_", yr, "_dly.fit")
    glo <- fread(glo_file, showProgress=F)
    colnames(glo)[1:2] <- c("lat", "lon")
    glo$lat <- round(glo$lat, 1)
    glo$lon <- round(glo$lon, 1)
    glo_ext <- extract_climate(species_data, glo)
    save_climate(glo_ext, yr, out_folder, p_var = "glo", species)
    
    pet_file <- paste0(pd_folder,"pet_",method_pet,"/", "ERA5LAND_", "pet", "_", yr, "_dly.fit")
    pet <- fread(pet_file, showProgress=F)
    colnames(pet)[1:2] <- c("lat", "lon")
    pet$lat <- round(pet$lat, 1)
    pet$lon <- round(pet$lon, 1)
    # NA and negative values set to zero
    cols <- c(3:ncol(pet))
    setnafill(pet, cols=cols, fill=0)
    pet[ , (cols) := lapply(.SD, pmax, 0), .SDcols = cols]
    
    pet_ext <- extract_climate(species_data, pet)
    save_climate(pet_ext, yr, out_folder, p_var = "pet", species, method=method_pet)
    
    pre_file <- paste0(pd_folder, "ERA5LAND_", "pre", "_", yr, "_dly.fit")
    pre <- fread(pre_file, showProgress=F)
    colnames(pre)[1:2] <- c("lat", "lon")
    pre$lat <- round(pre$lat, 1)
    pre$lon <- round(pre$lon, 1)
    pre_ext <- extract_climate(species_data, pre)
    save_climate(pre_ext, yr, out_folder, p_var = "pre", species)
    
    wnd_file <- paste0(pd_folder, "ERA5LAND_", "wnd", "_", yr, "_dly.fit")
    wnd <- fread(wnd_file, showProgress=F)
    colnames(wnd)[1:2] <- c("lat", "lon")
    wnd$lat <- round(wnd$lat, 1)
    wnd$lon <- round(wnd$lon, 1)
    wnd_ext <- extract_climate(species_data, wnd)
    save_climate(wnd_ext, yr, out_folder, p_var = "wnd", species)

  }
  
  # Altitude
  alt_file <- paste0(pd_folder, "ERA5LAND_Altitude.fit")
  alt <- fread(alt_file, showProgress=F)
  colnames(alt)[1:2] <- c("lat", "lon")
  alt$lat <- round(alt$lat, 1)
  alt$lon <- round(alt$lon, 1)
  alt_ext <- extract_climate(species_data, alt)
  save_climate(alt_ext, yr, out_folder, p_var = "Altitude", species)
  
  # WHC
  whc_file <- paste0(pd_folder, "ERA5LAND_WHC.fit")
  whc <- fread(whc_file, showProgress=F)
  colnames(whc)[1:2] <- c("lat", "lon")
  whc$lat <- round(whc$lat, 1)
  whc$lon <- round(whc$lon, 1)
  whc_ext <- extract_climate(species_data, whc)
  save_climate(whc_ext, yr, out_folder, p_var = "WHC", species)
  
}



phenofit_extract_all_climate <- function(years, pd_folder, out_folder, method_pet, filter = NULL){
  
  for(yr in years){
    
    tmp_file <- paste0(pd_folder, "ERA5LAND_", "tmp", "_", yr, "_dly.fit")
    tmp <- fread(tmp_file, showProgress=F)
    colnames(tmp)[1:2] <- c("lat", "lon")
    tmp_ext <- extract_all_climate(tmp, filter)
    save_climate(tmp_ext, yr, out_folder, p_var = "tmp", species = "all Europe")
    
    tmn_file <- paste0(pd_folder, "ERA5LAND_", "tmn", "_", yr, "_dly.fit")
    tmn <- fread(tmn_file, showProgress=F)
    colnames(tmn)[1:2] <- c("lat", "lon")
    tmn_ext <- extract_all_climate(tmn, filter)
    save_climate(tmn_ext, yr, out_folder, p_var = "tmn", species = "all Europe")
    
    tmx_file <- paste0(pd_folder, "ERA5LAND_", "tmx", "_", yr, "_dly.fit")
    tmx <- fread(tmx_file, showProgress=F)
    colnames(tmx)[1:2] <- c("lat", "lon")
    tmx_ext <- extract_all_climate(tmx, filter)
    save_climate(tmx_ext, yr, out_folder, p_var = "tmx", species = "all Europe")
    
    rh_file <- paste0(pd_folder, "ERA5LAND_", "RH", "_", yr, "_dly.fit")
    rh <- fread(rh_file, showProgress=F)
    colnames(rh)[1:2] <- c("lat", "lon")
    rh_ext <- extract_all_climate(rh, filter)
    save_climate(rh_ext, yr, out_folder, p_var = "RH", species = "all Europe")
    
    glo_file <- paste0(pd_folder, "ERA5LAND_", "glo", "_", yr, "_dly.fit")
    glo <- fread(glo_file, showProgress=F)
    colnames(glo)[1:2] <- c("lat", "lon")
    glo_ext <- extract_all_climate(glo, filter)
    save_climate(glo_ext, yr, out_folder, p_var = "glo", species = "all Europe")
    
    pet_file <- paste0(pd_folder,"pet_",method_pet,"/", "ERA5LAND_", "pet", "_", yr, "_dly.fit")
    pet <- fread(pet_file, showProgress=F)
    colnames(pet)[1:2] <- c("lat", "lon")
    # NA and negative values set to zero
    cols <- c(3:ncol(pet))
    setnafill(pet, cols=cols, fill=0)
    pet[ , (cols) := lapply(.SD, pmax, 0), .SDcols = cols]
    
    pet_ext <- extract_all_climate(pet, filter)
    save_climate(pet_ext, yr, out_folder, p_var = "pet", species = "all Europe", method=method_pet)
    
    pre_file <- paste0(pd_folder, "ERA5LAND_", "pre", "_", yr, "_dly.fit")
    pre <- fread(pre_file, showProgress=F)
    colnames(pre)[1:2] <- c("lat", "lon")
    pre_ext <- extract_all_climate(pre, filter)
    save_climate(pre_ext, yr, out_folder, p_var = "pre", species = "all Europe")
    
    wnd_file <- paste0(pd_folder, "ERA5LAND_", "wnd", "_", yr, "_dly.fit")
    wnd <- fread(wnd_file, showProgress=F)
    colnames(wnd)[1:2] <- c("lat", "lon")
    wnd_ext <- extract_all_climate(wnd, filter)
    save_climate(wnd_ext, yr, out_folder, p_var = "wnd", species = "all Europe")
    
  }
  
  # Altitude
  alt_file <- paste0(pd_folder, "ERA5LAND_Altitude.fit")
  alt <- fread(alt_file, showProgress=F)
  colnames(alt)[1:2] <- c("lat", "lon")
  alt_ext <- extract_all_climate(alt, filter)
  save_climate(alt_ext, yr, out_folder, p_var = "Altitude", species = "all Europe")
  
  # WHC
  whc_file <- paste0(pd_folder, "ERA5LAND_WHC.fit")
  whc <- fread(whc_file, showProgress=F)
  colnames(whc)[1:2] <- c("lat", "lon")
  whc_ext <- extract_all_climate(whc, filter)
  save_climate(whc_ext, yr, out_folder, p_var = "WHC", species = "all Europe")
  
  gc()
}