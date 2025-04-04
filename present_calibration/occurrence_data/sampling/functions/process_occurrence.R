

source(paste0(wd,"functions/read_process_GBIF.R"))

process_occurrence <- function(species, EUForest, GBIF, EVM, AFE, ERA5land, WOODIV=NULL){
  
  # GBIF data
  ## set-up filters
  EVM_filter <- EVM %>% dplyr::filter(!is.na(!!as.symbol(substr(paste0(word(species, 1), '_', word(species, 2)), 1, 10)))) %>% 
    dplyr::select(geometry) %>% 
    st_transform(crs=st_crs(4326))
  
  AFE$taxon_name <- word(AFE$taxon_name, 1,2)
  AFE_filter <- AFE %>% filter(taxon_name == species) %>% 
    dplyr::select(geometry = geom) %>% 
    st_transform(crs=st_crs(4326))
  
  ## filtering
  GBIF_filtered <- read_process_GBIF(GBIF, filter = rbind(AFE_filter, EVM_filter))
  rm(GBIF)
  
  # EU-Forest data
  EUForest_sf <- st_as_sf(EUForest, coords = c("X","Y"), crs=st_crs(3035))
  colnames(EUForest_sf)[2] <- "species"
  EUForest_sf <- EUForest_sf[EUForest_sf$species==species,] %>% 
    dplyr::select(species, geometry)
  EUForest_sf <- st_transform(EUForest_sf, crs = st_crs(4326))
 
  # WOODIV
  if(!is.null(WOODIV)){
    WOODIV <- WOODIV %>% dplyr::select(species, geometry) %>% st_transform(crs = st_crs(4326)) %>% st_centroid() %>% unique()
  }
  
  # Transform to grid
  GBIF_gridded <-  rasterize(GBIF_filtered, ERA5land, field=1, fun='count')
  GBIF_gridded[GBIF_gridded > 0] <- 1
  EUForest_gridded <- rasterize(EUForest_sf, ERA5land, field=1, fun='count')
  EUForest_gridded[EUForest_gridded > 0] <- 1
  
  # Find consensus
  if(is.null(WOODIV)){
    rs <- stack(GBIF_gridded, EUForest_gridded)
    cons <- calc(rs, sum, na.rm=T)
  }else{
    if(species== "Quercus ilex"){
      WOODIV <- WOODIV[WOODIV$species=='QILE',]
    }else if(species=="Quercus pubescens"){
      WOODIV <- WOODIV[WOODIV$species=='QPUB',]
    }else if(species=="Pinus pinaster"){
      WOODIV <- WOODIV[WOODIV$species=='PPIR',]
    }else if(species=="Pinus halepensis"){
      WOODIV <- WOODIV[WOODIV$species=='PHAL',]
    }else{
      stop("Error with Woodiv data. Are you sure the species name is correct ?")
    }
    WOODIV_gridded <- rasterize(WOODIV, ERA5land, field=1, fun='count')
    WOODIV_gridded[WOODIV_gridded>1] <- 1
    rs <- stack(GBIF_gridded, EUForest_gridded, WOODIV_gridded)
    cons <- calc(rs, sum, na.rm=T)
  }
  
  cons[cons == 0] <- NA
  names(cons) <- "nb_src"
  return(cons)
  
}