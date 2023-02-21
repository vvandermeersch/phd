  ##############################################
# Script to read and process occurrence data #
##############################################

# Author : V. Van der Meersch
# Last modified : 14/02/2022

######################################
### Loading packages and functions ----

library(sf)
library(dplyr)
library(raster)
library(data.table)
library(stringr)

wd_process <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/occurrence_data/process/"

source(paste0(wd_process,"functions/read_process_GBIF.R"))
source(paste0(wd_process,"functions/transform_to_grid.R"))
source(paste0(wd_process,"functions/find_consensus.R"))

### Settings ----

data_dir <- "D:/"

list_species <- c("Abies alba", "Betula pendula", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua",
                  "Picea abies", "Pinus sylvestris", "Quercus ilex", "Quercus pubescens")
list_species_abb <- c("abialb", "betpen", "fagsyl", "fraexc", "lardec",
                      "picabi", "pinsyl", "queile", "quepub")
list_species <- rbind(list_species, list_species_abb)

######################################
### EVM and AFE data ----

# EVM data
EVM <- read_sf(paste0(data_dir,"species/EuroVegMap/evm_species.shp"))


#AFE data
AFE <- read_sf(paste0(data_dir,"species/AFE/species/all.gpkg")) # WARNING : no data for Fraxinus !
AFE$taxon_name <- word(AFE$taxon_name, 1,2)


### EU-Forest data ----
EUForest_2017 <- read.csv(paste0(data_dir,"species/EUForest/EuForestspecies.csv"))
EUForest_sf <- st_as_sf(EUForest_2017, coords = c("X","Y"), crs=st_crs(3035))
colnames(EUForest_sf)[2] <- "species"
EUForest_sf <- st_transform(EUForest_sf, crs = st_crs(4326))


### GBIF data ----

# ABIES ALBA
GBIF_Abiesalba <- fread(paste0(data_dir,"species/GBIF/Abies_alba_18012022/0109922-210914110416597.csv"), sep="\t", dec=".", quote = "",
                        header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Abiesalba <- EVM %>% filter(!is.na("Abies_alba")) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326)) 
AFE_Abiesalba <- AFE %>% filter(taxon_name == "Abies alba") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326)) 
GBIF_abialb_sf <- read_process_GBIF(GBIF_Abiesalba, filter = rbind(AFE_Abiesalba, EVM_Abiesalba))
GBIF_abialb_sf_unfiltered <- read_process_GBIF(GBIF_Abiesalba)
rm(GBIF_Abiesalba)


# BETULA PENDULA 
GBIF_Betulapendula <- fread(paste0(data_dir,"species/GBIF/Betula_pendula_21012022/0112957-210914110416597.csv"), sep="\t", dec=".", quote = "",
                            header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Betulapendula <- EVM %>% filter(!is.na(Betula_pen)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326)) 
AFE_Betulapendula <- AFE %>% filter(taxon_name == "Betula pendula") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326)) 
GBIF_betpen_sf <- read_process_GBIF(GBIF_Betulapendula, filter = rbind(AFE_Betulapendula, EVM_Betulapendula))
GBIF_betpen_sf_unfiltered <- read_process_GBIF(GBIF_Betulapendula)
rm(GBIF_Betulapendula)


# FAGUS SYLVATICA 
GBIF_Fagussylvatica <- fread(paste0(data_dir,"species/GBIF/Fagus_sylvatica_20012022/0112486-210914110416597.csv"), sep="\t", dec=".", quote = "",
                             header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Fagussylvatica <- EVM %>% filter(!is.na(Fagus_sylv)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326))
AFE_Fagussylvatica <- AFE %>% filter(taxon_name == "Fagus sylvatica") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326))
GBIF_fagsyl_sf <- read_process_GBIF(GBIF_Fagussylvatica, filter = rbind(AFE_Fagussylvatica, EVM_Fagussylvatica))
GBIF_fagsyl_sf_unfiltered <- read_process_GBIF(GBIF_Fagussylvatica)
rm(GBIF_Fagussylvatica)


# FRAXINUS EXCELSIOR 
GBIF_Fraxinusexcelsior <- fread(paste0(data_dir,"species/GBIF/Fraxinus_excelsior_24012022/0114084-210914110416597.csv"), sep="\t", dec=".", quote = "",
                                header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Fraxinusexcelsior <- EVM %>% filter(!is.na(Fraxinus_e)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326))
# AFE_Fraxinusexcelsior <- AFE %>% filter(taxon_name == "Fraxinus excelsior") %>% 
#   dplyr::select(geometry = geom) %>% 
#   st_transform(crs=st_crs(4326))
GBIF_fraexc_sf <- read_process_GBIF(GBIF_Fraxinusexcelsior, filter = EVM_Fraxinusexcelsior)
GBIF_fraexc_sf_unfiltered <- read_process_GBIF(GBIF_Fraxinusexcelsior)
rm(GBIF_Fraxinusexcelsior)


# LARIX DECIDUA 
GBIF_Larixdecidua <- fread(paste0(data_dir,"species/GBIF/Larix_decidua_21012022/0112959-210914110416597.csv"), sep="\t", dec=".", quote = "",
                           header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Larixdecidua <- EVM %>% filter(!is.na(Larix_deci)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326))
AFE_Larixdecidua <- AFE %>% filter(taxon_name == "Larix decidua") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326))
GBIF_lardec_sf <- read_process_GBIF(GBIF_Larixdecidua, filter = rbind(AFE_Larixdecidua, EVM_Larixdecidua))
GBIF_lardec_sf_unfiltered <- read_process_GBIF(GBIF_Larixdecidua)
rm(GBIF_Larixdecidua)


# PICEA ABIES 
GBIF_Piceaabies <- fread(paste0(data_dir,"species/GBIF/Picea_abies_20012022/0112455-210914110416597.csv"), sep="\t", dec=".", quote = "",
                         header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Piceaabies <- EVM %>% filter(!is.na(Picea_abie)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326))
AFE_Piceaabies <- AFE %>% filter(taxon_name == "Picea abies") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326))
GBIF_picabi_sf <- read_process_GBIF(GBIF_Piceaabies, filter = rbind(AFE_Piceaabies, EVM_Piceaabies))
GBIF_picabi_sf_unfiltered <- read_process_GBIF(GBIF_Piceaabies)
rm(GBIF_Piceaabies)


# PINUS SYLVESTRIS 
GBIF_Pinussylvestris <- fread(paste0(data_dir,"species/GBIF/Pinus_sylvestris_21012022/0112955-210914110416597.csv"), sep="\t", dec=".", quote = "",
                              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Pinussylvestris <- EVM %>% filter(!is.na(Pinus_sylv)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326))
AFE_Pinussylvestris <- AFE %>% filter(taxon_name == "Pinus sylvestris") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326))
GBIF_pinsyl_sf <- read_process_GBIF(GBIF_Pinussylvestris, filter = rbind(AFE_Pinussylvestris, EVM_Pinussylvestris))
GBIF_pinsyl_sf_unfiltered <- read_process_GBIF(GBIF_Pinussylvestris)
rm(GBIF_Pinussylvestris)


# QUERCUS ILEX 
GBIF_Quercusilex <- fread(paste0(data_dir,"species/GBIF/Quercus_ilex_24012022/0114082-210914110416597.csv"), sep="\t", dec=".", quote = "",
                          header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Quercusilex <- EVM %>% filter(!is.na(Quercus_il)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326))
AFE_Quercusilex <- AFE %>% filter(taxon_name == "Quercus ilex") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326))
GBIF_queile_sf <- read_process_GBIF(GBIF_Quercusilex, filter = rbind(AFE_Quercusilex, EVM_Quercusilex))
GBIF_queile_sf_unfiltered <- read_process_GBIF(GBIF_Quercusilex)
rm(GBIF_Quercusilex)


# QUERCUS PUBESCENS 
GBIF_Quercuspubescens <- fread(paste0(data_dir,"species/GBIF/Quercus_pubescens_20012022/0112488-210914110416597.csv"), sep="\t", dec=".", quote = "",
                               header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Quercuspubescens <- EVM %>% filter(!is.na(Quercus_pu)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326))
AFE_Quercuspubescens <- AFE %>% filter(taxon_name == "Quercus pubescens") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326))
GBIF_quepub_sf <- read_process_GBIF(GBIF_Quercuspubescens, filter = rbind(AFE_Quercuspubescens, EVM_Quercuspubescens))
GBIF_quepub_sf_unfiltered <- read_process_GBIF(GBIF_Quercuspubescens)
rm(GBIF_Quercuspubescens)


# AGREGATION 
GBIF_sf <- rbind(GBIF_abialb_sf, GBIF_betpen_sf, GBIF_fagsyl_sf, GBIF_fraexc_sf,
                 GBIF_lardec_sf, GBIF_picabi_sf, GBIF_pinsyl_sf, GBIF_queile_sf,
                 GBIF_quepub_sf)
GBIF_sf <- st_transform(GBIF_sf, crs = st_crs(4326))

GBIF_sf_unfiltered <- rbind(GBIF_abialb_sf_unfiltered, GBIF_betpen_sf_unfiltered, GBIF_fagsyl_sf_unfiltered, GBIF_fraexc_sf_unfiltered,
                            GBIF_lardec_sf_unfiltered, GBIF_picabi_sf_unfiltered, GBIF_pinsyl_sf_unfiltered, GBIF_queile_sf_unfiltered,
                            GBIF_quepub_sf_unfiltered)
GBIF_sf_unfiltered <- st_transform(GBIF_sf_unfiltered, crs = st_crs(4326))

rm(GBIF_abialb_sf, GBIF_betpen_sf, GBIF_fagsyl_sf, GBIF_fraexc_sf,
   GBIF_lardec_sf, GBIF_picabi_sf, GBIF_pinsyl_sf, GBIF_queile_sf,
   GBIF_quepub_sf)

rm(GBIF_abialb_sf_unfiltered, GBIF_betpen_sf_unfiltered, GBIF_fagsyl_sf_unfiltered, GBIF_fraexc_sf_unfiltered,
   GBIF_lardec_sf_unfiltered, GBIF_picabi_sf_unfiltered, GBIF_pinsyl_sf_unfiltered, GBIF_queile_sf_unfiltered,
   GBIF_quepub_sf_unfiltered)





### WOODIV data ----
source("D:/species/WOODIV/WOODIV_DB_release_v1/OCCURRENCE/WOODIV_working_file_generation.R")
rm(working.file, working.file.Victor, WOODIV_Species_code, woodiv_grid, WOODIV_Occurrence_source, woodiv.grid.merge)

WOODIV_sf <- rbind(woodiv_queile, woodiv_quepub) %>% dplyr::select(source_code, species=spcode, geometry)

WOODIV_queile_ERA5 <- transform_to_grid(WOODIV_sf, ERA5land_grid, "QILE", "WOODIV")
WOODIV_quepub_ERA5 <- transform_to_grid(WOODIV_sf, ERA5land_grid, "QPUB", "WOODIV")
### Grid data ----

# Grille ERA5
ERA5land <- raster(paste0(data_dir,"climate/europe_ERA5.grib"))
ERA5land_grid <-  st_as_sf(rasterToPolygons(ERA5land))
ERA5land_grid <- st_transform(ERA5land_grid, crs = st_crs(4326))
ERA5land_grid[,3] <- c(1:nrow(ERA5land_grid))
colnames(ERA5land_grid) <- c("value", "geometry", "id_cell")
rm(ERA5land)
######################################
### Merge data (and find consensus) ----

# ERA5_land resolution

# For all except Q. ilex and Q. pubescens
list_species_red <- list_species[,which(!list_species[2,] %in% c("queile", "quepub"))]
for(i in 1:ncol(list_species_red)){
  gbif <- paste0("GBIF_", list_species_red[2,i],"_ERA5")
  assign(gbif, transform_to_grid(GBIF_sf, ERA5land_grid, list_species_red[1,i], "GBIF"))
  euforest <- paste0("EUForest_", list_species_red[2,i],"_ERA5")
  assign(euforest, transform_to_grid(EUForest_sf, ERA5land_grid, list_species_red[1,i], "EU-Forest"))
  eval(parse(text=paste0("consensus_", list_species_red[2,i],"_ERA5 <- find_consensus(rbind(", gbif, ", ", euforest,
                         "))  %>% st_crop(st_bbox(extent(EUForest_sf)))")))
}

# With Woodiv data for Q. ilex and Q. pubescens
list_species_red <- list_species[,which(list_species[2,] %in% c("queile", "quepub"))]
for(i in 1:ncol(list_species_red)){
  gbif <- paste0("GBIF_", list_species_red[2,i],"_ERA5")
  assign(gbif, transform_to_grid(GBIF_sf, ERA5land_grid, list_species_red[1,i], "GBIF"))
  euforest <- paste0("EUForest_", list_species_red[2,i],"_ERA5")
  assign(euforest, transform_to_grid(EUForest_sf, ERA5land_grid, list_species_red[1,i], "EU-Forest"))
  woodiv <- paste0("WOODIV_", list_species_red[2,i],"_ERA5")
  eval(parse(text=paste0("consensus_", list_species_red[2,i],"_ERA5 <- find_consensus(rbind(", gbif, ", ", euforest,
                         "))  %>% st_crop(st_bbox(extent(EUForest_sf)))")))
}



