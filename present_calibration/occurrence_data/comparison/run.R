##### Packages #####

library(ggplot2)
library(ggnewscale)
library(cowplot)
library(sf)
library(dplyr)
library(rnaturalearth)
library(ggpubr)
library(raster)
library(extrafont)
library(data.table)
library(stringr)
loadfonts(device = "win")

##### CONFIG #####

source("functions/read_process_GBIF.R")
source("functions/transform_to_grid.R")
source("functions/find_consensus.R")
source("functions/create_consensus_map.R")
source("functions/calculate_grid_density.R")

data_dir <- "D:/"

list_species <- c("Abies alba", "Betula pendula", "Fagus sylvatica", "Fraxinus excelsior", "Larix decidua",
                  "Picea abies", "Pinus sylvestris", "Quercus ilex", "Quercus pubescens")
list_species_abb <- c("abialb", "betpen", "fagsyl", "fraexc", "lardec",
                      "picabi", "pinsyl", "queile", "quepub")
list_species_ws <- c("Abiesalba", "Betulapendula", "Fagussylvatica", "Fraxinusexcelsior", "Larixdecidua",
                  "Piceaabies", "Pinussylvestris", "Quercusilex", "Quercuspubescens")
list_species <- rbind(list_species, list_species_abb, list_species_ws)

##### GRID data ######

# INSPIRE 1km Grid
eurostat_grid_1km <- read_sf(paste0(data_dir,"grid/eurostat/grid_1km_surf.gpkg")) %>% dplyr::select(GRD_ID, geometry)
colnames(eurostat_grid_1km) <- c("id_cell", "geometry")

# EUForest data
EUForest_2017 <- read.csv(paste0(data_dir,"species/EUForest/EuForestspecies.csv"))

# EVM data
EVM <- read_sf(paste0(data_dir,"species/EuroVegMap/evm_species.shp"))

#AFE data
AFE <- read_sf(paste0(data_dir,"species/AFE/species/all.gpkg")) # WARNING : no data for Fraxinus !
AFE$taxon_name <- word(AFE$taxon_name, 1,2)

# Grille ERA5
ERA5land <- raster(paste0(data_dir,"climate/europe_ERA5.grib"))
ERA5land_grid <-  st_as_sf(rasterToPolygons(ERA5land))
ERA5land_grid <- st_transform(ERA5land_grid, crs = st_crs(eurostat_grid_1km))
ERA5land_grid[,3] <- c(1:nrow(ERA5land_grid))
colnames(ERA5land_grid) <- c("value", "geometry", "id_cell")
rm(ERA5land)

# Grille 10km
EEA_grid_10km <- read_sf(paste0(data_dir,"grid/EEA_reference_grids/europe_10km.shp"))
EEA_grid_10km  <- st_transform(EEA_grid_10km, crs = st_crs(eurostat_grid_1km))

##### JRC data ######
# JRC_Abiesalba_plg <- read_sf(paste0(data_dir,"species/JRC/Abies_alba_shp/", layer="Abies_alba_plg.shp")) %>%
#   st_transform(crs = st_crs(eurostat_grid_1km))
# JRC_Abiesalba_plg_grid_1km <- eurostat_grid_1km[unique(unlist(st_intersects(JRC_Abiesalba_plg, eurostat_grid_1km))),]
# JRC_Abiesalba_pnt <- read_sf(paste0(data_dir,"species/JRC/Abies_alba_shp/", layer="Abies_alba_pnt.shp")) %>%
#   st_transform(crs = st_crs(eurostat_grid_1km))
# JRC_Abiesalba_pnt_grid_1km <- eurostat_grid_1km[unique(unlist(st_intersects(JRC_Abiesalba_pnt, eurostat_grid_1km))),]
# JRC_Abiesalba_syn_plg <- read_sf(paste0(data_dir,"species/JRC/Abies_alba_shp/", layer="Abies_alba_syn_plg.shp")) %>%
#   st_transform(crs = st_crs(eurostat_grid_1km))
# JRC_Abiesalba_syn_plg_grid_1km <- eurostat_grid_1km[unique(unlist(st_intersects(JRC_Abiesalba_syn_plg, eurostat_grid_1km))),]
# JRC_Abiesalba_syn_pnt <- read_sf(paste0(data_dir,"species/JRC/Abies_alba_shp/", layer="Abies_alba_syn_pnt.shp")) %>%
#   st_transform(crs = st_crs(eurostat_grid_1km))
# JRC_Abiesalba_syn_pnt_grid_1km <- eurostat_grid_1km[unique(unlist(st_intersects(JRC_Abiesalba_syn_pnt, eurostat_grid_1km))),]
# 
# JRC_Abiesalba_grid_1km <- st_union(JRC_Abiesalba_plg_grid_1km, JRC_Abiesalba_pnt_grid_1km, 
#                                    JRC_Abiesalba_syn_plg_grid_1km, JRC_Abiesalba_syn_pnt_grid_1km)

##### EU-Forest data ######

EUForest_sf <- st_as_sf(EUForest_2017, coords = c("X","Y"), crs=st_crs(3035))
EUForest_sf <- st_transform(EUForest_sf, crs = st_crs(eurostat_grid_1km))
colnames(EUForest_sf)[2] <- "species"


##### WORLD MAP ######

countries_EUForest <- as.character(unique(EUForest_sf$COUNTRY))
countries_EUForest[7] <- "Czech Rep."
countries_EUForest[22] <- "Poland"
other_countries <- c("Ukraine", "Bosnia and Herzegovina", "Serbia", "Macedonia", "Greece",
                     "Kosovo", "Albania", "Montenegro")
countries <- c(countries_EUForest, other_countries)


world_map <- ne_countries(scale="medium",returnclass = 'sf')
eu_map <- world_map %>% filter(name %in% countries)
eu_map_cropped <- eu_map %>% 
  st_crop(st_bbox(c(xmin = -12, xmax = 40, ymax = 71, ymin = 34), crs = st_crs(4326))) %>% 
  st_transform(crs = st_crs(eurostat_grid_1km))


##### GBIF data ######

  ## ABIES ALBA ----
GBIF_Abiesalba <- fread(paste0(data_dir,"species/GBIF/Abies_alba_18012022/0109922-210914110416597.csv"), sep="\t", dec=".", quote = "",
                             header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
EVM_Abiesalba <- EVM %>% filter(!is.na(Abies_alba)) %>% 
  dplyr::select(geometry) %>% 
  st_transform(crs=st_crs(4326)) 
AFE_Abiesalba <- AFE %>% filter(taxon_name == "Abies alba") %>% 
  dplyr::select(geometry = geom) %>% 
  st_transform(crs=st_crs(4326)) 
GBIF_abialb_sf <- read_process_GBIF(GBIF_Abiesalba, filter = rbind(AFE_Abiesalba, EVM_Abiesalba))
GBIF_abialb_sf_unfiltered <- read_process_GBIF(GBIF_Abiesalba)
rm(GBIF_Abiesalba)


  # BETULA PENDULA ----
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


  # FAGUS SYLVATICA ----
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


  # FRAXINUS EXCELSIOR ----
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


  # LARIX DECIDUA ----
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


  # PICEA ABIES ----
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


  # PINUS SYLVESTRIS ----
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


  # QUERCUS ILEX ----
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


  # QUERCUS PUBESCENS ----
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


  # AGREGATION ----
GBIF_sf <- rbind(GBIF_abialb_sf, GBIF_betpen_sf, GBIF_fagsyl_sf, GBIF_fraexc_sf,
                 GBIF_lardec_sf, GBIF_picabi_sf, GBIF_pinsyl_sf, GBIF_queile_sf,
                 GBIF_quepub_sf)
GBIF_sf <- st_transform(GBIF_sf, crs = st_crs(eurostat_grid_1km))

GBIF_sf_unfiltered <- rbind(GBIF_abialb_sf_unfiltered, GBIF_betpen_sf_unfiltered, GBIF_fagsyl_sf_unfiltered, GBIF_fraexc_sf_unfiltered,
                 GBIF_lardec_sf_unfiltered, GBIF_picabi_sf_unfiltered, GBIF_pinsyl_sf_unfiltered, GBIF_queile_sf_unfiltered,
                 GBIF_quepub_sf_unfiltered)
GBIF_sf_unfiltered <- st_transform(GBIF_sf_unfiltered, crs = st_crs(eurostat_grid_1km))

# GBIF_Abiesalba_grid_1km <- eurostat_grid_1km[unique(unlist(st_intersects(GBIF_Abiesalba_sf, eurostat_grid_1km))),]
# GBIF_Abiesalba_grid_10km <- EEA_grid_10km[unique(unlist(st_intersects(GBIF_Abiesalba_sf, EEA_grid_10km))),]
rm(GBIF_abialb_sf, GBIF_betpen_sf, GBIF_fagsyl_sf, GBIF_fraexc_sf,
   GBIF_lardec_sf, GBIF_picabi_sf, GBIF_pinsyl_sf, GBIF_queile_sf,
   GBIF_quepub_sf)

rm(GBIF_abialb_sf_unfiltered, GBIF_betpen_sf_unfiltered, GBIF_fagsyl_sf_unfiltered, GBIF_fraexc_sf_unfiltered,
  GBIF_lardec_sf_unfiltered, GBIF_picabi_sf_unfiltered, GBIF_pinsyl_sf_unfiltered, GBIF_queile_sf_unfiltered,
  GBIF_quepub_sf_unfiltered)


###### CONSENSUS ######

# ERA5_land resolution
for(i in 1:ncol(list_species)){
  gbif <- paste0("GBIF_", list_species[2,i],"_grid_ERA5")
  assign(gbif, transform_to_grid(GBIF_sf, ERA5land_grid, list_species[1,i], "GBIF"))
  euforest <- paste0("EUForest_", list_species[2,i],"_grid_ERA5")
  assign(euforest, transform_to_grid(EUForest_sf, ERA5land_grid, list_species[1,i], "EU-Forest"))
  eval(parse(text=paste0("consensus_", list_species[2,i],"_ERA5 <- find_consensus(", gbif, ", ", euforest,
                         ")  %>% st_crop(st_bbox(extent(eu_map_cropped)))")))
}

# ERA5_land resolution, unfiltered
for(i in 1:ncol(list_species)){
  gbif <- paste0("GBIF_", list_species[2,i],"_grid_ERA5")
  assign(gbif, transform_to_grid(GBIF_sf_unfiltered, ERA5land_grid, list_species[1,i], "GBIF"))
  euforest <- paste0("EUForest_", list_species[2,i],"_grid_ERA5")
  assign(euforest, transform_to_grid(EUForest_sf, ERA5land_grid, list_species[1,i], "EU-Forest"))
  eval(parse(text=paste0("consensus_", list_species[2,i],"_unfiltered_ERA5 <- find_consensus(", gbif, ", ", euforest,
                         ")  %>% st_crop(st_bbox(extent(eu_map_cropped)))")))
}

# 1km resolution
for(i in 1:ncol(list_species)){
  gbif <- paste0("GBIF_", list_species[2,i],"_grid_1km")
  assign(gbif, transform_to_grid(GBIF_sf, eurostat_grid_1km, list_species[1,i], "GBIF"))
  euforest <- paste0("EUForest_", list_species[2,i],"_grid_1km")
  assign(euforest, transform_to_grid(EUForest_sf, eurostat_grid_1km, list_species[1,i], "EU-Forest"))
  eval(parse(text=paste0("consensus_", list_species[2,i],"_1km <- find_consensus(", gbif, ", ", euforest,
                         ")  %>% st_crop(st_bbox(extent(eu_map_cropped)))")))
}

# 1km resolution, unfiltered
for(i in 1:ncol(list_species)){
  gbif <- paste0("GBIF_", list_species[2,i],"_grid_1km")
  assign(gbif, transform_to_grid(GBIF_sf_unfiltered, eurostat_grid_1km, list_species[1,i], "GBIF"))
  euforest <- paste0("EUForest_", list_species[2,i],"_grid_1km")
  assign(euforest, transform_to_grid(EUForest_sf, eurostat_grid_1km, list_species[1,i], "EU-Forest"))
  eval(parse(text=paste0("consensus_", list_species[2,i],"_unfiltered_1km <- find_consensus(", gbif, ", ", euforest,
                         ")  %>% st_crop(st_bbox(extent(eu_map_cropped)))")))
}


##### DENSITY #####

# from 1km res to ERA5-Land res

max_n_occ <- 0
for(i in 1:ncol(list_species)){
  consensus <- paste0("consensus_", list_species[2,i],"_1km")
  density <- paste0("consensus_", list_species[2,i],"_density_ERA5")
  eval(parse(text=paste0(density, " <- calculate_grid_density(", 
                         consensus, ", ERA5land_grid)  %>% st_crop(st_bbox(extent(eu_map_cropped)))")))
  eval(parse(text=paste0("max_temp <- max(", density, "$n_occ)")))
  print(max_temp)
  if(max_temp > max_n_occ){max_n_occ <- max_temp}
}



max_n_occ_ctd <- 0
for(i in 1:ncol(list_species)){
  consensus <- paste0("consensus_", list_species[2,i],"_1km")
  
  eval(parse(text=paste0("consensus_ctd <- st_centroid(", consensus, ")")))
  density <- paste0("consensus_", list_species[2,i],"_dens_ctd_ERA5")
  eval(parse(text=paste0(density,
                         " <- calculate_grid_density(consensus_ctd, ERA5land_grid)  %>% st_crop(st_bbox(extent(eu_map_cropped)))")))
  eval(parse(text=paste0("max_temp <- max(", density, "$n_occ)")))
  print(max_temp)
  if(max_temp > max_n_occ_ctd){max_n_occ_ctd <- max_temp}
}



#####
# consensus_map_1km <- ggplot() +
#   geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
#   geom_sf(data=consensus_1km , aes(fill=factor(n)), color=NA) + 
#   scale_fill_manual(values=c("#2d6a4f", "#1b4332")) + 
#   theme_void()
# 
# consensus_map_10km <- ggplot() +
#   geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
#   geom_sf(data=consensus_10km , aes(fill=factor(n)), color=NA) + 
#   scale_fill_manual(name = "Number of data sources", values=c("#52b788", "#2d6a4f")) + 
#   theme_void()  
# 
# consensus_map_10km_sources <- ggplot() +
#   geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
#   geom_sf(data=consensus_10km , aes(fill=origin_of_data), color=NA) + 
#   scale_fill_manual(name = "Source(s) of data", values=c("#9b9b7a", "#e4b074", "#997b66")) + 
#   theme_void() 
##### PLOTS #####

# ERA 5 plots
for(i in 1:ncol(list_species)){
  plot <- paste0("plot_", list_species[2,i],"_ERA5")
  consensus <- paste0("consensus_", list_species[2,i],"_ERA5")
  eval(parse(text=paste0(plot, " <- create_consensus_map(", consensus, ", '", list_species[1,i],
                         "', eu_map_cropped, type='n_sources')")))
}

for(i in 1:ncol(list_species)){
  plot <- paste0("plot_", list_species[2,i],"_ERA5_s")
  consensus <- paste0("consensus_", list_species[2,i],"_ERA5")
  eval(parse(text=paste0(plot, " <- create_consensus_map(", consensus, ", '", list_species[1,i],
                         "', eu_map_cropped, type='sources')")))
}

# ERA 5 plots, unfiltered
for(i in 1:ncol(list_species)){
  plot <- paste0("plot_", list_species[2,i],"_unfiltered_ERA5")
  consensus <- paste0("consensus_", list_species[2,i],"_unfiltered_ERA5")
  eval(parse(text=paste0(plot, " <- create_consensus_map(", consensus, ", '", list_species[1,i],
                         "', eu_map_cropped, type='n_sources')")))
}

for(i in 1:ncol(list_species)){
  plot <- paste0("plot_", list_species[2,i],"_unfiltered_ERA5_s")
  consensus <- paste0("consensus_", list_species[2,i],"_unfiltered_ERA5")
  eval(parse(text=paste0(plot, " <- create_consensus_map(", consensus, ", '", list_species[1,i],
                         "', eu_map_cropped, type='sources', annotate=TRUE)")))
}

# Density plots
for(i in 1:ncol(list_species)){
  plot <- paste0("plot_", list_species[2,i],"_ERA5_d")
  consensus <- paste0("consensus_", list_species[2,i],"_density_ERA5")
  eval(parse(text=paste0(plot, " <- create_consensus_map(", consensus, ", '", list_species[1,i],
                         "', eu_map_cropped, type='density', max = max_n_occ)")))
}

for(i in 1:ncol(list_species)){
  plot <- paste0("plot_", list_species[2,i],"_ERA5_dctd")
  consensus <- paste0("consensus_", list_species[2,i],"_dens_ctd_ERA5")
  eval(parse(text=paste0(plot, " <- create_consensus_map(", consensus, ", '", list_species[1,i],
                         "', eu_map_cropped, type='density', max = max_n_occ_ctd)")))
}

# Filter plots
AFE_Fraxinusexcelsior <- NULL
for(i in 1:ncol(list_species)){
  plot <- paste0("plot_filter_", list_species[2,i])
  filter <- paste0("rbind(AFE_", list_species[3,i], ", EVM_", list_species[3,i],")")
  eval(parse(text=paste0(plot, " <- create_consensus_map(", filter, ", '", NULL,
                         "', eu_map_cropped, type='filter')")))
}


##### MAPS #####

# Combined ----
combined <- plot_grid(
  plot_grid(plot_abialb_ERA5_s + theme(legend.position = "none"),
            plot_betpen_ERA5_s + theme(legend.position = "none"), 
            plot_fagsyl_ERA5_s + theme(legend.position = "none"), 
            plot_fraexc_ERA5_s + theme(legend.position = "none"), 
            plot_lardec_ERA5_s + theme(legend.position = "none"), 
            plot_picabi_ERA5_s + theme(legend.position = "none"), 
            plot_pinsyl_ERA5_s + theme(legend.position = "none"), 
            plot_queile_ERA5_s + theme(legend.position = "none"), 
            plot_quepub_ERA5_s + theme(legend.position = "none"),
            get_legend(plot_abialb_ERA5_s),
            ncol=1,
            rel_heights = c(7,7,7,7,7,7,7,7,7,3)),
  
  plot_grid(plot_abialb_ERA5 + theme(legend.position = "none"),
            plot_betpen_ERA5 + theme(legend.position = "none"), 
            plot_fagsyl_ERA5 + theme(legend.position = "none"), 
            plot_fraexc_ERA5 + theme(legend.position = "none"), 
            plot_lardec_ERA5 + theme(legend.position = "none"), 
            plot_picabi_ERA5 + theme(legend.position = "none"), 
            plot_pinsyl_ERA5 + theme(legend.position = "none"), 
            plot_queile_ERA5 + theme(legend.position = "none"), 
            plot_quepub_ERA5 + theme(legend.position = "none"),
            get_legend(plot_abialb_ERA5),
            ncol=1,
            rel_heights = c(7,7,7,7,7,7,7,7,7,3)),
  
  # plot_grid(plot_abialb_ERA5_d + theme(legend.position = "none"),
  #           plot_betpen_ERA5_d + theme(legend.position = "none"), 
  #           plot_fagsyl_ERA5_d + theme(legend.position = "none"), 
  #           plot_fraexc_ERA5_d + theme(legend.position = "none"), 
  #           plot_lardec_ERA5_d + theme(legend.position = "none"), 
  #           plot_picabi_ERA5_d + theme(legend.position = "none"), 
  #           plot_pinsyl_ERA5_d + theme(legend.position = "none"), 
  #           plot_queile_ERA5_d + theme(legend.position = "none"), 
  #           plot_quepub_ERA5_d + theme(legend.position = "none"),
  #           get_legend(plot_abialb_ERA5_d),
  #           ncol=1,
  #           rel_heights = c(7,7,7,7,7,7,7,7,7,3)),
  
  plot_grid(plot_abialb_ERA5_dctd + theme(legend.position = "none"),
            plot_betpen_ERA5_dctd + theme(legend.position = "none"), 
            plot_fagsyl_ERA5_dctd + theme(legend.position = "none"), 
            plot_fraexc_ERA5_dctd + theme(legend.position = "none"), 
            plot_lardec_ERA5_dctd + theme(legend.position = "none"), 
            plot_picabi_ERA5_dctd + theme(legend.position = "none"), 
            plot_pinsyl_ERA5_dctd + theme(legend.position = "none"), 
            plot_queile_ERA5_dctd + theme(legend.position = "none"), 
            plot_quepub_ERA5_dctd + theme(legend.position = "none"),
            get_legend(plot_abialb_ERA5_dctd),
            ncol=1,
            rel_heights = c(7,7,7,7,7,7,7,7,7,3)),
  
  ncol=4, rel_widths = c(1.2,1,1))

# Filtering comparison ----

comparison <- plot_grid(
  plot_grid(plot_abialb_unfiltered_ERA5_s + theme(legend.position = "none"),
            plot_betpen_unfiltered_ERA5_s + theme(legend.position = "none"), 
            plot_fagsyl_unfiltered_ERA5_s + theme(legend.position = "none"), 
            plot_fraexc_unfiltered_ERA5_s + theme(legend.position = "none"), 
            plot_lardec_unfiltered_ERA5_s + theme(legend.position = "none"), 
            plot_picabi_unfiltered_ERA5_s + theme(legend.position = "none"), 
            plot_pinsyl_unfiltered_ERA5_s + theme(legend.position = "none"), 
            plot_queile_unfiltered_ERA5_s + theme(legend.position = "none"), 
            plot_quepub_unfiltered_ERA5_s + theme(legend.position = "none"),
            get_legend(plot_abialb_ERA5_s),
            ncol=1,
            rel_heights = c(7,7,7,7,7,7,7,7,7,3)),
  
  plot_grid(plot_filter_abialb + theme(legend.position = "none"),
            plot_filter_betpen + theme(legend.position = "none"), 
            plot_filter_fagsyl + theme(legend.position = "none"), 
            plot_filter_fraexc + theme(legend.position = "none"), 
            plot_filter_lardec + theme(legend.position = "none"), 
            plot_filter_picabi + theme(legend.position = "none"), 
            plot_filter_pinsyl + theme(legend.position = "none"), 
            plot_filter_queile + theme(legend.position = "none"), 
            plot_filter_quepub + theme(legend.position = "none"),
            get_legend(ggplot()),
            ncol=1,
            rel_heights = c(7,7,7,7,7,7,7,7,7,3)),
  
  plot_grid(plot_abialb_ERA5_s + theme(legend.position = "none"),
            plot_betpen_ERA5_s + theme(legend.position = "none"), 
            plot_fagsyl_ERA5_s + theme(legend.position = "none"), 
            plot_fraexc_ERA5_s + theme(legend.position = "none"), 
            plot_lardec_ERA5_s + theme(legend.position = "none"), 
            plot_picabi_ERA5_s + theme(legend.position = "none"), 
            plot_pinsyl_ERA5_s + theme(legend.position = "none"), 
            plot_queile_ERA5_s + theme(legend.position = "none"), 
            plot_quepub_ERA5_s + theme(legend.position = "none"),
            get_legend(ggplot()),
            ncol=1,
            rel_heights = c(7,7,7,7,7,7,7,7,7,3)),
 
  ncol=3, rel_widths = c(1.2,1,1))





# Pyrenees ----

limits <- c(xmin = -1.05 , xmax = 0.05, ymax = 43.15 , ymin = 42.75 )
pyrenees <- eu_map %>% st_crop(st_bbox(limits, crs = st_crs(4326)))

eurostat_grid_pyrenees <- st_transform(eurostat_grid_1km, crs = st_crs(4326)) %>% st_crop(st_bbox(limits, crs = st_crs(4326)))

GBIF_pyrenees <- st_transform(GBIF_Abiesalba_sf, crs = st_crs(4326)) %>% st_crop(st_bbox(limits, crs = st_crs(4326)))
GBIF_grid_pyrenees <- st_transform(GBIF_Abiesalba_grid_1km, crs = st_crs(4326)) %>% st_crop(st_bbox(limits, crs = st_crs(4326)))

EUFor_pyrenees <- st_transform(EUForest_Abiesalba_sf, crs = st_crs(4326)) %>% st_crop(st_bbox(limits, crs = st_crs(4326)))
EUFor_grid_pyrenees <- st_transform(EUForest_Abiesalba_grid_1km, crs = st_crs(4326)) %>% st_crop(st_bbox(limits, crs = st_crs(4326)))

Duputie_pyrenees <- st_transform(Duputie_Abiesalba_sf, crs = st_crs(4326)) %>% st_crop(st_bbox(limits, crs = st_crs(4326)))

rm(eurostat_grid_1km)


p_pyrenees <- ggplot() + 
  geom_sf(data = pyrenees, fill = 'orange', alpha=0.1) +
  geom_sf(data=ERA5_land_ex_grid, color="black", fill = NA) +
  geom_sf(data = eurostat_grid_pyrenees, color="grey", fill = NA) +
  # geom_sf(data = JRC_Abiesalba_plg_grid_1km_pyrenees, fill = 'orange', alpha=0.8) +
  geom_sf(data = GBIF_grid_pyrenees, fill = "darkgreen", alpha=0.2) +
  geom_sf(data = EUFor_grid_pyrenees, fill = "red", alpha=0.2) +
  geom_sf(data = GBIF_pyrenees, color = "darkgreen") +
  geom_sf(data = EUFor_pyrenees, color = "red") +
  theme_void() +
  labs(title="Abies alba records in the Pyrenees, red=EUForest, green=GBIF")

GBIF_grid_pyrenees$origin <- "GBIF"
EUFor_grid_pyrenees$origin <- "EUForest"
#consensus <- base::unique(st_as_sf(c(GBIF_grid_pyrenees$geometry, EUFor_grid_pyrenees$geometry)))



consensus <- unique(rbind(GBIF_grid_pyrenees, EUFor_grid_pyrenees)) %>% 
  group_by(GRD_ID) %>% 
  mutate(origin_of_data = paste0(origin, collapse = ", "), n= n()) %>%
  dplyr::select(origin_of_data, n)


ERA5_land_ex_grid <- st_transform(ERA5_land_ex_grid, crs = st_crs(4326))
ERA5_land_ex_grid[,3] <- c(1:nrow(ERA5_land_ex_grid))
colnames(ERA5_land_ex_grid) <- c("value", "geometry", "id_cell")
intersection <- st_intersection(x = ERA5_land_ex_grid, y = consensus)
int_result <- intersection %>% 
  group_by(id_cell) %>% 
  count()
int_result <- as.data.frame(int_result)[,-3]
ERA5_land_density <- sp::merge(x = ERA5_land_ex_grid, y = int_result, by="id_cell", all=T)
ERA5_land_density[is.na(ERA5_land_density$n), ]$n <- 0

p_pyrenees_consensus <- ggplot() +
  geom_sf(data=ERA5_land_density , aes(fill=n), alpha=0.9, color=NA) + 
  scale_fill_gradient(low = "white", high = "darkgreen") +
  new_scale_fill() +
  geom_sf(data = consensus, aes(fill=origin_of_data), alpha=1) +
  scale_fill_manual(values=c("#f4a261", "#e9c46a", "#e76f51")) +
  theme_void()





