# QUERCUS ONLY #

# To look at Woodiv data


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

list_species <- c("Quercus ilex", "Quercus pubescens")
list_species_abb <- c("queile", "quepub")
list_species_ws <- c("Quercusilex", "Quercuspubescens")
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
  st_crop(st_bbox(c(xmin = -12, xmax = 40, ymax = 47, ymin = 34), crs = st_crs(4326))) %>% 
  st_transform(crs = st_crs(eurostat_grid_1km))


##### GBIF data ######

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
GBIF_sf <- rbind(GBIF_queile_sf,
                 GBIF_quepub_sf)
GBIF_sf <- st_transform(GBIF_sf, crs = st_crs(eurostat_grid_1km))

GBIF_sf_unfiltered <- rbind(GBIF_queile_sf_unfiltered,
                            GBIF_quepub_sf_unfiltered)
GBIF_sf_unfiltered <- st_transform(GBIF_sf_unfiltered, crs = st_crs(eurostat_grid_1km))

# GBIF_Abiesalba_grid_1km <- eurostat_grid_1km[unique(unlist(st_intersects(GBIF_Abiesalba_sf, eurostat_grid_1km))),]
# GBIF_Abiesalba_grid_10km <- EEA_grid_10km[unique(unlist(st_intersects(GBIF_Abiesalba_sf, EEA_grid_10km))),]
rm(GBIF_queile_sf,
   GBIF_quepub_sf)

rm(GBIF_queile_sf_unfiltered,
   GBIF_quepub_sf_unfiltered)



##### WOODIV data #####
source("D:/species/WOODIV/WOODIV_DB_release_v1/OCCURRENCE/WOODIV_working_file_generation.R")
rm(working.file, working.file.Victor, WOODIV_Species_code, woodiv_grid, WOODIV_Occurrence_source, woodiv.grid.merge)

WOODIV_sf <- rbind(woodiv_queile, woodiv_quepub) %>% dplyr::select(source_code, species=spcode, geometry)

WOODIV_queile_1km <- transform_to_grid(WOODIV_sf, eurostat_grid_1km, "QILE", "WOODIV")
WOODIV_quepub_1km <- transform_to_grid(WOODIV_sf, eurostat_grid_1km, "QPUB", "WOODIV")

WOODIV_queile_ERA5 <- transform_to_grid(WOODIV_sf, ERA5land_grid, "QILE", "WOODIV")
WOODIV_quepub_ERA5 <- transform_to_grid(WOODIV_sf, ERA5land_grid, "QPUB", "WOODIV")

plot_woodiv_qrid <- ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
  geom_sf(data= woodiv_queile, fill="yellow", alpha=0.5, color=NA) + 
  geom_sf(data= WOODIV_queile_1km , fill="orange", alpha=1, color=NA) + 
  theme_void() +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))


###### CONSENSUS ######

# ERA5_land resolution
for(i in 1:ncol(list_species)){
  gbif <- paste0("GBIF_", list_species[2,i],"_grid_ERA5")
  assign(gbif, transform_to_grid(GBIF_sf, ERA5land_grid, list_species[1,i], "GBIF"))
  euforest <- paste0("EUForest_", list_species[2,i],"_grid_ERA5")
  assign(euforest, transform_to_grid(EUForest_sf, ERA5land_grid, list_species[1,i], "EU-Forest"))
  eval(parse(text=paste0("consensus_", list_species[2,i],"_ERA5 <- find_consensus(rbind(", gbif, ", ", euforest,
                         "))  %>% st_crop(st_bbox(extent(eu_map_cropped)))")))
}

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




plot_queile <- ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
  geom_sf(data= WOODIV_queile_ERA5, fill="orange", alpha=0.5, color=NA) +
  geom_sf(data= consensus_queile_ERA5, fill="darkgreen", alpha=0.2, color=NA) +
  theme_void() +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))

plot_quepub <- ggplot() +
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
  geom_sf(data= WOODIV_quepub_ERA5, fill="orange", alpha=0.5, color=NA) + 
  geom_sf(data= consensus_quepub_ERA5, fill="darkgreen", alpha=0.2, color=NA) +
  theme_void() +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))


