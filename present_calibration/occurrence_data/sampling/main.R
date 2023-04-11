##########################################
# Script to sample presence/absence data #
##########################################

# author : V. Van der Meersch
# date : 16/02/2022

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/occurrence_data/sampling/"


## Packages needed
library(future)
library(future.apply)
library(data.table)
library(dplyr)
library(stringr)
library(raster)
library(ggplot2)
library("abind")
setDTthreads(4)


## Functions needed
source(paste0(wd, 'functions/compute_biovars2.R')) # to compute bioclim variables from Phenofit daily variables, v2 faster !
source(paste0(wd, 'functions/sample_presence_by_env.R')) # to sample records in environmental clusters
source(paste0(wd, 'functions/process_occurrence.R')) # extract and assemble occurrence data from multiple databases
source(paste0(wd, 'functions/sample_absence.R')) # random sample of pseudo-absences
source(paste0(wd, 'functions/create_occurrence.R')) # merge presence and absence
source(paste0(wd, 'functions/generate_subsets.R')) # generate pres/abs combination


## Settings ##
processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed/"
rawdata_folder <- "D:/climate/ERA5-Land/raw/"
bioclimdata_folder <- "D:/climate/ERA5-Land/bioclim_format/"
speciesdata_folder <- "D:/species/"


## Compute bioclim variables... save in bioclimdata_folder
compute_biovars2(1950:1968, processeddata_folder, bioclimdata_folder, ncores=3)
gc()


## Process occurrence data ##

# load data
# EVM <- read_sf(paste0(speciesdata_folder,"EuroVegMap/evm_species.shp"))
EVM <- read_sf(paste0(speciesdata_folder,"EuroVegMap/evm_bonusspecies.shp"))
# AFE <- read_sf(paste0(speciesdata_folder,"AFE/species/all.gpkg")) %>% dplyr::filter(status == 6)
AFE <- read_sf(paste0(speciesdata_folder,"AFE/species/all_bonusspecies.gpkg")) %>% dplyr::filter(status == 6)
EUForest <- read.csv(paste0(speciesdata_folder,"EUForest/EuForestspecies.csv")) 
ERA5land <- raster(paste0(rawdata_folder,"2m_dewpoint_temperature_1969_01.nc"))
source(paste0(speciesdata_folder, "WOODIV/WOODIV_DB_release_v1/OCCURRENCE/WOODIV_working_file_generation.R"))
rm(working.file, working.file.Victor, WOODIV_Species_code, woodiv_grid, WOODIV_Occurrence_source, woodiv.grid.merge)
WOODIV <- rbind(woodiv_queile, woodiv_quepub, woodiv_pinpin) %>% dplyr::select(source_code, species=spcode, geometry)

# create rasters
GBIF <- fread(paste0(speciesdata_folder,"GBIF/Abies_alba_18012022/0109922-210914110416597.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
abiesalba_occ_rs <- process_occurrence("Abies alba", EUForest, GBIF, EVM, AFE, ERA5land)

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Fagus_sylvatica_20012022/0112486-210914110416597.csv"), sep="\t", dec=".", quote = "",
                             header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
fagussylvatica_occ_rs <- process_occurrence("Fagus sylvatica", EUForest, GBIF, EVM, AFE, ERA5land)
save(fagussylvatica_occ_rs, file = "D:/species/processed/fagus_sylvatica/fagus_sylvatica_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Quercus_ilex_24012022/0114082-210914110416597.csv"), sep="\t", dec=".", quote = "",
                          header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
quercusilex_occ_rs <- process_occurrence("Quercus ilex", EUForest, GBIF, EVM, AFE, ERA5land, WOODIV)
save(quercusilex_occ_rs, file = "D:/species/processed/quercus_ilex/quercus_ilex_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Larix_decidua_21012022/0112959-210914110416597.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
larixdecidua_occ_rs <- process_occurrence("Larix decidua", EUForest, GBIF, EVM, AFE, ERA5land)
save(larixdecidua_occ_rs, file = "D:/species/processed/larix_decidua/larixdecidua_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Pinus_sylvestris_21012022/0112955-210914110416597.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
pinussylvestris_occ_rs <- process_occurrence("Pinus sylvestris", EUForest, GBIF, EVM, AFE, ERA5land)
save(pinussylvestris_occ_rs, file = "D:/species/processed/pinus_sylvestris/pinussylvestris_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Picea_abies_20012022/0112455-210914110416597.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
piceaabies_occ_rs <- process_occurrence("Picea abies", EUForest, GBIF, EVM, AFE, ERA5land)
save(piceaabies_occ_rs, file = "D:/species/processed/picea_abies/piceaabies_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Quercus_robur_24012022/0114182-210914110416597.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
quercusrobur_occ_rs <- process_occurrence("Quercus robur", EUForest, GBIF, EVM, AFE, ERA5land)
save(quercusrobur_occ_rs, file = "D:/species/processed/quercus_robur/quercusrobur_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Quercus_petraea_24012022/0114179-210914110416597.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
quercuspetraea_occ_rs <- process_occurrence("Quercus petraea", EUForest, GBIF, EVM, AFE, ERA5land)
save(quercuspetraea_occ_rs, file = "D:/species/processed/quercus_petraea/quercuspetraea_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Quercus_pubescens_20012022/0112488-210914110416597.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
quercuspubescens_occ_rs <- process_occurrence("Quercus pubescens", EUForest, GBIF, EVM, AFE, ERA5land, WOODIV)
save(quercuspubescens_occ_rs, file = "D:/species/processed/quercus_pubescens/quercuspubescens_occ_rs.Rdata") # one can save file to prevent future computations

GBIF <- fread(paste0(speciesdata_folder,"GBIF/Pinus_pinaster_13122022/0214385-220831081235567.csv"), sep="\t", dec=".", quote = "",
              header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
pinuspinaster_occ_rs <- process_occurrence("Pinus pinaster", EUForest, GBIF, EVM, AFE, ERA5land, WOODIV)
save(pinuspinaster_occ_rs, file = "D:/species/processed/pinus_pinaster/pinuspinaster_occ_rs.Rdata") # one can save file to prevent future computations

## Sampling presence and absence ##

# Mean of 31 years of bioclim data
biovars_all <- list()
for(i in as.character(1970:2000)){
  load(paste0(bioclimdata_folder, "biovars_", i, ".Rdata"))
  biovars_all <- append(biovars_all, list(biovars))
}
rm(biovars)
biovars_30y <- abind(biovars_all, along=3)
biovars_30y <- apply(biovars_30y, c(1,2), mean)
biovars_30y <- as.data.frame(biovars_30y)


# Create several combinations 
N <- 5 # number of combinations
npres <- 1000 # number of presence points
nabs <- 1000 # number of absence points
nclusters <- 10 # number of clusters for presence data

output_folder <- "D:/species/processed/quercus_pubescens"
#load("D:/species/processed/quercus_ilex/quercusilex_occ_rs.Rdata")
#load("D:/species/processed/quercus_petraea/quercuspetraea_occ_rs.Rdata")

generate_subsets(N, npres, nabs, nclusters, quercuspubescens_occ_rs, output_folder)


 # Missing points ? Some points are not covered by ERA5-Land or soil databases
#fagussylvatica_missing <- sample_presence_by_env(fagussylvatica_occ_rs, biovars_30y, k = 10, nb_samples = 2000, debug=TRUE)



# Save all presence/pseudo-absence points to calculate a "true" AUC 
presences <- sample_presence_by_env(quercuspubescens_occ_rs, biovars_30y, k = NULL, nb_samples = NULL) %>% dplyr::select(-c(nb_src))
presences$pres <- 1
absences <- sample_absence(species_data = quercuspubescens_occ_rs, EUForest, ERA5land, nb_samples = NULL, env_data = biovars_30y)
absences$pres <- 0
quercuspubescens_presabs <- rbind(presences, absences)
save(quercuspubescens_presabs, file = "D:/species/processed/quercus_pubescens/quercus_pubescens_presabs.Rdata")
saveRDS(quercuspubescens_presabs, file = "D:/species/processed/quercus_pubescens/quercus_pubescens_presabs.rds")







## Map example ##
#####
library(rnaturalearth)
countries_EUForest <- as.character(unique(EUForest$COUNTRY))
countries_EUForest[7] <- "Czech Rep."
countries_EUForest[22] <- "Poland"
other_countries <- c("Ukraine", "Bosnia and Herzegovina", "Republic of Serbia", "Macedonia", "Greece",
                     "Kosovo", "Albania", "Montenegro")
countries <- c(countries_EUForest, other_countries)
world_map <- ne_countries(scale="medium",returnclass = 'sf')
eu_map <- world_map %>% filter(sovereignt %in% countries)
eu_map_cropped <- eu_map %>% 
  st_crop(st_bbox(c(xmin = -12, xmax = 40, ymax = 71, ymin = 34), crs = st_crs(4326)))
#####

abiesalba_occ_df <- na.omit(as.data.frame(abiesalba_occ_rs, xy=T))
load("D:/species/processed/abies_alba/2000pres_2000abs/occurrence_subset_1.Rdata")
species_occurrence <- fagussylvatica_occ
#species_occurrence$pres <- as.factor(species_occurrence$pres)

ggplot() + 
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
  geom_tile(data=abiesalba_occ_df, aes(x, y), fill = "darkgreen", alpha=0.2) +
  geom_tile(data = species_occurrence[species_occurrence$pres==1,], aes(x = lon, y = lat), fill = "darkgreen", alpha=1) +
  geom_tile(data = species_occurrence[species_occurrence$pres==0,], aes(x = lon, y = lat), fill = "grey", alpha=1) +
  #geom_tile(data = species_occurrence, aes(x = lon, y = lat, fill=factor(fold))) +
  #geom_tile(data = fagussylvatica_missing, aes(x = lon, y = lat, ), fill='black') +
  theme_void() +
  theme(legend.position="bottom") +
  labs(fill="Fold", col='Absence') +
  #ggtitle(paste("Sampling of F. sylvatica records within 10 clusters (mean of bioclim variables 1970-2000)")) +
  theme(plot.title = element_text(size=12))




quercusilex_occ_df <- na.omit(as.data.frame(quercusilex_occ_rs, xy=T))

ggplot() + 
  geom_sf(data = eu_map_cropped, fill = 'grey', alpha=0.1, color='grey') +
  geom_tile(data=quercusilex_occ_df, aes(x, y), fill = "darkgreen", alpha=0.4) +
  geom_sf(data = AFE[AFE$taxon == "439",], fill = 'grey', alpha=0.2, color='grey') +
  geom_sf(data = EVM[!is.na(EVM$Quercus_il),], fill = 'grey', alpha=0.2, color='grey') +
  theme_void() +
  theme(legend.position="bottom") +
  labs(fill="Fold") +
  theme(plot.title = element_text(size=12))

