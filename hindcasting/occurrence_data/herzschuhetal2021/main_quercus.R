
#----------------------------------------------------------------------------#
# Distinguish between evergreen and deciduous Quercus pollen (when possible) #
#----------------------------------------------------------------------------#

# Author: V. Van der Meersch
# Date: 31/10/2023

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"
input_folder <- "D:/species/pollen/herzschuhetal2021"

library(tidyr)
library(plyr)
library(dplyr)
`%notin%` <- Negate(`%in%`)
library(neotoma2)
library(data.table)
library(terra)


#..................#
### PREPARE DATA ###
#..................#

# Load European dataset (Herzschuh et al. 2022)
dataset_count <- data.table::fread(file.path(input_folder, "Europe_pollen_datasets", "pollen_counts_europe.csv")) %>%
  as.data.frame()

# Load taxa harmonization table:
Harmonisation_Master <- read.csv(file.path(wd, "LegacyPollen", "taxa_harmonisation_table_VV.csv"), 
                                 header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, encoding = "UTF-8")

# Make minor corrections
source(file.path(wd, "scripts", "legacy_corrections.R"))

# Filter data
dataset_count <- dataset_count %>%
  dplyr::select(-c(`Cyperaceae (#)`)) %>% # remove Cyperaceae
  dplyr::mutate(pollen_count = rowSums(across(15:222))) %>%
  dplyr::filter(pollen_count >= 200) %>% # removing sequences with less than 200 pollen grain counts 
  dplyr::filter(`maximum_Age (cal. ka BP)` - `minimum_Age (cal. ka BP)` <= 0.5) %>% # less than 500yr uncertainty
  dplyr::filter(!(Dataset_ID %in% c(3902, 45214, 45216, 46713, 46714))) # problem with Neotoma2 database for these sample (see below)

# N.B.: https://data.neotomadb.org/3902, https://data.neotomadb.org/45214, https://data.neotomadb.org/45216
# https://data.neotomadb.org/46713, https://data.neotomadb.org/46714
# these URLs do not exist!

# Load site metadata
metadata <- fread(file.path(input_folder, "Europe_pollen_datasets","Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))

# Removing marine sites following Maguire et al. (2016)
dataset_count <- left_join(dataset_count, metadata, by = c("Dataset_ID" = "ID (Dataset)")) %>%
  dplyr::filter(`Loc type` != "Marine" & `Loc type` != "Lagoon")


#............................#
### PROCESS QUERCUS POLLEN ###
#............................#

IDs <- unique(dataset_count$Dataset_ID)
quercus_counts_df <- data.frame()
dataset_count_quercus <- data.frame()
errors <- c()
for(dataID in IDs){
  res <- try(source(file.path(wd, "scripts", "quercus_pollen_distinction.R")))
  if(class(res) == "try-error"){
    errors <- c(errors, dataID) # save ID when an error happened
  }
}

length(errors) # 1 error out of 871 datasets
# 3932: problem with depths

saveRDS(quercus_counts_df, file = file.path(wd, "output", "quercus_counts_df.rds")) # save intermediary file
names(dataset_count_quercus)[225:227] <- paste0(names(dataset_count_quercus)[225:227], " (#)") # same format as LegacyAge
dataset_count_quercus <- subset(dataset_count_quercus, select = -c(`Quercus (#)`)) # drop old Quercus column
dataset_count_quercus <- dataset_count_quercus[,c(1:221, 224:226, 222:223)]
saveRDS(dataset_count_quercus, file = file.path(wd, "output", "dataset_count_quercus.rds")) # save file


#........................#
### COMPUTE OCCURRENCE ###
#........................#

# Choose species and year range
field <- "Quercus.dec.type (#)" # species fieldname in custom dataset
years <- seq(500, 16500, 500)

# Find threshold with maximum relative abundance (not used anymore ?)
#sp_threshold <- find_threshold(years, window = 250, field, dataset_count, factor = 0.05)
sp_threshold <- 0.025 # quercus

# Loop on years
for(i in 1:length(years)){
  year <- years[i]
  
  grid <- readRDS(paste0("D:/simulations/phenofit/paleo/expert/025deg/fagus_sylvatica/",ifelse(year==0,15,year),"BP.rds"))
  names(grid) <- c("lat", "lon", "pred")
  grid <- rast(grid[,c("lon", "lat", "pred")])
  
  # "one is enough" binarization - specific threshold
  pollen_r <- create_pollen_raster(year, window = 250, 
                                   field = field, dataset_count_quercus, 
                                   grid,
                                   method = "one_is_enough", threshold = sp_threshold)
  pollen_df <- as.data.frame(pollen_r, xy = T)
  names(pollen_df) <- c("lon", "lat", "pres")
  pollen_df[,1:2] <- round(pollen_df[,1:2],2)
  output_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/0025thr_500yrunc"
  saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  
}

#.........................................................#
### GOING FURTHER WITH INDISTINGUISHABLE QUERCUS POLLEN ###
#.........................................................#

# We the Atlas Flora Europaea and the EuroVegMap native plant ranges to 
# determine which pollen are unlikely to be occurrence of evergreen Quercus

output_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc"

# AFE and EVM ranges + buffer of 25km
EVM <- vect(file.path("D:/species","EuroVegMap/evm_species.shp"))
AFE <- vect(file.path("D:/species","AFE/species/all.gpkg")) %>% tidyterra::filter(status == 6)
AFE_qilex <- aggregate(AFE[AFE$taxon_name == "Quercus ilex"])
EVM_qilex <- aggregate(EVM[!is.na(EVM$Quercus_il)]) %>% project(crs(AFE_qilex))
currenthabitat_qilex <- aggregate(buffer(aggregate(union(AFE_qilex, EVM_qilex)), width = 25000)) # 25km buffer

for(i in 1:length(years)){
  year <- years[i]
  pollen_df <- readRDS(file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
  pollen_df$likely_only_deciduous <- 0
  pollen_df[pollen_df$pres > 0, "likely_only_deciduous"] <- as.numeric(!is.related(vect(pollen_df[pollen_df$pres > 0,]), currenthabitat_qilex , "intersects"))
  saveRDS(pollen_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
}


