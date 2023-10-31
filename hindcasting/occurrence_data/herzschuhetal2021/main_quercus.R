
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


### PREPARE DATA ###

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

# Load metadata
metadata <- fread(file.path(input_folder, "Europe_pollen_datasets","Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))

# Removing marine sites following Maguire et al. (2016)
dataset_count <- left_join(dataset_count, metadata, by = c("Dataset_ID" = "ID (Dataset)")) %>%
  dplyr::filter(`Loc type` != "Marine" & `Loc type` != "Lagoon")


### PROCESS QUERCUS POLLEN ###

IDs <- unique(dataset_count$Dataset_ID)
quercus_counts_df <- data.frame()
errors <- c()
for(dataID in IDs){
  res <- try(source(file.path(wd, "quercus", "pollen_distinction.R")))
  if(class(res) == "try-error"){
    errors <- c(errors, dataID) # save ID when an error happened
  }
}

length(errors) # 1 error out of 911 datasets
# 3932: problem with depths





