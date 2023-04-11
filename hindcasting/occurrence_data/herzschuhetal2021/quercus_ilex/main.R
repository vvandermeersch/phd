# Code to extract Q. ilex pollen occurrences

# Directly Inspired from the code LegacyPollen 1.0
### Thomas Böhmer
### Alfred-Wegener-Institute Helmhotz Centre for Polar- and Marine Research, Potsdam, Germany 2021

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"

library(tidyr)
library(plyr)
library(dplyr)
`%notin%` <- Negate(`%in%`)
library(neotoma2)
library(data.table)

# load taxa harmonization table:
Harmonisation_Master <- read.csv(file.path(wd, "LegacyPollen", "taxa_harmonisation_table_VV.csv"), 
                                 header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, encoding = "UTF-8")

# load age-depth-file
baconAgeDepths_all <- read.table(file.path(wd, "LegacyAge", "Table-S5_legacyage_1_0_chronology.csv"), header=TRUE, encoding = "UTF-8")
names(baconAgeDepths_all) <- c("ID", "nb_dates", "depth", "min", "max", "median", "mean", "unc_depth", "unc_mean")
baconAgeDepths_all <- baconAgeDepths_all %>% dplyr::select("ID", "depth", "min", "max", "median", "mean")

# harmonize taxa name and age

IDs <- unique(fread(file.path("D:/species/pollen/herzschuhetal2021", "Herzschuh-etal_2021_Europe-counts.tab"), skip = 1302) %>% dplyr::select("ID (Dataset)"))
IDs <- IDs[IDs %notin% c(225, 331, 866, 873, 986, 1152, 1661, 1776, 
                         1836, 1958, 1972, 1974, 2025, 2063,2312, 
                         2319, 2627, 3078, 3080, 3557, 3561, 3776,
                         3861, 3898, 4010, 4142)] # problem with LegacyAge for these datasets
IDs <- IDs[IDs %notin% c(1107, 3902)] # problem with Neotoma for these datasets
for(i in 1:nrow(IDs)){
  dataID <- as.numeric(IDs[i,])
  print(dataID)
  try(source(file.path(wd, "quercus_ilex", "pollen_taxa_harmonization.R")))
}

# combine csv files
pollen_counts <- lapply(list.files(file.path(wd, "quercus_ilex", "output"), pattern = "pollen_counts", full.names = T), function(x){
  data <- read.table(x, header = T, sep= "\t")
  return(data)
})
pollen_counts <- do.call(rbind.fill, pollen_counts)
pollen_counts[is.na(pollen_counts)] <- 0

# removing sequences with less thant 150 pollen grain counts
pollen_counts <- pollen_counts %>% mutate(pollen_count = rowSums(across(12:221))) %>%
  dplyr::filter(pollen_count >= 150) 

# Load metadata
metadata <- fread(file.path("D:/species/pollen/herzschuhetal2021", "Herzschuh-etal_2021_Europe-meta.tab"), skip = 1108) %>%
  dplyr::select(c(`ID (Dataset)`, `Loc type`))
names(metadata) <- c("datasetid", "loctype")

# Removing marine sites following Maguire et al. (2016)
pollen_counts <- left_join(pollen_counts, metadata) %>%
  dplyr::filter(loctype != "Marine" & loctype != "Lagoon")

# Load climate data grid
grid <- fread("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/05deg/15BP/HadCM3B_Altitude.fit")
names(grid) <- c("lat", "lon", "alt")
grid <- rast(grid[,c("lon", "lat", "alt")])

output_folder <- "D:/species/pollen/processed/quercus_ilex"
for(year in seq(0, 21000, 500)){
  pres_r <- create_species_raster_qilex(year, field = 'Quercus.ilex', pollen_counts, grid)
  pres_df <- as.data.frame(pres_r, xy = T)
  names(pres_df) <- c("lon", "lat", "pres")
  
  saveRDS(pres_df, file.path(output_folder, paste0("pres_",year, "BP", ".rds")))
}

