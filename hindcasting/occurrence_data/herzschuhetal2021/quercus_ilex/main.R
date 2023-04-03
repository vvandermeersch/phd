# Code to extract Q. ilex pollen occurrences

# Directly Inspired from the code LegacyPollen 1.0
### Thomas Böhmer
### Alfred-Wegener-Institute Helmhotz Centre for Polar- and Marine Research, Potsdam, Germany 2021

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"

library(dplyr)
`%notin%` <- Negate(`%in%`)
library(neotoma2)

# -------------------------------------------------------------------------------------------------

### LOAD INPUT DATA

# load taxa harmonization table:
Harmonisation_Master <- read.csv(file.path(wd, "LegacyPollen", "taxa_harmonisation_table_VV.csv"), header=TRUE, stringsAsFactors=FALSE, check.names=FALSE)

# load age-depth-file
baconAgeDepths_all <- read.table(file.path(wd, "LegacyAge", "Table-S5_legacyage_1_0_chronology.csv"), header=TRUE)
names(baconAgeDepths_all) <- c("ID", "nb_dates", "depth", "min", "max", "median", "mean", "unc_depth", "unc_mean")
baconAgeDepths_all <- baconAgeDepths_all %>% dplyr::select("ID", "depth", "min", "max", "median", "mean")

# -------------------------------------------------------------------------------------------------

# harmonize taxa name and age
IDs <- unique(baconAgeDepths_all$ID)
IDs <- IDs[IDs %notin% c(225, 331, 866, 873, 986, 1152, 1661)] # problem with LegacyAge for these datasets
IDs <- IDs[IDs %notin% c(1107)] # problem with Neotoma for these datasets
for(dataID in IDs){
  print(dataID)
  source(file.path(wd, "quercus_ilex", "pollen_taxa_harmonization.R"))
}

