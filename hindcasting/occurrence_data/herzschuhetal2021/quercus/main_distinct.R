

library(tidyr)
library(plyr)
library(dplyr)
`%notin%` <- Negate(`%in%`)
library(neotoma2)
library(data.table)
library(terra)


input_folder <- "D:/species/pollen/herzschuhetal2021/Europe_pollen_datasets"
wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"

# Load European dataset (Herzschuh et al. 2022)
dataset_count <- data.table::fread(file.path(input_folder, "pollen_counts_europe.csv")) %>%
  dplyr::select(-c(`Cyperaceae (#)`)) %>% # remove Cyperaceae
  dplyr::mutate(pollen_count = rowSums(across(15:222))) %>%
  dplyr::filter(pollen_count >= 200) %>% # removing sequences with less than 200 pollen grain counts 
  dplyr::filter(`maximum_Age (cal. ka BP)` - `minimum_Age (cal. ka BP)` <= 0.5) %>%
  as.data.frame()

# Load taxa harmonization table:
Harmonisation_Master <- read.csv(file.path(wd, "LegacyPollen", "taxa_harmonisation_table_VV.csv"), 
                                 header=TRUE, stringsAsFactors=FALSE, check.names=FALSE, encoding = "UTF-8")


# Process datasets
IDs <- unique(dataset_count$Dataset_ID)
IDs <- IDs[IDs %notin% c(3902)] # problem with Neotoma database for this sample (https://data.neotomadb.org/3902 does not exist!)
quercus_counts_df <- data.frame()
errors <- c()
for(dataID in IDs){
  res <- try(source(file.path(wd, "quercus", "pollen_distinction.R")))
  if(class(res) == "try-error"){
    errors <- c(errors, dataID) # save ID when an error happened
  }
}

length(errors) # 35 errors out of 911 datasets
# 3932

  






