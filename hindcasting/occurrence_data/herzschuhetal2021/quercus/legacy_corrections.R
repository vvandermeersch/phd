
# Script to make some corrections to LegacyPollen dataset


# Change chronologies for some datasets according to LegacyAge (to obtain age uncertainty)
chron_22636 <- data.table::fread(file.path(input_folder, "ages", "22636_17_ages.txt")) %>% # file send by Chenzhi Li
  dplyr::mutate(depth = depth/100) %>%
  dplyr::filter(depth %in% dataset_count[dataset_count$Dataset_ID == 22636, c("Depth (m)")]) %>%
  as.data.frame()
dataset_count[dataset_count$Dataset_ID == 22636, c("minimum_Age (cal. ka BP)", "maximum_Age (cal. ka BP)",
                                                   "median_Age (cal. ka BP)", "mean_Age (cal. ka BP)")] <- 
  chron_22636[,c("min","max","median", "mean")]/1000


# Regroup depth for some datasets (according to data in Neotoma2 R package)
dataset_count[dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` == 3.78, 15:223] <-
  colSums(dataset_count[dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` %in% c(3.78,3.79), 15:223])
dataset_count <- dataset_count %>% 
  dplyr::filter(!(dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` == 3.79))
dataset_count[dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` == 3.82, 15:223] <-
  colSums(dataset_count[dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` %in% c(3.82,3.83), 15:223])
dataset_count <- dataset_count %>% 
  dplyr::filter(!(dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` == 3.83))
dataset_count[dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` == 4.63, 15:223] <-
  colSums(dataset_count[dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` %in% c(4.63,4.64), 15:223])
dataset_count <- dataset_count %>% 
  dplyr::filter(!(dataset_count$Dataset_ID == 22636 & dataset_count$`Depth (m)` == 4.64))


# Correction for dataset 42716 (updated number of pollen in Neotoma2 at depth 4.58m ?)
dataID <- 42716
source(file.path(wd, "quercus"," harmonization.R"))
taxa_42716 <- names(harmonized_counts_df)[7:length(harmonized_counts_df)]
dataset_count[dataset_count$Dataset_ID == 42716 & dataset_count$`Depth (m)` == 4.58, paste0(taxa_42716, " (#)")] <- 
  harmonized_counts_df[harmonized_counts_df$depth == 458, taxa_42716]



