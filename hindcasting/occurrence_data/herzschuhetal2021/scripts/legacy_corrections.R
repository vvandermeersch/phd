
#------------------------------------------------#
# Make minor corrections to LegacyPollen dataset #
#------------------------------------------------#

# Some datasets do not have age uncertainties...
ids_without_uncertainties <- unique(dataset_count[dataset_count$AgeModel_Source == "Neotoma" & dataset_count$`median_Age (cal. ka BP)` == 0 &
                dataset_count$`minimum_Age (cal. ka BP)` == 0 & dataset_count$`maximum_Age (cal. ka BP)` == 0 & dataset_count$`mean_Age (cal. ka BP)` != 0, 
                c("Dataset_ID", "Site_Name")]) %>%
  left_join(fread(file.path(wd, "Table-S6_chronology_comparison.csv")), by = join_by(Dataset_ID))

# Dataset 22636: "Original chronology is within the 95% confidence intervals of the LegacyAge 1.0" 
# we assign LegacyAge chronology (a bit less reliable than varve-based model, but contain uncertainties)
chron_22636 <- data.table::fread(file.path(input_folder, "ages", "22636_17_ages.txt")) %>% # file send by Chenzhi Li
  dplyr::mutate(depth = depth/100) %>%
  dplyr::filter(depth %in% dataset_count[dataset_count$Dataset_ID == 22636, c("Depth (m)")]) %>%
  as.data.frame()
dataset_count[dataset_count$Dataset_ID == 22636, c("minimum_Age (cal. ka BP)", "maximum_Age (cal. ka BP)",
                                                   "median_Age (cal. ka BP)", "mean_Age (cal. ka BP)")] <- 
  chron_22636[,c("min","max","median", "mean")]/1000
dataset_count[dataset_count$Dataset_ID == 22636, "AgeModel_Source"] <- "AWI"
ids_without_uncertainties <- ids_without_uncertainties %>% dplyr::filter(Dataset_ID != 22636)

# Dataset 26608: "Original chronology is partially or completely outside the 95% confidence intervals of the LegacyAge 1.0"
# rather than keeping EPD chronology (without uncertainty), we choose the Giesecke et al. 2014 (MADCAP) chronology
dataID <- 26608
psite <- get_downloads(dataID, all_data = T, verbose = TRUE)
psite[[1]]$collunits[[1]]$chronologies <-  set_default(psite[[1]]$collunits[[1]]$chronologies,
                                                       16350) # Giesecke chronology
psamples <- samples(psite)
newchron <- unique(psamples[,c("age", "ageolder", "ageyounger", "depth")])
newchron$depth <- round(newchron$depth/100,2)
newchron$agemed <- (newchron$ageolder + newchron$ageyounger)/2
dataset_count[dataset_count$Dataset_ID == 26608, c("minimum_Age (cal. ka BP)", "maximum_Age (cal. ka BP)",
                                                   "median_Age (cal. ka BP)", "mean_Age (cal. ka BP)")] <-
  newchron[,c("ageyounger", "ageolder", "agemed", "age")]/1000
dataset_count[dataset_count$Dataset_ID == 26608, "AgeModel_Source"] <- "Neotoma"
ids_without_uncertainties <- ids_without_uncertainties %>% dplyr::filter(Dataset_ID != 26608)

# Remove other records without uncertainties... 
dataset_count <- dataset_count %>%
  dplyr::filter(Dataset_ID %notin% ids_without_uncertainties$Dataset_ID)

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
source(file.path(wd, "scripts","data_harmonization.R"))
taxa_42716 <- names(harmonized_counts_df)[7:length(harmonized_counts_df)]
dataset_count[dataset_count$Dataset_ID == 42716 & dataset_count$`Depth (m)` == 4.58, paste0(taxa_42716, " (#)")] <- 
  harmonized_counts_df[harmonized_counts_df$depth == 458, taxa_42716]



