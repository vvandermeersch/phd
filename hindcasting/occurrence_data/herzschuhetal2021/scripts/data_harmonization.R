
#------------------------------------------------#
# Process and harmonize pollen data from Neotoma #
#------------------------------------------------#

# Directly Inspired from the code LegacyPollen 1.0 (Thomas Böhmer)

### modify by V. van der Meersch to match with new version of R package neotoma2
### and to match the purpose of our work

psite <- get_downloads(dataID, all_data = T, verbose = TRUE)
psamples <- samples(psite)

psamples <- psamples %>%
  group_by(siteid, datasetid, sitename, lat, long, depth) %>%
  dplyr::filter(units %in% c("NISP", "NISP digitized")) %>%
  dplyr::select(siteid, datasetid, sitename, lat, long, depth, variablename, value) %>%
  pivot_wider(names_from = variablename, values_from = value, values_fn=sum) %>%
  ungroup()

subset_counts <- psamples %>%
  dplyr::select(-c(siteid, datasetid, sitename, lat, long, depth))
subset_counts <- cbind(ID=seq(1:nrow(subset_counts)), subset_counts)

# join the Master Harmonisation table with the subset and trim it to only show shared taxa names
name_variant <- names(subset_counts)
length(name_variant[-1]) # number of unhomogenized pollen taxa before the harmonization
subset_names <- data.frame(seq(1:length(name_variant[-1])), data.frame(name_variant[-1]))
colnames(subset_names) <- c("ID","TaxonName")
taxa_df <- full_join(Harmonisation_Master, subset_names, by = join_by(TaxonName))
taxa_df_subset <- taxa_df[which(!is.na(taxa_df$ID)), ]
taxa_df_subset <- unique(na.omit(taxa_df_subset))
cat(".")

# taxa harmonization
harmonized_pollen <- subset_counts %>%
  gather(key=taxa, value=count, -ID) %>%
  dplyr::mutate(taxa=plyr::mapvalues(taxa, from=taxa_df_subset$TaxonName, to=taxa_df_subset$Harmonisation)) %>%
  dplyr::group_by(ID, taxa) %>%
  dplyr::summarise(count=sum(as.numeric(count), na.rm = T), .groups = 'drop') %>%
  spread(key=taxa, value=count)
harmonized_pollen <- as.data.frame(harmonized_pollen)
cat(".")

# delete the Index- and "Delete"-columns from the homogenized dataset:
# "Delete" is a group of taxa and notations that are excluded from the homogenization like e.g. aquatic taxa or ferns
if(is.element("Delete", names(harmonized_pollen))) {subset_harmonized <- subset(harmonized_pollen, select=-c(ID,Delete)) 
}else{subset_harmonized <- subset(harmonized_pollen, select=-ID)}

dim(subset_harmonized)[2] # number of homogenized pollen taxa after the harmonization

# replace NA with zero
subset_harmonized[is.na(subset_harmonized)] <- 0

# create count dataframe with site information and homogenized pollen taxa
site_info <- psamples %>%
  dplyr::select(c(siteid, datasetid, sitename, lat, long, depth))
harmonized_counts_df <- cbind.data.frame(site_info,
                                         subset_harmonized)