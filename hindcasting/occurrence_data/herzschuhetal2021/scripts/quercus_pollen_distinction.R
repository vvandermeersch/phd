
#------------------------------------------------#
# Quercus evergreen/deciduous distinction script #
#------------------------------------------------#

cat(paste0("\nDataset ", dataID,"\n"))
source(file.path(wd, "scripts","data_harmonization.R")) # download sample from Neotoma database and harmonize

# If taxa is absent, we assigned zero count
names(harmonized_counts_df)[names(harmonized_counts_df) == "Quercus"] <- "Quercus.indist" # indistinguishable pollen
quercus_taxa <- c("Quercus.indist", "Quercus.dec.type", "Quercus.evg.type")  
harmonized_counts_df[,quercus_taxa[which(quercus_taxa %notin% names(harmonized_counts_df))]] <- 0

# Verify if everything went good
harmonized_counts_df$depth <- round(harmonized_counts_df$depth/100,2)
harmonized_counts_df$Quercus_sum <- rowSums(harmonized_counts_df[,quercus_taxa])

if(dataID %in% c(20018, 20027, 20165, 22960, 22988, 24188, 24305, 40443, 40955, 40963, 41460, 45196, 46280, 47520)){
  # Additional keys to avoid multiple matches
  dataset_count_quercus_id <- left_join(dataset_count[dataset_count$Dataset_ID == dataID,], harmonized_counts_df, 
                                     by = c("Site_ID" = "siteid", "Dataset_ID" = "datasetid", "Depth (m)" = "depth",
                                            "Quercus (#)" = "Quercus_sum",
                                            "Alnus (#)" = "Alnus",
                                            "Rumex (#)" = "Rumex",
                                            "Salix (#)" = "Salix",
                                            "Poaceae (#)" = "Poaceae"))
}else{
  dataset_count_quercus_id <- left_join(dataset_count[dataset_count$Dataset_ID == dataID,], harmonized_counts_df, 
                                     by = c("Site_ID" = "siteid", "Dataset_ID" = "datasetid", "Depth (m)" = "depth",
                                            "Quercus (#)" = "Quercus_sum"))
}


test <- dataset_count_quercus_id[, c("Quercus (#)", quercus_taxa)]
if(any(is.na(test[,quercus_taxa])) | any(rowSums(test[,quercus_taxa]) != test$`Quercus (#)`)){
  stop("Error durign verification!")
}

# Add to previous datasets
quercus_counts_df <- rbind(quercus_counts_df,
                           harmonized_counts_df[,c("siteid", "datasetid", "depth", quercus_taxa)])

dataset_count_quercus <- rbind(dataset_count_quercus,
                                  dataset_count_quercus_id[,c(names(dataset_count), quercus_taxa)])

cat(".")
  
