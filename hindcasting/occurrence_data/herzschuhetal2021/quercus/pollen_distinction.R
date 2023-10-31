
# Quercus distinction bewteen evergreen and deciduous type (when possible)

# Directly Inspired from the code LegacyPollen 1.0
### Thomas Böhmer

### modify by V. van der Meersch to match with new version of R package neotoma2
### and to match the purpose of our work

cat(paste0("\nDataset ", dataID,"\n"))
source(file.path(wd, "quercus","harmonization.R")) # download sample from Neotoma database and harmonize

# If taxa is absent, we assigned zero count
quercus_taxa <- c("Quercus", "Quercus.dec.type", "Quercus.evg.type")
harmonized_counts_df[,quercus_taxa[which(quercus_taxa %notin% names(harmonized_counts_df))]] <- 0

# Verify if everything went good
harmonized_counts_df$depth <- round(harmonized_counts_df$depth/100,2)
harmonized_counts_df$Quercus_sum <- rowSums(harmonized_counts_df[,quercus_taxa])
dataset_count_quercus <- left_join(dataset_count[dataset_count$Dataset_ID == dataID,], harmonized_counts_df, 
                                   by = c("Site_ID" = "siteid", "Dataset_ID" = "datasetid", "Depth (m)" = "depth",
                                          "Quercus (#)" = "Quercus_sum"))

test <- dataset_count_quercus[, c("Quercus (#)", quercus_taxa)]
if(any(is.na(test[,quercus_taxa])) | any(rowSums(test[,quercus_taxa]) != test$`Quercus (#)`)){
  stop("Error durign verification!")
}

# Add to previous dataset
quercus_counts_df <- rbind(quercus_counts_df,
                           harmonized_counts_df[,c("siteid", "datasetid", "depth", quercus_taxa)])
cat(".")
  
