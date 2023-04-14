### LegacyPollen 1.0
### Thomas Böhmer
### Alfred-Wegener-Institute Helmhotz Centre for Polar- and Marine Research, Potsdam, Germany 2021

### modify by V. van der Meersch to match with new version of R package neotoma2

baconAgeDepths <- baconAgeDepths_all[baconAgeDepths_all$ID == dataID,]


psite <- get_downloads(dataID, all_data = T)  
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
taxa_df <- full_join(Harmonisation_Master, subset_names)
taxa_df_subset <- taxa_df[which(!is.na(taxa_df$ID)), ]
taxa_df_subset <- unique(na.omit(taxa_df_subset))

# taxa harmonization
harmonized_pollen <- subset_counts %>%
  gather(key=taxa, value=count, -ID) %>%
  mutate(taxa=plyr::mapvalues(taxa, from=taxa_df_subset$TaxonName, to=taxa_df_subset$Harmonisation)) %>%
  group_by(ID, taxa) %>%
  summarise(count=sum(as.numeric(count)), .groups = 'drop') %>%
  spread(key=taxa, value=count)
harmonized_pollen <- as.data.frame(harmonized_pollen)

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

# remove rows with no counts
harmonized_counts_df <- subset(harmonized_counts_df, rowSums(harmonized_counts_df %>% select(-c(siteid, datasetid, sitename, lat, long, depth)), na.rm = TRUE) != 0)
site_info <- harmonized_counts_df %>%
  dplyr::select(c(siteid, datasetid, sitename, lat, long, depth))
subset_harmonized <- harmonized_counts_df %>%
  dplyr::select(-c(siteid, datasetid, sitename, lat, long, depth))

# calculate pollen percentages:
percentagetable <- as.data.frame(matrix(NA, dim(subset_harmonized)[1], dim(subset_harmonized)[2]))
colnames(percentagetable) <- names(subset_harmonized)


for(i in 1:dim(subset_harmonized)[1]){
  
  tablerow <- subset_harmonized[i, ]
  spp_rowsum <- rowSums(tablerow)
  
  for(j in 1:dim(tablerow)[2]){
    
    percentagetable[i,j] <- (tablerow[1,j]/spp_rowsum)*100
    
  }
}

if(all(round(rowSums(percentagetable)) == 100)){
  print("pollen percentages successfully calculated")
}



# create percentage dataframe with site information and homogenized pollen taxa:
harmonized_percentages_df <- cbind.data.frame(site_info,
                                              percentagetable)




### age assignment:

{
  
  ### the age-depth data provided for this example is taken from the dataset "Harmonized chronologies of a global late Quaternary pollen dataset (LegacyAge 1.0)"
  ### by Li et al. (2021), which is open accessible and available on PANGAEA (https://doi.pangaea.de/10.1594/PANGAEA.933132).
  
  ### Please refer to the dataset by Li et al. for any other age-depth data if you want to homogenize pollen data from this dataset for yourself.  
  
  
  # assign age-depth data to the depths from the pollen dataframe:
  
  {
    
    Depths <- harmonized_percentages_df$depth
    
    new_age <- data.frame(Age_Source=NA, minAgeBP=NA, maxAgeBP=NA, medianAgeBP=NA, meanAgeBP=NA)
    
    
    for(k in 1:length(Depths)){
      print(Depths[k])
      
      model_rest <- baconAgeDepths$depth[length(baconAgeDepths$depth)]-floor(baconAgeDepths$depth[length(baconAgeDepths$depth)])
      
      pollen_num <- as.numeric(as.character(floor(Depths[k]-model_rest)+model_rest))  
      
      pollen_rest <- (Depths[k]) - pollen_num
      
      # -------------------------------------------------------------------------------------------------
      
      # case 1: extrapolation at the lower border:
      
      {
        
        if(Depths[k] < baconAgeDepths$depth[1]){
          
          lower_age <- baconAgeDepths[1,c(2:5)]
          upper_age <- baconAgeDepths[2,c(2:5)]
          
          m <- (upper_age-lower_age)/(baconAgeDepths$depth[2]-baconAgeDepths$depth[1])
          b <- upper_age - m*baconAgeDepths$depth[2]
          
          new_age[k,c(2:5)] <- m*Depths[k]+b
          
        } # end if (case 1)
        
      }
      
      # -------------------------------------------------------------------------------------------------
      
      # case 2: interpolation between depths:
      
      {
        
        if(Depths[k] >= baconAgeDepths$depth[1] & Depths[k] < baconAgeDepths$depth[length(baconAgeDepths$depth)]){
          
          lower_age <- baconAgeDepths[baconAgeDepths$depth == pollen_num, c(2:5)]
          upper_age <- baconAgeDepths[baconAgeDepths$depth == pollen_num+1, c(2:5)]
          
          
          new_age[k,c(2:5)] <- lower_age*(1-pollen_rest) + upper_age*pollen_rest
          
        } # end if (case 2) 
        
      }
      
      # -------------------------------------------------------------------------------------------------
      
      # case 3: interpolation between depths (the last depth in the dataset):
      
      {
        
        if(Depths[k] == baconAgeDepths$depth[length(baconAgeDepths$depth)]){
          
          new_age[k,c(2:5)] <- baconAgeDepths[length(baconAgeDepths$depth), c(2:5)]
          
        } # end if (case 3) 
        
      }
      
      # -------------------------------------------------------------------------------------------------
      
      # case 4: extrapolation at the upper border:
      
      {
        
        if(Depths[k] > baconAgeDepths$depth[length(baconAgeDepths$depth)]){
          
          lower_age <- baconAgeDepths[length(baconAgeDepths$depth)-1, c(2:5)]
          upper_age <- baconAgeDepths[length(baconAgeDepths$depth), c(2:5)]
          
          m <- (upper_age-lower_age)/(baconAgeDepths$depth[length(baconAgeDepths$depth)]-baconAgeDepths$depth[length(baconAgeDepths$depth)-1])
          b <- upper_age - m*baconAgeDepths$depth[length(baconAgeDepths$depth)]
          
          new_age[k,c(2:5)] <- m*Depths[k]+b
          
        } # end if (case 4)
        
      }
      
      # -------------------------------------------------------------------------------------------------
      
    } # end for (calculate ages)
    
    
    new_age$Age_Source <- "AWI"
    
    # -------------------------------------------------------------------------------------------------
    
    # test if there are NA 
    if(any(is.na(new_age$meanAgeBP))){
      
      print("ages contain NA")
      
    }else{
      
      print("all ages assigned")
      
    }
    
    # -------------------------------------------------------------------------------------------------
    
  }
  
  # -------------------------------------------------------------------------------------------------
  
  # (A) assign age dataframe to the homogenized pollen counts dataframe:
  
  {
    
    if(length(new_age$meanAgeBP) > 0){
      
      if(length(new_age$meanAgeBP) == length(harmonized_counts_df$depth)){ 
        
        combine_age_counts <- cbind.data.frame(new_age, harmonized_counts_df) 
        
        combine_age_counts_sorted <- combine_age_counts[ ,c(6:11, 1:5, 12:ncol(combine_age_counts))]
        
        # save output file:
        write.table(combine_age_counts_sorted, file = file.path(wd, "quercus", paste0("output/final_harmonized_pollen_counts_dataset_",dataID,".csv")), fileEncoding="UTF-8", row.names=FALSE, sep="\t")
        
      } # end if (same length)
      
    } # end if (ages present)
    
    # -------------------------------------------------------------------------------------------------
    
  }  
  
  # -------------------------------------------------------------------------------------------------
  
  ### (B) assign ages to harmonized Pollen percentages:
  
  {
    
    if(length(new_age$meanAgeBP) > 0){
      
      if(length(new_age$meanAgeBP) == length(harmonized_percentages_df$depth)){ 
        
        combine_age_perc <- cbind.data.frame(new_age, harmonized_percentages_df) 
        
        combine_age_perc_sorted <- combine_age_perc[ ,c(6:11, 1:5, 12:ncol(combine_age_counts))]
        
        # save output file:
        write.table(combine_age_perc_sorted, file = file.path(wd, "quercus", paste0("output/final_harmonized_pollen_percentages_dataset_",dataID,".csv")), fileEncoding="UTF-8", row.names=FALSE, sep="\t")
        
      } # end if (same length)
      
    } # end if (ages present)
    
    # -------------------------------------------------------------------------------------------------
    
  }  
  
  # -------------------------------------------------------------------------------------------------
  
}
