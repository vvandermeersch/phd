
# Inspired by blockCV package (Valavi et al.)

# species_data : sf object containing species presence
# env_data : data.frame object of covariates to identify environmental groups

# k : number of the folds (e.g. k=10)
# nb_samples : total number of samples (e.g. nb_samples=1000)

sample_presence_by_env <- function(species_data, env_data, k, nb_samples, debug=FALSE){
  
  # transform species data
  species_data_df <- as.data.frame(species_data, xy=T) 
  species_data_df <- na.omit(species_data_df) %>%
    dplyr::mutate(lon = round(x,1),
                  lat = round(y,1)) %>% 
    dplyr::select(-c(x,y))
  
  # extract environmental values
  env_data$lat <- round(env_data$lat,1)
  env_data$lon <- round(env_data$lon,1)
  
  ext <- left_join(species_data_df, env_data, by = c("lon", "lat"))
  if(anyNA(ext)){
    message("The input environmental dataframe does not cover all the study area !")
    if(debug){
      message("Debug = TRUE, returning missing points...")
      
      return(ext[is.na(ext$bio1), c("lon", "lat")])
    }
    ext <- inner_join(species_data_df, env_data, by = c("lon", "lat")) %>%
      dplyr::select(-c(nb_src, lon, lat))
    message(paste0("Droping ", nrow(species_data_df)-nrow(ext), " species records (out of ", nrow(species_data_df)," records)..."))
    species_data_df <- inner_join(species_data_df, env_data, by = c("lon", "lat")) %>%
      dplyr::select(c(nb_src, lon, lat))
  }
  
  if(is.null(nb_samples)){
    #return all presence points 
    return(species_data_df)
  }
  
  # variables with wider ranges of values may dominate the clusters
  # first we need to standardize them
  ext <- scale(ext)
  
  # environmental clustering
  # k-means algorithms use Euclidean distance
  kms <- stats::kmeans(ext, centers = k, iter.max = 10000000, nstart = 25, algorithm="Lloyd")
  species_data_df$fold <- kms$cluster
  
  # extract number of records per cluster to sample
  cluster_infos <- species_data_df %>% count(fold)
  cluster_infos$n_spl <- round(nb_samples * cluster_infos$n/sum(cluster_infos$n),0)
  
  # sampling
  new_species_data <- c()
  for(i in 1:k){
    n_spl <- as.numeric(cluster_infos[cluster_infos$fold==i, "n_spl"])
    species_data_k <- species_data_df[species_data_df$fold==i,]
    sample_k <- species_data_k[sample(1:nrow(species_data_k),n_spl,replace=FALSE),]
    new_species_data <- rbind(new_species_data, sample_k)
  }
  
  return(new_species_data)
  
}
