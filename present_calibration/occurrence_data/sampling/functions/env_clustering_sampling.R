
# Inspired by blockCV package (Valavi et al.)

# species_data : sf object containing species data 
# env_data : data.frame object of covariates to identify environmental groups

# k : number of the folds (e.g. k=10)
# nb_samples : total number of samples (e.g. nb_samples=1000)



env_clustering_sampling <- function(species_data, env_data, k, nb_samples){

  # transform species data
  species_data <- st_centroid(species_data) %>%
    dplyr::mutate(lon = round(sf::st_coordinates(.)[,1],1),
                  lat = round(sf::st_coordinates(.)[,2],1)) %>%
    st_drop_geometry()
  
  # extract environmental values
  ext <- left_join(species_data, env_data, by = c("lon", "lat")) %>%
    dplyr::select(-c(origin, lon, lat)) 
  
  # variables with wider ranges of values may dominate the clusters
  # first we need to standardize them
  ext <- scale(ext)
  
  if(anyNA(ext)){
    stop("The input environmental dataframe does not cover all the study area !")
  }
  
  # environmental clustering
  # k-means algorithms use Euclidean distance
  kms <- stats::kmeans(ext, centers = k, iter.max = 500, nstart = 25)
  species_data$fold <- kms$cluster
  
  # extract number of records per cluster to sample
  cluster_infos <- species_data %>% count(fold)
  cluster_infos$n_spl <- round(nb_samples * cluster_infos$n/sum(cluster_infos$n),0)
  
  # sampling
  new_species_data <- c()
  for(i in 1:k){
    n_spl <- as.numeric(cluster_infos[cluster_infos$fold==i, "n_spl"])
    species_data_k <- species_data[species_data$fold==i,]
    sample_k <- species_data_k[sample(1:nrow(species_data_k),n_spl,replace=FALSE),]
    new_species_data <- rbind(new_species_data, sample_k)
  }
  
  return(new_species_data)

}