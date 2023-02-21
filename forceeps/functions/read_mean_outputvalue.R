library(vroom)

read_mean_outputvalue <- function(grid, ind_paths, var = "adultTreeBasalArea"){
  
  output <- matrix(ncol=length(grid), nrow=length(ind_paths), byrow=TRUE)
  
  cnames <- c("date", "patchId", "speciesId", "speciesShortName", "adultProdBasalArea", "adultProdBiomass", "adultTreeBasalArea", "adultTreeBiomass", 
              "deadBasalArea", "deadBiomass", "saplingBasalArea", "saplingBiomass", "adultTreeNumber", "deadNumber", "saplingNumber", "droughtIndexAnnual",
              "droughtIndexSeasonal")
  
  for(l in 1:length(ind_paths)){
    
    filenames <- list.files(path = ind_paths[l], pattern = "\\.siteproductivity.txt$", recursive = T, full.names = TRUE)
    
    yearly_results <- vroom(filenames, show_col_types = FALSE, progress=FALSE, skip = 9, col_names=cnames) %>%
      dplyr::select(var)
    
    nyears <- nrow(yearly_results)/length(grid)
    
    mean_results <- aggregate(yearly_results, list(rep(1:(nrow(yearly_results) %/% nyears + 1), each = nyears, len = nrow(yearly_results))), mean)[-1]
    
    output[l,] <- t(mean_results)
    
  }
  
  return(output)
}