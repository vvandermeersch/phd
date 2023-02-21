
create_occurrence <- function(presence, absence){
  
  presence <- presence %>%
    dplyr::select(-c(nb_src, fold))
  presence$pres <- 1
  absence$pres <- 0
  
  return(rbind(presence,absence))
  
}