#--------------------------------------------------------#
# Small script to compute multi-model agreement ensemble #
#--------------------------------------------------------#

library(terra)
library(dplyr)

output_folder <- "D:/simulations/mmensemble/paleo/025deg/fagus_sylvatica"

list_models <- c("D:/simulations/phenofit/paleo/fitted/025deg/fagus_sylvatica",
                 "D:/simulations/phenofit/paleo/expert/025deg/fagus_sylvatica",
                 "D:/simulations/csdm/random_forest/paleo/025deg/fagus_sylvatica",
                 "D:/simulations/csdm/gam/paleo/025deg/fagus_sylvatica",
                 "D:/simulations/csdm/brt/paleo/025deg/fagus_sylvatica",
                 "D:/simulations/csdm/lasso_glm/paleo/025deg/fagus_sylvatica")

for(year in seq(250, 12000, 250)){
  print(year)
  fitness_mme <- c()
  init <- TRUE
  
  for(m in list_models){
    fitness <- readRDS(file.path(m, paste0(year, "BP.rds"))) %>% as.data.frame(xy = TRUE)

    if(init){fitness_mme <- fitness}else{fitness_mme <- left_join(fitness_mme, fitness, by = c("lat", "lon"))}
    
    init <- FALSE
  }
  fitness_mme <- cbind(fitness_mme[1:2], rowMeans(fitness_mme[3:8]))
  names(fitness_mme)[3] <- "pred"
  
  saveRDS(fitness_mme, file.path(output_folder, paste0(year, "BP.rds")))
}
