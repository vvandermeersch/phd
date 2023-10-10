#--------------------------------------------------------#
# Small script to compute multi-model agreement ensemble #
#--------------------------------------------------------#

library(terra)
library(dplyr)

output_folder <- "D:/simulations/mmaensemble/paleo/025deg/fagus_sylvatica"

list_models <- c("D:/simulations/phenofit/paleo/migration/fitted/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                 "D:/simulations/phenofit/paleo/migration/expert/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                 "D:/simulations/csdm/random_forest/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                 "D:/simulations/csdm/gam/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                 "D:/simulations/csdm/brt/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                 "D:/simulations/csdm/lasso_glm/paleo/migration/fagus_sylvatica_expandLDD_from12k_scprb_2km20km",
                 "D:/simulations/castanea/paleo/migration/expert/fagus_sylvatica_expandLDD_from12k_scprb_2km20km")

for(year in seq(250, 12000, 250)){
  print(year)
  agreement_mmae <- c()
  init <- TRUE
  
  for(m in list_models){
    fitness <- readRDS(file.path(m, paste0(year, "BP.rds"))) %>% as.data.frame(xy = TRUE)
    fitness$output_raster <- ifelse(fitness$output_raster > 0, 1, 0)
    
    if(init){agreement_mmae <- fitness}else{agreement_mmae <- left_join(agreement_mmae, fitness, by = c("x", "y"))}
    
    init <- FALSE
  }
  agreement_mmae <- cbind(agreement_mmae[1:2], rowSums(agreement_mmae[3:9]))
  names(agreement_mmae)[3] <- "pred"
  
  saveRDS(agreement_mmae[c(2,1,3)], file.path(output_folder, paste0(year, "BP.rds")))
}
