#-----------------------#
# Process area occupied #
#-----------------------#



#----------#
# 1. Fagus #
#----------#

years <- c(seq(500,11500, 500))
brk <- 2000
models <- data.frame(name = c("PHENOFIT (fitted)",
                              "PHENOFIT",
                              "Random Forest",
                              "BRT",
                              "GAM"),
                     type = c("PHENOFIT (fitted)",
                              "PHENOFIT",
                              "cSDM",
                              "cSDM",
                              "cSDM"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/fagus_sylvatica_expandLDD",
                                   "D:/simulations/phenofit/paleo/migration/expert/fagus_sylvatica_expandLDD",
                                   "D:/simulations/csdm/random_forest/paleo/migration/fagus_sylvatica_expandLDD",
                                   "D:/simulations/csdm/brt/paleo/migration/fagus_sylvatica_expandLDD",
                                   "D:/simulations/csdm/gam/paleo/migration/fagus_sylvatica_expandLDD"))

model_occupied_area <- lapply(1:nrow(models),function(i){
  
  mod <- models[i,]
  
  perf <-lapply(years, function(year){
    print(year)
    
    distribution <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    distribution <- terra::unwrap(distribution)
    
    # area occupied by the species
    distribution[distribution != 2] <- NA
    area_occupied_km <- terra::expanse(distribution, unit = "km")
    
    return(data.frame(year = year, 
                      area_occupied_km = area_occupied_km,
                      mod = mod$name, type = mod$type))})
  return(do.call(rbind.data.frame, perf))}
)
model_occupied_area <- do.call(rbind.data.frame, model_occupied_area)
fagus_occupied_area <- model_occupied_area

#----------#
# 2. Abies #
#----------#

years <- c(seq(500,11500, 500))
brk <- 2000
models <- data.frame(name = c("PHENOFIT (fitted)",
                              "PHENOFIT",
                              "Random Forest",
                              "BRT",
                              "GAM"),
                     type = c("PHENOFIT (fitted)",
                              "PHENOFIT",
                              "cSDM",
                              "cSDM",
                              "cSDM"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/abies_alba_expandLDD",
                                   "D:/simulations/phenofit/paleo/migration/expert/abies_alba_expandLDD",
                                   "D:/simulations/csdm/random_forest/paleo/migration/abies_alba_expandLDD",
                                   "D:/simulations/csdm/brt/paleo/migration/abies_alba_expandLDD",
                                   "D:/simulations/csdm/gam/paleo/migration/abies_alba_expandLDD"))

model_occupied_area <- lapply(1:nrow(models),function(i){
  
  mod <- models[i,]
  
  perf <-lapply(years, function(year){
    print(year)
    
    distribution <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    distribution <- terra::unwrap(distribution)
    
    # area occupied by the species
    distribution[distribution != 2] <- NA
    area_occupied_km <- terra::expanse(distribution, unit = "km")
    
    return(data.frame(year = year, 
                      area_occupied_km = area_occupied_km,
                      mod = mod$name, type = mod$type))})
  return(do.call(rbind.data.frame, perf))}
)
model_occupied_area <- do.call(rbind.data.frame, model_occupied_area)
