# Script to load area suitable/occupied

model_area <- lapply(1:nrow(models),function(i){
  
  mod <- models[i,]
  print(i)
  
  perf <-lapply(years, function(year){
    print(year)
    
    distribution <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    distribution <- terra::unwrap(distribution)
    distribution_df <- as.data.frame(distribution, xy = T)
    distribution_df[,1:2] <- round(distribution_df[,1:2], 2)
    names(distribution_df) <- c("lon", "lat", "pred")
    
    # proportion of suitable area
    prop_suit <- nrow(distribution_df[distribution_df$pred > 0,])/nrow(distribution_df)
    prop_mig <- nrow(distribution_df[distribution_df$pred == 2,])/nrow(distribution_df)
    prop_mig_suit <- nrow(distribution_df[distribution_df$pred == 2,])/nrow(distribution_df[distribution_df$pred > 0,])
    
    # area occupied by the species
    distribution[distribution != 2] <- NA
    area_occupied_km <- terra::expanse(distribution, unit = "km")
    
    
    return(data.frame(year = year, 
                      prop_suit = prop_suit, area_occupied_km = area_occupied_km,
                      mod = mod$name, type = mod$type, type2 = mod$type2))})
  
  return(do.call(rbind.data.frame, perf))}
)

model_area <- do.call(rbind.data.frame, model_area)
