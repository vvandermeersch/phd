# Script to load model perfomance metrics, with migration

model_performance <- lapply(1:nrow(models),function(i){
  
  mod <- models[i,]
  print(i)
  
  perf <-lapply(years, function(year){
    print(year)
      
    distribution <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    distribution <- terra::unwrap(distribution)
    distribution_df <- as.data.frame(distribution, xy = T)
    distribution_df[,1:2] <- round(distribution_df[,1:2], 2)
    names(distribution_df) <- c("lon", "lat", "pred")
    
    pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
    pollen <- left_join(pollen, distribution_df)
    pollen[pollen $lat > 66, "pred"] <- 0 # locations with lat > 66 are considered as not colonized (rather than NA)
    pollen <- na.omit(pollen)

    
    # proportion of suitable area
    prop_suit <- nrow(distribution_df[distribution_df$pred > 0,])/nrow(distribution_df)
    prop_mig <- nrow(distribution_df[distribution_df$pred == 2,])/nrow(distribution_df)
    prop_mig_suit <- nrow(distribution_df[distribution_df$pred == 2,])/nrow(distribution_df[distribution_df$pred > 0,])
    
    # area occupied by the species
    distribution[distribution != 2] <- NA
    area_occupied_km <- terra::expanse(distribution, unit = "km")
    
    # confusion matrix, with migration
    tp <- nrow(pollen[pollen$pred == 2 & pollen$pres == 1,])
    fp <- nrow(pollen[pollen$pred == 2 & pollen$pres == 0,])
    tn <- nrow(pollen[pollen$pred != 2 & pollen$pres == 0,])
    fn <- nrow(pollen[pollen$pred != 2 & pollen$pres == 1,])
    
    # metrics, with migration
    mig_sens = tp/(tp+fn)
    mig_spec = tn/(tn+fp)
    mig_opr = fp/(tp+fp) # overprediction rate
    mig_upr = fn/(tp+fn) # underprediction rate
    mig_tss = mig_sens + mig_spec - 1
    mig_sorensen = 2*tp/(fn + 2*tp + fp)
    
    
    # confusion matrix, without migration
    tp <- nrow(pollen[pollen$pred > 0 & pollen$pres == 1,])
    fp <- nrow(pollen[pollen$pred > 0 & pollen$pres == 0,])
    tn <- nrow(pollen[pollen$pred == 0 & pollen$pres == 0,])
    fn <- nrow(pollen[pollen$pred == 0 & pollen$pres == 1,])
    
    # metrics, with migration
    sens = tp/(tp+fn)
    spec = tn/(tn+fp)
    opr = fp/(tp+fp) # overprediction rate
    upr = fn/(tp+fn) # underprediction rate
    tss = sens + spec - 1
    sorensen = 2*tp/(fn + 2*tp + fp)
    
    return(data.frame(year = year, 
                      sens = sens, spec = spec, opr = opr, upr = upr, tss = tss, sorensen = sorensen,
                      mig_sens = mig_sens, mig_spec = mig_spec, mig_opr = mig_opr, mig_upr = mig_upr, mig_tss = mig_tss, mig_sorensen = mig_sorensen,
                      prop_mig =  prop_mig, prop_suit = prop_suit, area_occupied_km = area_occupied_km,
                      npollen_pres = length(which(pollen$pres==1)), 
                      npollen =length(which(!is.na(pollen$pres))), 
                      mod = mod$name, type = mod$type, type2 = mod$type2))})
  return(do.call(rbind.data.frame, perf))}
)
model_performance <- do.call(rbind.data.frame, model_performance)
