# Script to load model perfomance metrics, with migration

model_performance_withmig <- lapply(1:nrow(models),function(i){
  
  mod <- models[i,]
  print(i)
  
  perf <-lapply(years, function(year){
    print(year)
    
    distribution <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    distribution <- terra::unwrap(distribution)
    distribution_df <- as.data.frame(distribution, xy = T)
    distribution_df[,1:2] <- round(distribution_df[,1:2], 2)
    names(distribution_df) <- c("lon", "lat", "mig")
    
    fitness <- readRDS(file.path(mod$simfolder, paste0("fitness_", year, "BP.rds")))
    fitness <- terra::unwrap(fitness)
    fitness_df <- as.data.frame(fitness, xy = T)
    fitness_df[,1:2] <- round(fitness_df[,1:2], 2)
    names(fitness_df) <- c("lon", "lat", "fit")
    
    pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
    pollen <- left_join(pollen, distribution_df)
    pollen <- left_join(pollen, fitness_df)
    pollen[pollen $lat > 66, "mig"] <- 0 # locations with lat > 66 are considered as not colonized (rather than NA)
    pollen[pollen $lat > 66, "fit"] <- 0 # locations with lat > 66 are considered as not colonized (rather than NA)
    pollen <- na.omit(pollen)
    
    # proportion of suitable area
    prop_suit <- nrow(distribution_df[distribution_df$mig > 0,])/nrow(distribution_df)
    prop_mig <- nrow(distribution_df[distribution_df$mig == 2,])/nrow(distribution_df)
    prop_mig_suit <- nrow(distribution_df[distribution_df$mig == 2,])/nrow(distribution_df[distribution_df$mig > 0,])
    
    # area occupied by the species
    distribution[distribution != 2] <- NA
    area_occupied_km <- terra::expanse(distribution, unit = "km")
    
    # confusion matrix, with migration
    tp <- nrow(pollen[pollen$mig == 2 & pollen$pres == 1,])
    fp <- nrow(pollen[pollen$mig == 2 & pollen$pres == 0,])
    tn <- nrow(pollen[pollen$mig != 2 & pollen$pres == 0,])
    fn <- nrow(pollen[pollen$mig != 2 & pollen$pres == 1,])
    
    # metrics, with migration
    mig_sens = tp/(tp+fn)
    mig_spec = tn/(tn+fp)
    mig_opr = fp/(tp+fp) # overprediction rate
    mig_upr = fn/(tp+fn) # underprediction rate
    mig_tss = mig_sens + mig_spec - 1
    mig_sorensen = 2*tp/(fn + 2*tp + fp)
    
    eval_obj <- precrec::evalmod(scores = pollen$fit, labels = pollen$pres)
    aucroc <- precrec::auc(eval_obj)$aucs[1]
    
    
    # confusion matrix, without migration
    tp <- nrow(pollen[pollen$mig > 0 & pollen$pres == 1,])
    fp <- nrow(pollen[pollen$mig > 0 & pollen$pres == 0,])
    tn <- nrow(pollen[pollen$mig == 0 & pollen$pres == 0,])
    fn <- nrow(pollen[pollen$mig == 0 & pollen$pres == 1,])
    
    # metrics, without migration
    sens = tp/(tp+fn)
    spec = tn/(tn+fp)
    opr = fp/(tp+fp) # overprediction rate
    upr = fn/(tp+fn) # underprediction rate
    tss = sens + spec - 1
    sorensen = 2*tp/(fn + 2*tp + fp)
    
    return(data.frame(year = year, 
                      sens = sens, spec = spec, opr = opr, upr = upr, tss = tss, sorensen = sorensen,
                      mig_sens = mig_sens, mig_spec = mig_spec, mig_opr = mig_opr, mig_upr = mig_upr, 
                      mig_tss = mig_tss, mig_sorensen = mig_sorensen, mig_auc = aucroc,
                      prop_mig =  prop_mig, prop_suit = prop_suit, area_occupied_km = area_occupied_km,
                      npollen_pres = length(which(pollen$pres==1)), 
                      npollen =length(which(!is.na(pollen$pres))), 
                      mod = mod$name, type = mod$type, type2 = mod$type2))})
  return(do.call(rbind.data.frame, perf))}
)
model_performance_withmig <- do.call(rbind.data.frame, model_performance_withmig)
