# Script to load model perfomance metrics

model_performance <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(years, function(year){
    print(year)
    
    if(combine_models){
      fitness1 <- readRDS(file.path(mod$simfolder1, paste0(year, "BP.rds")))
      fitness2 <- readRDS(file.path(mod$simfolder2, paste0(year, "BP.rds")))
      fitness3 <- readRDS(file.path(mod$simfolder3, paste0(year, "BP.rds")))
      fitness4 <- readRDS(file.path(mod$simfolder4, paste0(year, "BP.rds")))
      fitness <- data.frame(lat = fitness1$lat, lon = fitness1$lon, 
                            pred = rowMeans(cbind(fitness1$pred,fitness2$pred, fitness3$pred, fitness4$pred)))
    }else{
      
      fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
      
    }
    
    pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
    
    fitness$lat <- round(fitness$lat,2)
    fitness$lon <- round(fitness$lon,2)
    fitness <- left_join(fitness, pollen, by = c("lat", "lon"))
    
    # boyce index
    # boyce_ind <- ecospat.boyce(fit = fitness$pred, obs = na.omit(fitness[fitness$pres == 1,]$pred), 
    #                                   nclass=0, window.w="default", res=100, 
    #                                   PEplot = FALSE, rm.duplicate = TRUE,  method = 'spearman' )$cor
    
    boyce_ind <- as.numeric(Boyce(obs = fitness[fitness$pres == 1, c("lon", "lat")], pred = rast(fitness[,c(2,1,3)]), 
                                  main = "Boyce index", plot= FALSE,
                                  res = 100)$Boyce)
    
    
    # binarized predictions
    if(combine_models){
      ths <- readRDS(file.path(mod$modfolder1, mod$mod1))$best_threshold
      fitness1$bin_pred <- 0
      fitness1[fitness1$pred >= ths, "bin_pred"] <- 1
      
      ths <- readRDS(file.path(mod$modfolder2, mod$mod2))$best_threshold
      fitness2$bin_pred <- 0
      fitness2[fitness2$pred >= ths, "bin_pred"] <- 1
      
      ths <- readRDS(file.path(mod$modfolder3, mod$mod3))$best_threshold
      fitness3$bin_pred <- 0
      fitness3[fitness3$pred >= ths, "bin_pred"] <- 1
      
      ths <- readRDS(file.path(mod$modfolder4, mod$mod4))$best_threshold
      fitness4$bin_pred <- 0
      fitness4[fitness4$pred >= ths, "bin_pred"] <- 1
      
      fitness$bin_pred <- matrixStats::rowMaxs(cbind(fitness1$bin_pred,fitness2$bin_pred, fitness3$bin_pred, fitness4$bin_pred))
      fitness[fitness$bin_pred > 0, "bin_pred"] <- 1
      
    }else{
      
      ths <- readRDS(file.path(mod$modfolder, mod$mod))$best_threshold
      fitness$bin_pred <- 0
      fitness[fitness$pred >= ths, "bin_pred"] <- 1
      
    }
    
    # proportion of suitable area
    prop_suit <- nrow(fitness[fitness$bin_pred == 1,])/nrow(fitness)
    
    # auc
    fitness <- na.omit(fitness)
    eval_obj <- evalmod(scores = fitness$pred, labels = fitness$pres)
    aucroc <- precrec::auc(eval_obj)
    
    # confusion matrix
    tp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 1,])
    fp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 0,])
    tn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 0,])
    fn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 1,])
    
    # metrics
    sens = tp/(tp+fn)
    spec = tn/(tn+fp)
    opr = fp/(tp+fp) # overprediction rate
    upr = fn/(tp+fn) # underprediction rate
    tss = sens + spec - 1
    sorensen = 2*tp/(fn + 2*tp + fp)
    
    return(data.frame(year = year, 
                      bi = boyce_ind, auc = round(aucroc$aucs[1],5), 
                      sens = sens, spec= spec, opr = opr, upr = upr, tss = tss,
                      sorensen = sorensen,
                      prop_suit = prop_suit,
                      npollen_pres = length(which(fitness$pres==1)), 
                      npollen =length(which(!is.na(fitness$pres))), 
                      mod = mod$name, type = mod$type))})
  return(do.call(rbind.data.frame, perf))}
)
model_performance <- do.call(rbind.data.frame, model_performance)