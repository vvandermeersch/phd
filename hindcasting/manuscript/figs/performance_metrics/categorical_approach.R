
library(hypervolume)

# Load predictors (present conditions used during calibration)
calibration_predictors <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/calibration_predictors.rds")
calibration_predictors = calibration_predictors[,c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")]

pred_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"


calibration_predictors_s <- calibration_predictors
# Normalize predictors
meanv_l <- c()
sdv_l <- c()
i <- 1
for(v in c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")){
  meanv_l[i] <- mean(calibration_predictors_s[, v])
  sdv_l[i] <- sd(calibration_predictors_s[, v])
  calibration_predictors_s[, v] <- (calibration_predictors_s[, v] - meanv_l[i]) / sdv_l[i]
  i <- i+1
}

# Compute hypervolume
# hv = hypervolume(calibration_predictors_s, method='gaussian')
hv <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/performance/hypervolume/hypervolume_predictors.rds")
e_box <- expectation_box(calibration_predictors_s) # broader estimate (range limits)

# Determine categories
predictors_cat <- lapply(seq(1000,15000,1000), function(year){
  
  pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
  predictors <- readRDS(file.path(pred_folder, paste0("predictors_", year, "BP.rds")))
  predictors <- predictors[, c("lat", "lon", "bio6", "bio12", "sum_apsep_GDD5", "w_bal")]
  predictors <- na.omit(left_join(predictors, pollen, by = c("lat", "lon")))
  
  
  # normalize predictors
  i <- 1
  predictors_s <- predictors
  for(v in c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")){
    predictors_s[, v] <- (predictors_s[, v] - meanv_l[i]) / sdv_l[i]
    i <- i+1
  }
  
  predictors$overlapping <- hypervolume_inclusion_test(hv, predictors_s[,c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")], 
                                                       fast.or.accurate='accurate')
  predictors$combination <- hypervolume_inclusion_test(e_box, predictors_s[,c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")], 
                                                       fast.or.accurate='accurate')
  predictors$novel <- FALSE
  predictors[which(predictors$overlapping == FALSE & predictors$combination == FALSE), "novel"] <- TRUE
  
  # check and correct issues (temporary ?)
  cat(paste0("\nNumber of issues: ", nrow(predictors[which(predictors$overlapping == TRUE & predictors$combination == FALSE),]), "\n"))
  predictors[which(predictors$overlapping == TRUE & predictors$combination == FALSE), "combination"] <- TRUE
  
  predictors$year <- year
  
  return(predictors[,c("year", "lat", "lon", "pres", "overlapping", "combination", "novel")])
  
})
predictors_cat <- do.call(rbind.data.frame, predictors_cat)



# Link with model performances
model_performance_cat <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(seq(500,9000,500), function(year){

    fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    fitness <- right_join(fitness, predictors_cat[predictors_cat$year == year,], by = c("lat", "lon"))
    
    # Compute AUCs
    eval_obj <- evalmod(scores = fitness[which(fitness$overlapping == TRUE),'pred'], 
                        labels = fitness[which(fitness$overlapping == TRUE),'pres'])
    auc_overlapping <- round(precrec::auc(eval_obj)$aucs[1],2)
    npres_overlapping <- nrow(fitness[which(fitness$overlapping == TRUE & fitness$pres == 1),])
    nabs_overlapping <- nrow(fitness[which(fitness$overlapping == TRUE & fitness$pres == 0),])
    
    auc_combination <- NA
    if(length(unique(fitness[which(fitness$overlapping == FALSE & fitness$combination == TRUE),'pres'])) > 1){
      eval_obj <- evalmod(scores = fitness[which(fitness$overlapping == FALSE & fitness$combination == TRUE),'pred'], 
                          labels = fitness[which(fitness$overlapping == FALSE & fitness$combination == TRUE),'pres'])
      auc_combination <- round(precrec::auc(eval_obj)$aucs[1],2)
    }
    npres_combination <- nrow(fitness[which(fitness$overlapping == FALSE & fitness$combination == TRUE & fitness$pres == 1),])
    nabs_combination <- nrow(fitness[which(fitness$overlapping == FALSE & fitness$combination == TRUE & fitness$pres == 0),])
    
    auc_novel <- NA
    if(length(unique(fitness[which(fitness$novel == TRUE),'pres'])) > 1){
      eval_obj <- evalmod(scores = fitness[which(fitness$novel == TRUE),'pred'], 
                          labels = fitness[which(fitness$novel == TRUE),'pres'])
      auc_novel <- round(precrec::auc(eval_obj)$aucs[1],2)
    }
    npres_novel <- nrow(fitness[which(fitness$novel == TRUE & fitness$pres == 1),])
    nabs_novel <- nrow(fitness[which(fitness$novel == TRUE & fitness$pres == 0),])
    
    return(data.frame(year = year, 
                      auc_overlapping = auc_overlapping,
                      npres_overlapping = npres_overlapping,
                      nabs_overlapping = nabs_overlapping,
                      auc_combination = auc_combination,
                      npres_combination = npres_combination,
                      nabs_combination = nabs_combination,
                      auc_novel = auc_novel,
                      npres_novel = npres_novel,
                      nabs_novel = nabs_novel,
                      mod = mod$name))})
  return(do.call(rbind.data.frame, perf))}
)
model_performance_cat <- do.call(rbind.data.frame, model_performance_cat)





