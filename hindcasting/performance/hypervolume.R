
library(hypervolume)

calibration_predictors <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/calibration_predictors.rds")
calibration_predictors = calibration_predictors[,c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")]

pred_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"
year <- 5000
predictors <- readRDS(file.path(pred_folder, paste0("predictors_", year, "BP.rds")))
predictors <- predictors[, c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")]

calibration_predictors_s <- calibration_predictors
predictors_s <- predictors
i <- 1
for(v in c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")){
  meanv_l[i] <- mean(model_data_norm[, v])
  sdv_l[i] <- sd(model_data_norm[, v])
  calibration_predictors_s[, v] <- (calibration_predictors_s[, v] - meanv_l[i]) / sdv_l[i]
  predictors_s[, v] <- (predictors_s[, v] - meanv_l[i]) / sdv_l[i]
  i <- i+1
}




#hv = hypervolume(calibration_predictors_s, method='gaussian')
#plot(hv)
#plot(hv, show.3d=TRUE)
e_convex <- expectation_convex(calibration_predictors_s, check.memory=F)
e_box <- expectation_box(calibration_predictors_s)


predictors$overlapping <- hypervolume_inclusion_test(hv, predictors_s, fast.or.accurate='accurate')
predictors$combination <- hypervolume_inclusion_test(e_box, predictors_s, fast.or.accurate='accurate')
predictors$novel <- FALSE
predictors[which(predictors$overlapping == FALSE & predictors$combination == FALSE), "novel"] <- TRUE


# check issues
predictors[which(predictors$overlapping == TRUE & predictors$combination == FALSE),]
