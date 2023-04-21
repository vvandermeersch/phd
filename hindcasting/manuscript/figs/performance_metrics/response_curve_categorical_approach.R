
#----------------------------#
# Evaluate model performance #
#----------------------------#

library(hypervolume)

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
e_box <- expectation_box(calibration_predictors_s[,c("bio6", "bio12", "sum_apsep_GDD5", "w_bal")]) # broader estimate (range limits)


# Determine categories
predictors_cat <- lapply(seq(500,9000,500), function(year){
  
  predictors <- readRDS(file.path(pred_folder, paste0("predictors_", year, "BP.rds")))
  predictors <- predictors[, c("lat", "lon", "bio6", "bio12", "sum_apsep_GDD5", "w_bal")]
  
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
  
  return(predictors[,c("year", "lat", "lon", "overlapping", "combination", "novel")])
  
})
predictors_cat <- do.call(rbind.data.frame, predictors_cat)




model_response <- left_join(model_response, predictors_cat, by = c("year", "lat", "lon"))

bio6_plot <- .get_response_curve(model_response, variable = "bio6", 
                                 calibration_data = calibration_predictors,
                                 fullname = "Min. temperature of coldest month", 
                                 round_factor = 0.5,
                                 categorized = TRUE)

bio12_plot <- .get_response_curve(model_response, variable = "bio12", 
                                  calibration_data = calibration_predictors,
                                  fullname = "Annual precipitation", 
                                  round_factor = 100,
                                  categorized = TRUE)

wbal_plot <- .get_response_curve(model_response, variable = "w_bal",
                                 calibration_data = calibration_predictors,
                                 fullname = "Annual water balance", 
                                 round_factor = 20,
                                 categorized = TRUE)

gdd_plot <- .get_response_curve(model_response, variable = "sum_apsep_GDD5", 
                                calibration_data = calibration_predictors,
                                fullname = "Sum of GDD (>5°C, April-Sept.)", 
                                round_factor = 100,
                                categorized = TRUE)

response_figure_cat <- plot_grid(
  bio6_plot + theme(legend.position = 'none'), bio12_plot + theme(legend.position = 'none'),
  wbal_plot + theme(legend.position = 'none'), gdd_plot + theme(legend.position = 'none'),
  ncol = 1,
  align = "v"
) 

response_figure_cat <- plot_grid(
  response_figure_cat,
  get_legend(gdd_plot),
  ncol = 1,
  rel_heights = c(1,0.1)
)


