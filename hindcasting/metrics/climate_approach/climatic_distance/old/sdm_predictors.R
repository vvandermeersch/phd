#-----------------------------------------------#
# Compute climatic distance from SDM predictors #
#-----------------------------------------------#

library(terra)
library(data.table)
library(dplyr)
library(future.apply)
library(pracma)
library(ggplot2)

# functions
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/functions/years_functions.r")
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/functions/load_altitude_icesheet.R")


# setup
bc_covars <- c("bio6", "bio12") # bioclim predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, cc_covars)


# baseline conditions (calibration data)
baseline_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors"
load(file.path(baseline_dir, "predictors_data.Rdata"))
predictors_data <- predictors_data %>% 
  dplyr::select(all_of(c("lat", "lon", covars)))
baseline_preds <- rast(lapply(1:length(covars), 
                      function(i) rast(predictors_data %>% 
                                         dplyr::select(all_of(c("lon", "lat", covars[i]))), crs = "EPSG:4326")))
baseline_preds <- aggregate(baseline_preds, 5, na.rm = T)

### past data and minimum climatic distance (mahalanobis)
hadcm3b_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"
past_climdist <- list()
years <- c(15, seq(1000,15000,1000), 17000, 19000, 21000)
past_climdist <- lapply(years, function(yr){
  predictors <- readRDS(file.path(hadcm3b_dir, paste0("predictors_", yr, "BP.rds"))) %>% 
    dplyr::select(all_of(c("lat", "lon", covars)))
  past_preds <- rast(lapply(1:length(covars), 
                            function(i) rast(predictors %>% 
                                               dplyr::select(all_of(c("lon", "lat", covars[i]))), crs = "EPSG:4326")))
  
  # mask with ice sheet
  # ice_sht <- rast(load_icesheet(yr, folder = data_dir, extent = extent))
  # ice_sht[ice_sht > 0.5] <- NA # restrict analyses to grid locations with less than 50% ice cover (Burke et al. 2019)
  # crs(ice_sht) <- crs(past_preds)
  # past_preds <- mask(past_preds, ice_sht)
  
  r_climdist <- climatic_dissimilarity(focal = past_preds, baseline = baseline_preds,
                                       method = "mahalanobis")
  
  return(as.data.frame(r_climdist, xy = T)) # return a dataframe to avoid problem of external pointer
})
past_climdist <- lapply(past_climdist, rast)
names(past_climdist) <- as.character(years)
mean_climdist <- sapply(past_climdist, function(i) global(i, mean, na.rm = T)$mean)
median_climdist <- sapply(past_climdist, function(i) global(i, median, na.rm = T)$global)
q25_climdist <- sapply(past_climdist, function(i) global(i, quantile, na.rm = T)$X25.)
q75_climdist <- sapply(past_climdist, function(i) global(i, quantile, na.rm = T)$X75.)
climdist_df <- data.frame(year = as.numeric(names(mean_climdist)), mean = mean_climdist, 
                          median = median_climdist, q25 = q25_climdist, q75 = q75_climdist)
ggplot(data = climdist_df, aes(x = year, y = median)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#94d2bd", alpha = 0.5) +
  geom_line(aes(y = q75), color = "#0a9396", linewidth = 0.3, alpha = 0.6) +
  geom_line(aes(y = q25), color = "#0a9396", linewidth = 0.3, alpha = 0.6) +
  geom_line(col = "#0a9396", linewidth = 1) +
  scale_y_continuous(limits = c(0,2), expand = c(0, 0)) +
  scale_x_reverse(expand = c(0.01, 0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey90", size = 0.6, linetype = "solid"),
        axis.line.x = element_line(colour = "grey90", size = 0.7, linetype = "solid"),
        axis.ticks.length=unit(.2, "cm")) +
  xlab("Years BP") +
  ylab("Climatic distance")



ggplot(data = climdist_df, aes(y = 1, )) +
  geom_vline(aes(xintercept = -year, color = -median), size = 5)

ggplot(data = climdist_df) +
  geom_histogram(aes(x = -year, fill = factor(median))) +
  scale_fill_gradient2(low = "#cee5f2", mid = "#accbe1", high = "#7c98b3")
  
  
ggplot() +
  geom_raster(data = climdist_df, aes(x = -year, y = 1, fill = median)) +
  scale_fill_gradient2(low = "#a9d6e5", mid = "#2c7da0", high = "#013a63", limits = c(0,1.5),
                       breaks = c(0,0.75,1.5), midpoint = 0.75) 
  
saveRDS(climdist_df, file = "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_novelty/save/predictors_climdist.rds")

