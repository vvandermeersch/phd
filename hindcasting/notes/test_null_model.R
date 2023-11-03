library(terra)
library(data.table)

current_distribution <- readRDS("D:/species/processed/abies_alba/abies_alba_presabs_woUkraine.rds")
current_distribution <- rast(current_distribution[c(3,2,1)])

current_resolution <- as.data.frame(fread("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_Altitude.fit"))
current_resolution$V3 <- 0
current_resolution <- rast(current_resolution[c(2,1,3)])

current_distribution <- extend(current_distribution, current_resolution)
current_distribution[is.na(current_distribution)] <- 0
current_distribution <- current_distribution + current_resolution


for(yr in seq(250, 12000, 250)){
  past_resolution <- as.data.frame(
    fread(paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/",yr,"BP/HadCM3B_Altitude.fit")))
  past_resolution$V3 <- 0
  past_resolution <- rast(past_resolution[c(2,1,3)])
  current_distribution_pastres <- resample(current_distribution, past_resolution, method = "average")
  current_distribution_pastres[is.na(current_distribution_pastres)] <- 0
  current_distribution_pastres <- current_distribution_pastres + past_resolution
  null <- as.data.frame(current_distribution_pastres, xy = T)[c(2,1,3)]
  names(null) <- c("lat", "lon", "pred")
  saveRDS(null, file.path("D:/simulations/null/paleo/025deg/abies_alba", paste0(yr, "BP.rds")))
}

