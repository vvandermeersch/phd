
#------------------------------#
# Script to compute predictors #
#------------------------------#

cat(paste0("Year ", year, ":\n"))

# Compute bioclim variables... saved in bioclimdata_folder
dir.create(file.path(bioclimdata_folder, paste0(year, "BP")), showWarnings = F)
cat("  Computing bioclim variables...\n")
compute_biovars(years[1]:years[2], file.path(phenofitdata_folder, paste0(year, "BP")), 
                file.path(bioclimdata_folder, paste0(year, "BP")), ncores=2)
gc()


# Mean of 31 years of bioclim variables
cat("  Averaging bioclim variables...\n")
biovars_all <- list()
for(i in as.character(years[1]:years[2])){
  load(file.path(bioclimdata_folder, paste0(year, "BP"), paste0("biovars_", i, ".Rdata")))
  biovars_all <- append(biovars_all, list(biovars))
}
biovars_30y <- abind(biovars_all, along=3)
biovars_30y <- apply(biovars_30y, c(1,2), mean)
biovars_30y <- as.data.frame(biovars_30y)
biovars_30y$lat <- round(biovars_30y$lat,2)
biovars_30y$lon <- round(biovars_30y$lon,2)
rm(biovars, biovars_all)


# Mean GDD sum between April and September, threshold 5deg
cat("  Computing mean GDD...\n")
day_begin <- 91
day_end <- 273
base_temp <- 5
sumGDD_all <- c()
for(i in years[1]:years[2]){
  tmean_file <- file.path(phenofitdata_folder, paste0(year, "BP"), paste0("HadCM3B_", "tmp", "_", i, "_dly.fit"))
  tmean <- fread(tmean_file, showProgress = F)
  names(tmean)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(tmean) == 368){
    # leap year
    start <- start + 1
    end <- end +1 
  }
  sumGDD <- rowSums(replace(tmean[, (start+2):(end+2)], tmean[, (start+2):(end+2)] < base_temp, 0)) #+2 beacause of lat&lon columns
  sumGDD_all <- c(sumGDD_all, sumGDD)
}
sumGDD5_apsep_30y <- matrix(sumGDD_all, nrow = nrow(tmean), byrow = F)
sumGDD5_apsep_30y <- apply(sumGDD5_apsep_30y, 1, mean)
rm(tmean, sumGDD_all)


# Mean water balance (precipitation - PET), between June and July
cat("  Computing water balance...\n")
day_begin <- 152
day_end <- 212
water_bal_all <- c()
for(i in years[1]:years[2]){
  pet_file <- file.path(phenofitdata_folder, paste0(year, "BP"), paste0("HadCM3B_", "pet", "_", i, "_dly.fit"))
  pet <- fread(pet_file, showProgress=F)
  names(pet)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(pet) == 368){
    # leap year
    start <- start + 1
    end <- end +1 
  }
  yearly_pet <- pet %>%
    summarise(pet = dplyr::select(., (start+2):(end+2)) %>% rowSums(na.rm = F)) #+2 beacause of lat&lon columns
  pre_file <- file.path(phenofitdata_folder, paste0(year, "BP"), paste0("HadCM3B_", "pre", "_", i, "_dly.fit"))
  pre <- fread(pre_file, showProgress=F)
  names(pre)[1:2] <- c("lat", "lon")
  yearly_pre <- pre %>%
    summarise(pre = dplyr::select(., (start+2):(end+2)) %>% rowSums(na.rm = F)) #+2 beacause of lat&lon columns
  water_bal_all <- c(water_bal_all, yearly_pre - yearly_pet)
}
water_bal_30y <- abind(water_bal_all, along = 2)
water_bal_30y <- apply(water_bal_30y, 1, mean)
rm(pet, yearly_pet, yearly_pre, water_bal_all)


predictors_data <- cbind(biovars_30y, sum_apsep_GDD5 = sumGDD5_apsep_30y, w_bal = water_bal_30y)

# soil predictors
r_res <- rast(predictors_data[,c("lon", "lat", "bio6")])
soil_pred <- mask(terra::resample(soil_pred, r_res, method = "average"), r_res)
soil_pred <- mask(focal(soil_pred, w = 3, fun = "mean", na.policy ="only"), r_res)
soil_pred <- mask(focal(soil_pred, w = 3, fun = "mean", na.policy ="only"), r_res)
soil_pred <- mask(focal(soil_pred, w = 3, fun = "mean", na.policy ="only"),r_res)
soil_pred <- mask(focal(soil_pred, w = 3, fun = "mean", na.policy ="only"),r_res)
soil_pred <- mask(focal(soil_pred, w = 3, fun = "mean", na.policy ="only"),r_res)
soil_predictors <- round(as.data.frame(soil_pred, xy = T),2)
names(soil_predictors)[1:2] <- c("lon", "lat")

predictors_data <- inner_join(predictors_data, soil_predictors)
saveRDS(predictors_data, file = file.path(csdm_folder, paste0("predictors_", year, "BP.rds")))

