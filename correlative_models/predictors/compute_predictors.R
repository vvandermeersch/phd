#################################
# Computing predictors for cSDM #
#################################


library(data.table)
library(dplyr)
library(abind)

#--------------------#
# Bioclim predictors #
#--------------------#
bioclim_data_dir <- "D:/climate/ERA5-Land/bioclim_format_2"

# Mean of 31 years of bioclim data from ERA5-Land
biovars_all <- list()
for(i in as.character(1970:2000)){
  load(paste0(bioclim_data_dir, "/biovars_", i, ".Rdata"))
  biovars_all <- append(biovars_all, list(biovars))
}
biovars_30y <- abind(biovars_all, along=3)
biovars_30y <- apply(biovars_30y, c(1,2), mean)
biovars_30y <- as.data.frame(biovars_30y)
biovars_30y$lat <- round(biovars_30y$lat,1)
biovars_30y$lon <- round(biovars_30y$lon,1)
rm(biovars, biovars_all)

# Selecting bioclim variables
# temperature: mean (bio1), extremes (max: bio5, min: bio6), seasonality (bio4)
# precipitation: sum (bio12), extremes (max: bio13, min: bio14), seasonality (bio15)
bc_predictors <- c("bio1", "bio6", "bio5", "bio4", "bio12", "bio14", "bio13", "bio15")
bc_fullnames <- c("Annual mean temperature", "Min. temp. coldest month", "Max. temp. warmest month",  "Temperature seasonality", 
                  "Annual precipitation", "Prec. of driest month", "Prec. of wettest month", "Precipitation seasonality")

#-----------------#
# Soil predictors #
#-----------------#
soil_data_dir <- "D:/soil/processed"

# Loading data
load(file.path(soil_data_dir, "data_soil.Rdata"))

# Selecting soil variables
# soil properties: water holding capacity (WHC), soil bulk density (bld), pH
# nutrients: total nitrogen
soil_predictors <- c("WHC", "bld", "pH", "nitrogen", "carbon")
soil_fullnames <- c("Water holding cap.", "Bulk density", "pH", "Nitrogen", "Carbon")

#----------------------------#
# Custom climatic predictors #
#----------------------------#
clim_data_dir <- "D:/climate/ERA5-Land/phenofit_format/transformed"

# Mean water balance (precipitation - PET), between June and July
day_begin <- 152
day_end <- 212
water_bal_all <- c()
for(i in 1970:2000){
  pet_file <- paste0(clim_data_dir, "/ERA5LAND_", "pet", "_", i, "_dly.fit")
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
  pre_file <- paste0(clim_data_dir, "/ERA5LAND_", "pre", "_", i, "_dly.fit")
  pre <- fread(pre_file, showProgress=F)
  names(pre)[1:2] <- c("lat", "lon")
  yearly_pre <- pre %>%
    summarise(pre = dplyr::select(., (start+2):(end+2)) %>% rowSums(na.rm = F)) #+2 beacause of lat&lon columns
  water_bal_all <- c(water_bal_all, yearly_pre - yearly_pet)
}
water_bal_30y <- abind(water_bal_all, along = 2)
water_bal_30y <- apply(water_bal_30y, 1, mean)
rm(pet, yearly_pet, yearly_pre, water_bal_all)

# Mean GDD sum between Mars and October, threshold 0
day_begin <- 60
day_end <- 304
base_temp <- 0
sumGDD_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
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
sumGDD_30y <- matrix(sumGDD_all, nrow = nrow(tmean), byrow = F)
sumGDD_30y <- apply(sumGDD_30y, 1, mean)
rm(tmean, sumGDD_all)

# Mean GDD sum between Mars and October, threshold 5
day_begin <- 60
day_end <- 304
base_temp <- 5
sumGDD_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
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
sumGDD5_30y <- matrix(sumGDD_all, nrow = nrow(tmean), byrow = F)
sumGDD5_30y <- apply(sumGDD5_30y, 1, mean)
rm(tmean, sumGDD_all)

# Mean GDD sum between April and September, threshold 5
day_begin <- 91
day_end <- 273
base_temp <- 5
sumGDD_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
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


# Mean number of days with Tmean<10 between November and February
day_begin <- 305
day_end <- 59
ndays10_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
  tmean_n <- fread(tmean_file, showProgress = F)
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i-1, "_dly.fit")
  tmean_n1 <- fread(tmean_file, showProgress = F)
  names(tmean_n)[1:2] <- c("lat", "lon")
  names(tmean_n1)[1:2] <- c("lat", "lon")
  start <- day_begin
  if(ncol(tmean_n1) == 368){
    # leap year
    start <- start + 1
  }
  ndays10_n1 <- rowSums(tmean_n1[, (start+2):ncol(tmean_n1)] < 10) #+2 beacause of lat&lon columns
  end <- day_end
  if(ncol(tmean_n) == 368){
    # leap year
    end <- end + 1
  }
  ndays10_n <- rowSums(tmean_n[, 3:(end+2)] < 10) #+2 beacause of lat&lon columns
  ndays10_all <- c(ndays10_all, ndays10_n1 + ndays10_n)
}
ndays10_30y <- matrix(ndays10_all, nrow = nrow(tmean_n), byrow = F)
ndays10_30y <- apply(ndays10_30y, 1, mean)
rm(tmean_n, tmean_n1, ndays10_all)

# Mean number of days with Tmean<5 between November and February
day_begin <- 305
day_end <- 59
ndays5_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
  tmean_n <- fread(tmean_file, showProgress = F)
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i-1, "_dly.fit")
  tmean_n1 <- fread(tmean_file, showProgress = F)
  names(tmean_n)[1:2] <- c("lat", "lon")
  names(tmean_n1)[1:2] <- c("lat", "lon")
  start <- day_begin
  if(ncol(tmean_n1) == 368){
    # leap year
    start <- start + 1
  }
  ndays5_n1 <- rowSums(tmean_n1[, (start+2):ncol(tmean_n1)] < 5) #+2 beacause of lat&lon columns
  end <- day_end
  if(ncol(tmean_n) == 368){
    # leap year
    end <- end + 1
  }
  ndays5_n <- rowSums(tmean_n[, 3:(end+2)] < 5) #+2 beacause of lat&lon columns
  ndays5_all <- c(ndays5_all, ndays5_n1 + ndays5_n)
}
ndays5_30y <- matrix(ndays5_all, nrow = nrow(tmean_n), byrow = F)
ndays5_30y <- apply(ndays5_30y, 1, mean)
rm(tmean_n, tmean_n1, ndays5_all)

# Last day of frost (between January and July), very long to compute because of a shitty for-loop
day_begin <- 1
day_end <- 212
tfrost <- 0
lastdfrost_all <- c()
for(i in 1970:2000){
  tmin_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmn", "_", i, "_dly.fit")
  tmin <- fread(tmin_file, showProgress = F)
  names(tmin)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(tmin) == 368){
    # leap year
    start <- start + 1
    end <- end + 1
  }
  
  lastd_frost <- apply(tmin[, (start+2):(end+2)],1, function(l){
    dmax <- 0
    for(i in 1:length(l)){
      if(l[i] < tfrost){
        dmax <- i
      }
    }
    return(dmax) 
  })
  lastdfrost_all <- c(lastdfrost_all, lastd_frost)
}
lastdfrost_30y <- matrix(lastdfrost_all, nrow = nrow(tmin), byrow = F)
lastdfrost_30y <- apply(lastdfrost_30y, 1, mean)

ccvars_30y <- data.frame(lat = tmin$lat, lon = tmin$lon, sum_GDD = sumGDD_30y, nd_10deg = ndays10_30y, 
                         lastd_frost = lastdfrost_30y, w_bal = water_bal_30y)
rm(tmin, lastdfrost_all)

# Mean number of days with Tmin<-5 between April and May
day_begin <- 91
day_end <- 151
ndaysinf5_all <- c()
for(i in 1970:2000){
  tmin_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmn", "_", i, "_dly.fit")
  tmin_n <- fread(tmin_file, showProgress = F)
  names(tmin_n)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(tmin_n) == 368){
    # leap year
    start <- start + 1
    end <- end + 1
  }
  ndaysinf5_n <- rowSums(tmin_n[, (start+2):(end+2)] < -5) #+2 beacause of lat&lon columns
  ndaysinf5_all <- c(ndaysinf5_all, ndaysinf5_n)
}
ndaysinf5_30y <- matrix(ndaysinf5_all, nrow = nrow(tmin_n), byrow = F)
ndaysinf5_30y <- apply(ndaysinf5_30y, 1, mean)
rm(tmin_n, ndaysinf5_all)

# Mean number of days with Tmin<0 between April and May
day_begin <- 91
day_end <- 151
ndaysinf0_all <- c()
for(i in 1970:2000){
  tmin_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmn", "_", i, "_dly.fit")
  tmin_n <- fread(tmin_file, showProgress = F)
  names(tmin_n)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(tmin_n) == 368){
    # leap year
    start <- start + 1
    end <- end + 1
  }
  ndaysinf0_n <- rowSums(tmin_n[, (start+2):(end+2)] < 0) #+2 beacause of lat&lon columns
  ndaysinf0_all <- c(ndaysinf0_all, ndaysinf0_n)
}
ndaysinf0_30y <- matrix(ndaysinf0_all, nrow = nrow(tmin_n), byrow = F)
ndaysinf0_30y <- apply(ndaysinf0_30y, 1, mean)
rm(tmin_n, ndaysinf0_all)

# Mean number of days with Tmin<-2 between April and May
day_begin <- 91
day_end <- 151
ndaysinf2_all <- c()
for(i in 1970:2000){
  tmin_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmn", "_", i, "_dly.fit")
  tmin_n <- fread(tmin_file, showProgress = F)
  names(tmin_n)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(tmin_n) == 368){
    # leap year
    start <- start + 1
    end <- end + 1
  }
  ndaysinf2_n <- rowSums(tmin_n[, (start+2):(end+2)] < -2) #+2 beacause of lat&lon columns
  ndaysinf2_all <- c(ndaysinf2_all, ndaysinf2_n)
}
ndaysinf2_30y <- matrix(ndaysinf2_all, nrow = nrow(tmin_n), byrow = F)
ndaysinf2_30y <- apply(ndaysinf2_30y, 1, mean)
rm(tmin_n, ndaysinf2_all)

# Mean GDD sum between Mars and May
day_begin <- 60
day_end <- 151
base_temp <- 0
sumGDD_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
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
sumGDD_leaf_30y <- matrix(sumGDD_all, nrow = nrow(tmean), byrow = F)
sumGDD_leaf_30y <- apply(sumGDD_leaf_30y, 1, mean)
rm(tmean, sumGDD_all)

# Mean GDD sum between June and September
day_begin <- 152
day_end <- 273
base_temp <- 0
sumGDD_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
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
sumGDD_fruit_30y <- matrix(sumGDD_all, nrow = nrow(tmean), byrow = F)
sumGDD_fruit_30y <- apply(sumGDD_fruit_30y, 1, mean)
rm(tmean, sumGDD_all)

# Mean number of days with Tmean>10 between Mars and October
day_begin <- 60
day_end <- 304
ndayssup10_all <- c()
for(i in 1970:2000){
  tmean_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmp", "_", i, "_dly.fit")
  tmean_n <- fread(tmean_file, showProgress = F)
  names(tmean_n)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(tmean_n) == 368){
    # leap year
    start <- start + 1
    end <- end + 1
  }
  ndayssup10_n <- rowSums(tmean_n[, (start+2):(end+2)] > 10) #+2 because of lat&lon columns
  ndayssup10_all <- c(ndayssup10_all, ndayssup10_n)
}
ndayssup10_30y <- matrix(ndayssup10_all, nrow = nrow(tmean_n), byrow = F)
ndayssup10_30y <- apply(ndayssup10_30y, 1, mean)
rm(tmean_n, ndayssup10_all)

# Mean number of days with Tmin<0 in summer (june-august) = days of summer frost
day_begin <- 152
day_end <- 243
ndaysinf0_all <- c()
for(i in 1970:2000){
  tmin_file <- paste0(clim_data_dir, "/ERA5LAND_", "tmn", "_", i, "_dly.fit")
  tmin_n <- fread(tmin_file, showProgress = F)
  names(tmin_n)[1:2] <- c("lat", "lon")
  start <- day_begin
  end <- day_end
  if(ncol(tmin_n) == 368){
    # leap year
    start <- start + 1
    end <- end + 1
  }
  ndaysinf0_n <- rowSums(tmin_n[, (start+2):(end+2)] < 0) #+2 beacause of lat&lon columns
  ndaysinf0_all <- c(ndaysinf0_all, ndaysinf0_n)
}
ndayssummerfrost_30y <- matrix(ndaysinf0_all, nrow = nrow(tmin_n), byrow = F)
ndayssummerfrost_30y <- apply(ndayssummerfrost_30y, 1, mean)
rm(tmin_n, ndaysinf0_all)



# Selecting custom climatic variables
# temperature: mean number of frost days (nd_frost)
# water availability: water balance, i.e. precipitation - evapotranspiration (w_bal)
cc_predictors <- c("sum_GDD", "sum_GDD5", "sum_leaf_GDD", "sum_fruit_GDD", "ndayssup10_30y",
                   "nd_10deg", "nd_5deg", 
                   "nd_neg5deg", "nd_neg0deg", 
                   "lastd_frost",  "w_bal")
cc_fullnames <- c("Sum of growing degree days (Mar-Oct)", "Sum of growing degree days, th5 (Mar-Oct)",
                  "Sum of growing degree days (Mar-May)", "Sum of growing degree days (Jun-Sep)", 
                  "Number of days with Tmean>10C (Mar-Oct)",
                  "Number of days with Tmean<10C (Nov-Feb)", "Number of days with Tmean<5C (Nov-Feb)",
                  "Number of days with Tmin<-5C (April-May)", "Number of days with Tmin<0C (April-May)",
                  "Last frost day (Jan-Jul)","Water balance (Jun-Jul)")

predictors_data$sum_GDD5 <- sumGDD5_30y
predictors_data$sum_apsep_GDD5 <- sumGDD5_apsep_30y
predictors_data$sum_leaf_GDD <- sumGDD_leaf_30y
predictors_data$sum_fruit_GDD <- sumGDD_fruit_30y
predictors_data$nd_5deg <- ndays5_30y
predictors_data$nd_sup10deg <- ndayssup10_30y
predictors_data$nd_neg5deg <- ndaysinf5_30y
predictors_data$nd_neg0deg <- ndaysinf0_30y
predictors_data$nd_neg2deg <- ndaysinf2_30y
predictors_data$nd_summerfrost <- ndayssummerfrost_30y
save(predictors_data, file = "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors/predictors_data.Rdata")
