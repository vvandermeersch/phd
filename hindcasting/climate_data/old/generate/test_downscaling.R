# inspired by Copernicus Climate Change Service (CCCS)

library("doFuture")
library(raster)
library(data.table)
registerDoFuture()
plan(multisession, workers = 20)


clim_dir <- "D:/climate/ERA5-Land/phenofit_format/transformed"

alt <- fread(file.path(clim_dir, "ERA5LAND_Altitude.fit"))
alt <- rasterFromXYZ(alt[, c(2,1,3)])

# dummy high-resolution altitude
hr_alt <- alt 
res(hr_alt) <- c(0.05, 0.05)
hr_alt <- resample(alt, hr_alt)

max_temp <- data.frame(fread(file.path(clim_dir, "ERA5LAND_tmx_1969_dly.fit")))
min_temp <- data.frame(fread(file.path(clim_dir, "ERA5LAND_tmn_1969_dly.fit")))

max_regd <- alt
min_regd <- alt
max_reg <- stack()
min_reg <- stack()

# 1. quantify the change with height at the native coarse resolution
for(day in 3:ncol(max_temp)){ # sequential loop on days
  
  max_tempd <- rasterFromXYZ(max_temp[, c(2,1,day)])
  min_tempd <- rasterFromXYZ(min_temp[, c(2,1,day)])
  
  
  coef_linreg <- foreach(fcell = 1:ncell(max_tempd)) %dopar% { # parallel loop on raster cells
    if(!is.na(max_tempd[fcell])){
      ind_ncells <- adjacent(max_tempd, fcell, 8, include = T)[,2]
      max_linreg <- lm(max_tempd[ind_ncells] ~ alt[ind_ncells]) # linear regression for maximum temperature
      min_linreg <- lm(min_tempd[ind_ncells] ~ alt[ind_ncells]) # and minimum temperature
      c(as.numeric(max_linreg$coefficients[2]), as.numeric(min_linreg$coefficients[2]))
    }else{
      c(NA, NA)
    }
  }
  coef_linreg <- do.call("rbind", coef_linreg)
  max_regd[] <- coef_linreg[,1]
  min_regd[] <- coef_linreg[,2]
  max_reg <- stack(max_reg, max_regd)
  min_reg <- stack(min_reg, min_regd)
}

# 2. interpolate the slope to the high-resolution grid
hr_max_reg <- resample(max_reg, hr_alt)
hr_min_reg <- resample(min_reg, hr_alt)

# 3. interpolate the coarse variable to the high-resolution
max_temp_rast <- stack(lapply(3:ncol(max_temp), function(day) rasterFromXYZ(max_temp[, c(2,1,day)])))
min_temp_rast <- stack(lapply(3:ncol(min_temp), function(day) rasterFromXYZ(min_temp[, c(2,1,day)])))
hr_max_temp <- resample(max_temp_rast, hr_alt)
hr_min_temp <- resample(min_temp_rast, hr_alt)

# 4. create coarse elevation field at high resolution
coarse_alt_hr <- resample(alt, hr_alt, method = "ngb")

# 5. apply height correction 
hr_max_temp_cor <- hr_max_temp + hr_max_reg * (hr_alt - coarse_alt_hr)

