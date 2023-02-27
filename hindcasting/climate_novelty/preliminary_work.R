###############################
# Mapping climatic mechanisms #
###############################

library(raster)
library(data.table)

# Preliminary work #

## 0. Loading data

### present data
present_climate_dir <- "D:/climate/ERA5-Land/phenofit_format/transformed"
present_temp <- fread(file.path(present_climate_dir, "ERA5LAND_tmp_1970_dly.fit"))
present_temp <- data.frame(lon = present_temp[,2], lat = present_temp[,1], tas = rowMeans(present_temp[,-c(1,2)]))
present_temp <- rasterFromXYZ(present_temp)
present_pre <- fread(file.path(present_climate_dir, "ERA5LAND_pre_1970_dly.fit"))
present_pre <- data.frame(lon = present_pre[,2], lat = present_pre[,1], pre = rowSums(present_pre[,-c(1,2)]))
present_pre <- rasterFromXYZ(present_pre)
for(i in 1971:2000){
  
  print(i)
  present_temp_i <- fread(file.path(present_climate_dir, paste0("ERA5LAND_tmp_",i,"_dly.fit")))
  present_temp_i <- data.frame(lon = present_temp_i[,2], lat = present_temp_i[,1], tas = rowMeans(present_temp_i[,-c(1,2)]))
  present_temp_i <- rasterFromXYZ(present_temp_i)
  present_pre_i <- fread(file.path(present_climate_dir, paste0("ERA5LAND_pre_",i,"_dly.fit")))
  present_pre_i <- data.frame(lon = present_pre_i[,2], lat = present_pre_i[,1], pre = rowSums(present_pre_i[,-c(1,2)]))
  present_pre_i <- rasterFromXYZ(present_pre_i)
  
  present_temp <- stack(present_temp, present_temp_i)
  present_pre <- stack(present_pre, present_pre_i)
 
}
present_temp_mean <- mean(present_temp)
present_pre_mean <- mean(present_pre)


### past data (use here as baseline conditions)
past_climate_dir <- "D:/climate/HadCM3B_60Kyr_Climate/biasregrid_highres"
past_temp_all <- raster::stack(file.path(past_climate_dir, "bias_regrid_tas_highres_0_2.5kyr.nc"))
past_temp <- subset(past_temp_all, 17413:23412) # keep only 50 decades prior to 1950 BP
# past_temp <- subset(past_temp, 23221:23592) # keep only 31 years around 1950BP (0 A.D.)
past_temp <- crop(past_temp, present_temp) # only europe
indices<-rep(1:500,each=12)
past_temp_annual_mean <- stackApply(past_temp-273.15, indices, fun = mean) # annual mean temperature
past_temp_annual_var <- calc(past_temp_annual_mean, fun = var) # interannual variability of mean temperature
# past_temp_mean <- mean(past_temp_annual_mean) # mean temperature over 31 years
# past_temp_mean <- resample(past_temp_mean, present_temp) # downscaling by bilinear interpolation
# past_temp_mean <- mask(past_temp_mean, present_temp) # keep same land cells
# past_temp_annual_var <- resample(past_temp_annual_var, present_temp) # downscaling by bilinear interpolation
# past_temp_annual_var <- mask(past_temp_annual_var, present_temp) # keep same land cells

past_pre_all <- raster::stack(file.path(past_climate_dir, "bias_regrid_pr_highres_0_2.5kyr.nc"))
past_pre <- subset(past_pre_all, 17413:23412) # keep only 50 decades prior to 1950 BP
# past_pre <- subset(past_pre, 23221:23592) # keep only 31 years around 1950BP (0 A.D.) => climate novelty
past_pre <- crop(past_pre, present_temp) # only europe
indices<-rep(1:500,each=12)
past_pre_annual_sum <- stackApply(past_pre*30, indices, fun = sum, na.rm=FALSE) # annual sum of precipitation
past_pre_annual_var <- calc(past_pre_annual_sum, fun = var) # interannual variability of mean temperature
# past_pre_mean <- mean(past_pre_annual_sum) # mean temperature over 31 years
# past_pre_mean <- resample(past_pre_mean, present_temp) # downscaling by bilinear interpolation
# past_pre_mean <- mask(past_pre_mean, present_temp) # keep same land cells
# past_pre_annual_var <- resample(past_pre_annual_var, present_temp) # downscaling by bilinear interpolation
# past_pre_annual_var <- mask(past_pre_annual_var, present_temp) # keep same land cells

### stacking
past_temp_mean <- mean(past_temp_annual_mean)
past_pre_mean <- mean(past_pre_annual_sum)
past_data <- stack(past_temp_mean, past_pre_mean)
past_interannual_var <- stack(past_temp_annual_var, past_pre_annual_var)
present_data <- stack(present_temp_mean, present_pre_mean)



## 1. Climate dissimilarity 

### 1.1. Ordonnez et al.: minimum euclidean distance
climate_eucl_diss <- climatic_dissimilarity(focal_stack = present_data, baseline_stack = past_data, 
                                            method = "std_euclidean",
                                            interannual_var_stack = past_interannual_var)


climate_euc_diss_df <- as.data.frame(climate_eucl_diss, xy = T)
ggplot(data = climate_euc_diss_df, aes(x = x, y = y, fill = layer.1)) +
  geom_raster() +
  theme_void() +
  labs(fill = "Minimum standardized Euclidean distance") +
  scale_fill_gradientn(colours = c("#247BA0","#70C1B3","#FFE066","#EE3F3C"), 
                       values = scales::rescale(c(0, 1.168768, 1.168769, 6.06)),
                       breaks = c(0.01, 1.1687, 2, 3, 4, 5, 6),
                       na.value = NA) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.5,
                               ticks = FALSE)) +
  theme(legend.title.align = 0.5, legend.position = "bottom", 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(2.5, 'cm'),
        legend.title = element_text(size = 9))







## 2. Climate displacement and divergence

past_pre <- subset(past_pre_all, 17413:23412) # keep only 50 decades prior to 1950 BP
past_pre <- crop(past_pre, present_temp) # only europe
indices<-rep(1:500,each=12)
past_pre_annual_sum <- stackApply(past_pre*30, indices, fun = sum, na.rm=FALSE) # annual sum of precipitation
past_pre_annual_sum <- resample(past_pre_annual_sum, present_temp) # downscaling by bilinear interpolation
past_pre_annual_sum <- mask(past_pre_annual_sum, subset(present_temp,1)) # keep same land cells

past_temp <- subset(past_temp_all, 17413:23412) # keep only 50 decades prior to 1950 BP
past_temp <- crop(past_temp, present_temp) # only europe
indices<-rep(1:500,each=12)
past_temp_annual_mean <- stackApply(past_temp-273.15, indices, fun = mean) # annual mean temperature
past_temp_annual_mean <- resample(past_temp_annual_mean, present_temp) # downscaling by bilinear interpolation
past_temp_annual_mean <- mask(past_temp_annual_mean, subset(present_temp,1)) # keep same land cells


### 2.1. Local temporal rate of change
test_past_temp_annual_mean <- crop(past_temp_annual_mean, extent(c(-10,30, 35,60))) #reduced size
test_past_pre_annual_sum <- crop(past_pre_annual_sum, extent(c(-10,30, 35,60))) #reduced size


local_rate_temp <- local_rate_of_change(test_past_temp_annual_mean)

local_rate_temp_df <- as.data.frame(local_rate_temp, xy = T)
ggplot(data = local_rate_temp_df, aes(x = x, y = y, fill = index_1)) +
  geom_raster() +
  theme_void() +
  labs(fill = "Temporal rate of change (temperature)") +
  scale_fill_gradientn(colours = c("#247BA0", "#FFE066","#EE3F3C"),
                       values = scales::rescale(c(-0.002, 0.000, 0.001)),
                       breaks = c(-0.001, 0, 0.001),
                       na.value = NA) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.5,
                               ticks = FALSE)) +
  theme(legend.title.align = 0.5, legend.position = "bottom", 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(2.5, 'cm'),
        legend.title = element_text(size = 9))

local_rate_pre <- local_rate_of_change(test_past_pre_annual_sum)
local_rate_pre_df <- as.data.frame(local_rate_pre, xy = T)
ggplot(data = local_rate_pre_df, aes(x = x, y = y, fill = index_1)) +
  geom_raster() +
  theme_void() +
  labs(fill = "Temporal rate of change (precipitation)") +
  scale_fill_gradientn(colours = c("#247BA0", "#FFE066","#EE3F3C"),
                       values = scales::rescale(c(-0.0007, 0.000, 0.001)),
                       breaks = c(-0.0006, 0.000, 0.0009),
                       na.value = NA) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.5,
                               ticks = FALSE)) +
  theme(legend.title.align = 0.5, legend.position = "bottom", 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(2.5, 'cm'),
        legend.title = element_text(size = 9))

# equidistant cylindrical projection
crs(local_rate_pre) <- crs("+init=epsg:4326")
local_rate_pre <- projectRaster(local_rate_pre, crs = crs("+init=epsg:4087"))
crs(local_rate_temp) <- crs("+init=epsg:4326")
local_rate_temp <- projectRaster(local_rate_temp, crs = crs("+init=epsg:4087"))


temp_angle <- temporal_angle(stack(local_rate_temp, local_rate_pre))


### 2.2. Spatial gradients
past_pre_annual_sum_1century <- subset(test_past_pre_annual_sum, 401:500) # keep only 10 decades prior to 1950 BP
mean_past_pre_1century <- mean(past_pre_annual_sum_1century) 
past_temp_annual_mean_1century <- subset(test_past_temp_annual_mean, 401:500) # keep only 10 decades prior to 1950 BP
mean_past_temp_1century <- mean(past_temp_annual_mean_1century) 

# equidistant cylindrical projection
crs(mean_past_temp_1century) <- crs("+init=epsg:4326")
mean_past_temp_1century_ecp <- projectRaster(mean_past_temp_1century, crs = crs("+init=epsg:4087"))
crs(mean_past_pre_1century) <- crs("+init=epsg:4326")
mean_past_pre_1century_ecp <- projectRaster(mean_past_pre_1century, crs = crs("+init=epsg:4087"))


pre_spatial_gradient <- spatial_gradient(mean_past_pre_1century_ecp , res = 11.1)
temp_spatial_gradient <- spatial_gradient(mean_past_temp_1century_ecp, res = 11.1)

spatial_angle <- abs(pre_spatial_gradient$angle-temp_spatial_gradient$angle)

### 2.3. Divergence 
divergence <- abs(spatial_angle - temp_angle)

### 2.4 Displacement
speed_temp_velocity_vec <- abs(local_rate_temp/temp_spatial_gradient$norm)*10 #per decade
speed_temp_velocity_vec[speed_temp_velocity_vec > 1e+14] <- NA
speed_pre_velocity_vec <- abs(local_rate_pre/pre_spatial_gradient$norm)*10 #per decade
speed_pre_velocity_vec[speed_pre_velocity_vec > 1e+12] <- NA
displacement <- mean(speed_temp_velocity_vec, speed_pre_velocity_vec)


displacement_df <- as.data.frame(displacement, xy = T)
ggplot(data = displacement_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  theme_void() +
  scale_fill_gradient2(trans = 'log10', breaks = c(0.01, 0.1, 1, 10), 
                       low = "#247BA0", mid = "#FFE066", high = "#EE3F3C",
                       na.value = NA) +
  labs(fill = "Displacement (km/decade)") +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.5,
                               ticks = FALSE)) +
  theme(legend.title.align = 0.5, legend.position = "bottom", 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(2.5, 'cm'),
        legend.title = element_text(size = 9))


