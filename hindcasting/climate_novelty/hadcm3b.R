###############################
# Mapping climatic mechanisms #
###############################

library(terra)
library(data.table)
library(dplyr)
library(future.apply)

# functions
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/functions/years_functions.r")
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/functions/load_altitude_icesheet.R")

# HadCM3B #
data_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"
temp_fd <- "temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"
pre_fd <- "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"

### HadCM3B present data as baseline conditions
years <- c(0, 30)
file_spec <- years_to_file(years)
rmonths <- year_to_months(years[1], years[2], file_spec$max)
r_temp <- rast(file.path(data_dir, temp_fd, 
                         paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
               subds = "temp_mm_1_5m", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO")
r_pre <- rast(file.path(data_dir, pre_fd, 
                        paste0("precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
              subds = "precip_mm_srf", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO")
indices<-rep(0:30,each=12)
r_temp_yrmn <- tapp(r_temp, indices, fun = mean, na.rm=FALSE) # yearly mean temperature
r_pre_sm <- tapp(r_pre*30, indices, fun = sum, na.rm=FALSE) # yearly sum of precipitation
extent <- ext(c(-14,40,34,72))
r_temp_sd <- crop(app(r_temp_yrmn, "sd"), extent)
r_pre_sd <- crop(app(r_pre_sm, "sd"), extent) 
r_temp_mn <- crop(mean(r_temp_yrmn), extent)
r_pre_mn <- crop(mean(r_pre_sm), extent)
present_data <- c(r_temp_mn, r_pre_mn )
present_sd <- c(r_temp_sd, r_pre_sd)

.present_data <- wrap(present_data)
.present_sd <- wrap(present_sd)


### ERA5-Land present data as baseline conditions (// calibration)
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



### Past data and minimum euclidean distance
past_sed <- list()
years <- seq(21000,200,-200)
plan(multisession, workers = 20)
past_sed <- future_lapply(1:length(years), function(i){
  present_data <- rast(.present_data)
  present_sd <- rast(.present_sd)
  
  extent <- ext(c(-14,30,34,71))
  yr <- years[i]
  cat(paste0("Doing year ", yr, "...\n"))
  
  r_temp <- load_raster(yr, "temp", data_dir, wnd = 15)
  r_pre <- load_raster(yr, "pre", data_dir, wnd = 15)
  
  indices<-rep(0:30,each=12)
  r_temp_yrmn <- tapp(r_temp, indices, fun = mean, na.rm=FALSE) # yearly mean temperature
  r_pre_sm <- tapp(r_pre*30, indices, fun = sum, na.rm=FALSE) # yearly sum of precipitation
  r_temp_mn <- crop(mean(r_temp_yrmn), extent)
  r_pre_mn <- crop(mean(r_pre_sm), extent)
  past_data <- c(r_temp_mn, r_pre_mn )
  
  # mask with ice sheet
  ice_sht <- rast(load_icesheet(yr, folder = data_dir, extent = extent))
  ice_sht[ice_sht > 0.1] <- NA
  crs(ice_sht) <- crs(past_data)
  
  past_data <- mask(past_data, ice_sht)
  
  r_sed <- climatic_dissimilarity(focal = past_data, baseline = present_data, 
                                  method = "std_euclidean",
                                  interannual_sd = present_sd)
  return(as.data.frame(r_sed, xy = T)) # return a dataframe to avoid problem of external pointer
})

past_sed <- lapply(past_sed, rast)
names(past_sed) <- as.character(seq(21000,1000,-200))
mean_sed <- sapply(past_sed, function(i) global(i, mean, na.rm = T)$mean)
median_sed <- sapply(past_sed, function(i) global(i, median, na.rm = T)$global)
q25_sed <- sapply(past_sed, function(i) global(i, quantile, na.rm = T)$X25.)
q75_sed <- sapply(past_sed, function(i) global(i, quantile, na.rm = T)$X75.)
sed_df <- data.frame(year = as.numeric(names(mean_sed)), mean = mean_sed, 
                     median = median_sed, q25 = q25_sed, q75 = q75_sed)
ggplot(data = sed_df, aes(x = year, y = median)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#94d2bd", alpha = 0.5) +
  geom_line(aes(y = q75), color = "#0a9396", size = 0.3, alpha = 0.6) +
  geom_line(aes(y = q25), color = "#0a9396", size = 0.3, alpha = 0.6) +
  geom_line(col = "#0a9396", size = 1) +
  scale_y_continuous(limits = c(0,1.5), expand = c(0, 0)) +
  scale_x_reverse(expand = c(0.01, 0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey90", size = 0.7, linetype = "solid"),
        axis.line.x = element_line(colour = "grey90", size = 0.7, linetype = "solid"),
        axis.ticks.length=unit(.2, "cm")) +
  xlab("Years BP") +
  ylab("Climatic distance")
  
