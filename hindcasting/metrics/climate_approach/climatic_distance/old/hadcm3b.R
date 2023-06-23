###############################
# Mapping climatic mechanisms #
###############################

library(terra)
library(data.table)
library(dplyr)
library(future.apply)
library(pracma)

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
extent <- ext(c(-14,30,34,71))
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
present_temp_DJF <- data.frame(lon = present_temp[,2], lat = present_temp[,1], tas = rowMeans(present_temp[,c(3:61,337:365)]))
present_temp_MAM <- data.frame(lon = present_temp[,2], lat = present_temp[,1], tas = rowMeans(present_temp[,c(62:153)]))
present_temp_JJA <- data.frame(lon = present_temp[,2], lat = present_temp[,1], tas = rowMeans(present_temp[,c(154:245)]))
present_temp_SON <- data.frame(lon = present_temp[,2], lat = present_temp[,1], tas = rowMeans(present_temp[,c(247:336)]))
present_temp_DJF <- rast(present_temp_DJF)
present_temp_MAM <- rast(present_temp_MAM)
present_temp_JJA <- rast(present_temp_JJA)
present_temp_SON <- rast(present_temp_SON)
present_pre <- fread(file.path(present_climate_dir, "ERA5LAND_pre_1970_dly.fit"))
present_pre_DJF <- data.frame(lon = present_pre[,2], lat = present_pre[,1], pre = rowSums(present_pre[,c(3:61,337:365)]))
present_pre_MAM <- data.frame(lon = present_pre[,2], lat = present_pre[,1], pre = rowSums(present_pre[,c(62:153)]))
present_pre_JJA <- data.frame(lon = present_pre[,2], lat = present_pre[,1], pre = rowSums(present_pre[,c(154:245)]))
present_pre_SON <- data.frame(lon = present_pre[,2], lat = present_pre[,1], pre = rowSums(present_pre[,c(247:336)]))
present_pre_DJF <- rast(present_pre_DJF)
present_pre_MAM <- rast(present_pre_MAM)
present_pre_JJA <- rast(present_pre_JJA)
present_pre_SON <- rast(present_pre_SON)
for(i in 1971:2000){
  
  print(i)
  present_temp_i <- fread(file.path(present_climate_dir, paste0("ERA5LAND_tmp_",i,"_dly.fit")))
  present_temp_DJF_i <- data.frame(lon = present_temp_i[,2], lat = present_temp_i[,1], tas = rowMeans(present_temp_i[,c(3:61,337:365)]))
  present_temp_MAM_i <- data.frame(lon = present_temp_i[,2], lat = present_temp_i[,1], tas = rowMeans(present_temp_i[,c(62:153)]))
  present_temp_JJA_i <- data.frame(lon = present_temp_i[,2], lat = present_temp_i[,1], tas = rowMeans(present_temp_i[,c(154:245)]))
  present_temp_SON_i <- data.frame(lon = present_temp_i[,2], lat = present_temp_i[,1], tas = rowMeans(present_temp_i[,c(247:336)]))
  present_temp_DJF_i <- rast(present_temp_DJF_i)
  present_temp_MAM_i <- rast(present_temp_MAM_i)
  present_temp_JJA_i <- rast(present_temp_JJA_i)
  present_temp_SON_i <- rast(present_temp_SON_i)
  present_pre_i <- fread(file.path(present_climate_dir, paste0("ERA5LAND_pre_",i,"_dly.fit")))
  present_pre_DJF_i <- data.frame(lon = present_pre_i[,2], lat = present_pre_i[,1], pre = rowSums(present_pre_i[,c(3:61,337:365)]))
  present_pre_MAM_i <- data.frame(lon = present_pre_i[,2], lat = present_pre_i[,1], pre = rowSums(present_pre_i[,c(62:153)]))
  present_pre_JJA_i <- data.frame(lon = present_pre_i[,2], lat = present_pre_i[,1], pre = rowSums(present_pre_i[,c(154:245)]))
  present_pre_SON_i <- data.frame(lon = present_pre_i[,2], lat = present_pre_i[,1], pre = rowSums(present_pre_i[,c(247:336)]))
  present_pre_DJF_i <- rast(present_pre_DJF_i)
  present_pre_MAM_i <- rast(present_pre_MAM_i)
  present_pre_JJA_i <- rast(present_pre_JJA_i)
  present_pre_SON_i <- rast(present_pre_SON_i)
  
  present_temp_DJF <- c(present_temp_DJF, present_temp_DJF_i)
  present_temp_MAM <- c(present_temp_MAM, present_temp_MAM_i)
  present_temp_JJA <- c(present_temp_JJA, present_temp_JJA_i)
  present_temp_SON <- c(present_temp_SON, present_temp_SON_i)
  present_pre_DJF <- c(present_pre_DJF, present_pre_DJF_i)
  present_pre_MAM <- c(present_pre_MAM, present_pre_MAM_i)
  present_pre_JJA <- c(present_pre_JJA, present_pre_JJA_i)
  present_pre_SON <- c(present_pre_SON, present_pre_SON_i)
  
}
extent <- ext(c(-14,30,34,71))
r_temp_sd_DJF <- crop(app(present_temp_DJF, "sd"), extent)
r_temp_sd_MAM <- crop(app(present_temp_MAM, "sd"), extent)
r_temp_sd_JJA <- crop(app(present_temp_JJA, "sd"), extent)
r_temp_sd_SON <- crop(app(present_temp_SON, "sd"), extent)
r_pre_sd_DJF <- crop(app(present_pre_DJF, "sd"), extent)
r_pre_sd_MAM <- crop(app(present_pre_MAM, "sd"), extent)
r_pre_sd_JJA <- crop(app(present_pre_JJA, "sd"), extent)
r_pre_sd_SON <- crop(app(present_pre_SON, "sd"), extent)
r_temp_mn_DJF <- crop(mean(present_temp_DJF), extent)
r_temp_mn_MAM <- crop(mean(present_temp_MAM), extent)
r_temp_mn_JJA <- crop(mean(present_temp_JJA), extent)
r_temp_mn_SON <- crop(mean(present_temp_SON), extent)
r_pre_mn_DJF <- crop(mean(present_pre_DJF), extent)
r_pre_mn_MAM <- crop(mean(present_pre_MAM), extent)
r_pre_mn_JJA <- crop(mean(present_pre_JJA), extent)
r_pre_mn_SON <- crop(mean(present_pre_SON), extent)
present_ERA5Land_data <- c(r_temp_mn_DJF, r_temp_mn_MAM, r_temp_mn_JJA, r_temp_mn_SON, 
                           r_pre_mn_DJF, r_pre_mn_MAM, r_pre_mn_JJA, r_pre_mn_SON)
present_ERA5Land_sd <- c(r_temp_sd_DJF, r_temp_sd_MAM, r_temp_sd_JJA, r_temp_sd_SON,
                         r_pre_sd_DJF, r_pre_sd_MAM, r_pre_sd_JJA, r_pre_sd_SON)
present_ERA5Land_data_agg <- terra::aggregate(present_ERA5Land_data, fact= 5, fun = mean, na.rm = T)
.present_ERA5Land_data <- wrap(present_ERA5Land_data)
.present_ERA5Land_data_agg <- wrap(present_ERA5Land_data_agg)
.present_ERA5Land_sd <- wrap(present_ERA5Land_sd)


### Past data and minimum climatic distance (here : mahalanobis)
past_climdist <- list()
years <- seq(21000,500,-500)
plan(multisession, workers = 10)
past_climdist <- future_lapply(1:length(years), function(i){
  present_ERA5Land_data <- rast(.present_ERA5Land_data)
  present_ERA5Land_data_agg <- rast(.present_ERA5Land_data_agg)
  present_ERA5Land_sd <- rast(.present_ERA5Land_sd)
  
  extent <- ext(c(-14,30,34,71))
  yr <- years[i]
  cat(paste0("Doing year ", yr, "...\n"))
  
  r_temp <- load_raster(yr, "temp", data_dir, wnd = 15)
  r_pre <- load_raster(yr, "pre", data_dir, wnd = 15)
  
  indices<- unlist(lapply(0:30, function(i) c(1+4*i, 1+4*i, 2+4*i, 2+4*i, 2+4*i, 3+4*i, 3+4*i, 3+4*i, 4+4*i, 4+4*i, 4+4*i, 1+4*i)))
  r_temp_yrmn <- tapp(r_temp, indices, fun = mean, na.rm=FALSE) # 3-month mean temperature
  r_pre_sm <- tapp(r_pre*30, indices, fun = sum, na.rm=FALSE) # 3-month sum of precipitation
  indices<-rep(1:4,31)
  r_temp_mn <- crop(tapp(r_temp_yrmn, indices, fun = mean, na.rm=FALSE), extent) # 31-year mean
  r_pre_mn <- crop(tapp(r_pre_sm, indices, fun = mean, na.rm=FALSE), extent) # 31-year mean
  past_data <- c(r_temp_mn, r_pre_mn )
  
  # mask with ice sheet
  ice_sht <- rast(load_icesheet(yr, folder = data_dir, extent = extent))
  ice_sht[ice_sht > 0.5] <- NA # restrict analyses to grid locations with less than 50% ice cover (Burke et al. 2019)
  crs(ice_sht) <- crs(past_data)
  
  past_data <- mask(past_data, ice_sht)
  
  # r_climdist <- climatic_dissimilarity(focal = past_data, baseline = present_ERA5Land_data, 
  #                                 method = "std_euclidean",
  #                                 interannual_sd = present_ERA5Land_sd)
  
  r_climdist <- climatic_dissimilarity(focal = past_data, baseline = present_ERA5Land_data_agg,
                                  method = "mahalanobis")
  
  return(as.data.frame(r_climdist, xy = T)) # return a dataframe to avoid problem of external pointer
})

past_climdist <- lapply(past_climdist, rast)
names(past_climdist) <- as.character(c(seq(21000,500,-500), 15))
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
  scale_y_continuous(limits = c(0.5,3), expand = c(0, 0)) +
  scale_x_reverse(expand = c(0.01, 0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey90", size = 0.7, linetype = "solid"),
        axis.line.x = element_line(colour = "grey90", size = 0.7, linetype = "solid"),
        axis.ticks.length=unit(.2, "cm")) +
  xlab("Years BP") +
  ylab("Climatic distance")


ggplot(data = climdist_df, aes(x = year, y = mean)) +
  geom_line(col = "#0a9396", linewidth = 1) +
  scale_y_continuous(limits = c(0,1.8), expand = c(0, 0)) +
  scale_x_reverse(expand = c(0.01, 0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey90", size = 0.7, linetype = "solid"),
        axis.line.x = element_line(colour = "grey90", size = 0.7, linetype = "solid"),
        axis.ticks.length=unit(.2, "cm")) +
  xlab("Years BP") +
  ylab("Climatic distance") 
