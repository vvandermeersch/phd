#############################
# Climate from 1900 to 2000 #
#############################

# Compare ERA5-Land and HadCM3B 

library(terra)
library(data.table)
library(dplyr)
library(future.apply)
library(zoo)

# functions
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/functions/years_functions.r")
extent <- ext(c(-14,40,34,72))
extent <- ext(c(-5,10,42,52))


# HadCM3B #
# data from 1900 to 1950
data_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"
temp_fd <- "temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"
pre_fd <- "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"

years <- c(0, 50) # 1950-1900
file_spec <- years_to_file(years)
rmonths <- year_to_months(years[1], years[2], file_spec$max)
r_temp_had <- crop(rast(file.path(data_dir, temp_fd, 
                         paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
               subds = "temp_mm_1_5m", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
r_pre_had <- crop(rast(file.path(data_dir, pre_fd, 
                        paste0("precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
              subds = "precip_mm_srf", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO"), extent)
indices<-rep(0:50,each=12)
r_temp_yrmn <- tapp(r_temp_had, indices, fun = mean, na.rm=FALSE) # yearly mean temperature
r_pre_sm <- tapp(r_pre_had*30, indices, fun = sum, na.rm=FALSE) # yearly sum of precipitation



hadcm3b_data <- data.frame('year' = 1900:1950,
  'temp' = unlist(global(r_temp_yrmn, mean, na.rm = TRUE)),
  'pre' = unlist(global(r_pre_sm, mean, na.rm = TRUE)))



# ERA5-Land #
# data from 1950 to 2000
data_dir <- "D:/climate/ERA5-Land/raw_monthly"
r_temp <- crop(rast(file.path(data_dir, "meantemp_totpre_19502000.nc"), 
               subds = "t2m"), extent)
r_temp <- r_temp-273.15
r_pre <- crop(rast(file.path(data_dir, "meantemp_totpre_19502000.nc"), 
               subds = "tp"), extent)
r_pre <- r_pre*1000 # mm
r_temp <- aggregate(r_temp, 5, fun = "mean", na.rm = TRUE)
r_temp <- resample(r_temp, r_temp_had, method = "average")
r_temp <- mask(r_temp, r_temp_had)
r_pre <- aggregate(r_pre, 5, fun = "mean", na.rm = TRUE)
r_pre <- resample(r_pre, r_temp_had, method = "average")
r_temp <- mask(r_pre, r_temp_had)

indices<-rep(0:50,each=12)
r_temp_yrmn <- tapp(r_temp, indices, fun = mean, na.rm=FALSE) # yearly mean temperature
r_pre_sm <- tapp(r_pre*30, indices, fun = sum, na.rm=FALSE) # yearly sum of precipitation
era5land_data <- data.frame('year' = 1950:2000,
                           'temp' = unlist(global(r_temp_yrmn, mean, na.rm = TRUE)),
                           'pre' = unlist(global(r_pre_sm, mean, na.rm = TRUE)))



# CRU #
# data from 1901 to 2000
data_dir <- "D:/climate/CRU"
list_years <- c("1901.1910", "1911.1920", "1921.1930", "1931.1940", "1941.1950",
                "1951.1960", "1961.1970", "1971.1980", "1981.1990", "1991.2000")
r_pre_cru <- rast(lapply(list_years, function(i) rast(file.path(data_dir, "PRE", paste0("cru_ts4.05.",i,".pre.dat.nc")),subds = "pre")))
r_pre_cru <- crop(r_pre_cru, extent)
indices<-rep(1:100,each=12)
r_pre_sm <- tapp(r_pre_cru, indices, fun = sum, na.rm=FALSE) # yearly sum of precipitation
cru_data <- data.frame('year' = 1901:2000,
                            'temp' = 0,
                            'pre' = unlist(global(crop(r_pre_sm, extent), mean, na.rm = T)))




# Combine and plot #
climate_data <- rbind(hadcm3b_data, era5land_data, cru_data)
climate_data$mod <- c(rep("HadCM3B", 51), rep("ERA5-Land", 51), rep("CRU", 100))

climate_data <- climate_data %>% group_by(mod) %>% 
  mutate(roll_mean_temp = rollmean(temp, 5, na.pad = T),
         roll_mean_pre = rollmean(pre, 5, na.pad = T))

temp1950_had <- as.numeric(climate_data[climate_data$year == 1950 & 
                                         climate_data$mod == "HadCM3B", "temp"])
temp1950_era <- as.numeric(climate_data[climate_data$year == 1950 & 
                                          climate_data$mod == "ERA5-Land", "temp"])

temp_expr <- sprintf("Delta == %0.2f ~ \"°C\"", round(temp1950_era-temp1950_had,2))
ggplot(data = climate_data) +
  geom_segment(aes(x = 1950, 
                   y = temp1950_had, 
                   xend = 1950, 
                   yend = temp1950_era),
               linewidth = 1) +
  annotate("text", x = 1950, y = 7.67, label = temp_expr, parse = T) +
  geom_point(aes(x = year, y = temp, shape = mod, col = mod)) +
  geom_line(aes(x = year, y=roll_mean_temp, col = mod)) +
  ylab("Global mean temperature") +
  xlab("Year") +
  theme_minimal() +
  theme(legend.title = element_blank())

pre1950_had <- as.numeric(climate_data[climate_data$year == 1950 & 
                                          climate_data$mod == "HadCM3B", "pre"])
pre1950_era <- as.numeric(climate_data[climate_data$year == 1950 & 
                                          climate_data$mod == "ERA5-Land", "pre"])
pre1950_cru <- as.numeric(climate_data[climate_data$year == 1950 & 
                                         climate_data$mod == "CRU", "pre"])

pre_expr <- sprintf("Delta == %0.2f ~ \"mm\"", round(pre1950_era-pre1950_had,2))
ggplot(data = climate_data) +
  geom_segment(aes(x = 1950, 
                   y = pre1950_had, 
                   xend = 1950, 
                   yend = pre1950_era),
               linewidth = 1) +
  annotate("text", x = 1950, y = 687, label = pre_expr, parse = T) +
  geom_point(aes(x = year, y = pre, shape = mod, col = mod)) +
  geom_line(aes(x = year, y=roll_mean_pre, col = mod)) +
  ylab("Global total precipitation") +
  xlab("Year") +
  theme_minimal() +
  theme(legend.title = element_blank())


  


