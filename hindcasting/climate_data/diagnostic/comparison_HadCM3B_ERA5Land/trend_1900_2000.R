#############################
# Climate from 1900 to 2000 #
#############################

# Compare ERA5-Land and HadCM3B 

library(terra)
library(data.table)
library(dplyr)
library(future.apply)

# functions
source("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/functions/years_functions.r")



# HadCM3B #
data_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"
temp_fd <- "temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"
pre_fd <- "precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_24000_0kyr"

years <- c(0, 50) # 1950-1900
file_spec <- years_to_file(years)
rmonths <- year_to_months(years[1], years[2], file_spec$max)
r_temp <- rast(file.path(data_dir, temp_fd, 
                         paste0("temp_mm_1_5m_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
               subds = "temp_mm_1_5m", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO")
r_pre <- rast(file.path(data_dir, pre_fd, 
                        paste0("precip_mm_srf_old_sims_1yrAvg_monthly_0.5degRes_CRU_Europe_", file_spec$name, ".nc")),
              subds = "precip_mm_srf", lyrs = rmonths$min:rmonths$max, opts="HONOUR_VALID_RANGE=NO")
indices<-rep(0:50,each=12)
r_temp_yrmn <- tapp(r_temp, indices, fun = mean, na.rm=FALSE) # yearly mean temperature
r_pre_sm <- tapp(r_pre*30, indices, fun = sum, na.rm=FALSE) # yearly sum of precipitation

extent <- ext(c(-14,40,34,72))

hadcm3b_data <- data.frame('year' = 1900:1950,
  'temp' = unlist(global(crop(r_temp_yrmn, extent), mean, na.rm = T)),
  'pre' = unlist(global(crop(r_pre_sm, extent), mean, na.rm = T)))



# ERA5-Land #
# data from 1950 to 2000
data_dir <- "D:/climate/ERA5-Land/raw_monthly"
r_temp <- rast(file.path(data_dir, "meantemp_totpre_19502000.nc"), 
               subds = "t2m")
r_temp <- r_temp-273.15
r_pre <- rast(file.path(data_dir, "meantemp_totpre_19502000.nc"), 
               subds = "tp")
r_pre <- r_pre*1000 # mm
indices<-rep(0:50,each=12)
r_temp_yrmn <- tapp(r_temp, indices, fun = mean, na.rm=FALSE) # yearly mean temperature
r_pre_sm <- tapp(r_pre*30, indices, fun = sum, na.rm=FALSE) # yearly sum of precipitation

era5land_data <- data.frame('year' = 1950:2000,
                           'temp' = unlist(global(crop(r_temp_yrmn, extent), mean, na.rm = T)),
                           'pre' = unlist(global(crop(r_pre_sm, extent), mean, na.rm = T)))


# Combine and plot #
climate_data <- rbind(hadcm3b_data, era5land_data)
climate_data$mod <- c(rep("HadCM3B", 51), rep("ERA5-Land", 51))

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
  


