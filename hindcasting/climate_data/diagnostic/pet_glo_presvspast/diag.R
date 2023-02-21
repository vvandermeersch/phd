library(data.table)
library(ggplot2)

present_clim_dir <- "D:/climate/ERA5-Land/phenofit_format/transformed"

pres_pre <- fread(file.path(present_clim_dir, "ERA5LAND_pre_1970_dly.fit"))
pres_pre <- pres_pre[pres_pre$V1 == 49 & pres_pre$V2 == 2.5,]
pres_pre <- pres_pre[,3:367]

pres_glo <- fread(file.path(present_clim_dir, "ERA5LAND_glo_1970_dly.fit"))
pres_glo <- pres_glo[pres_glo$V1 == 49 & pres_glo$V2 == 2.5,]
pres_glo <- pres_glo[,3:367]

pres_pet <- fread(file.path(present_clim_dir, "ERA5LAND_pet_1970_dly.fit"))
pres_pet <- pres_pet[pres_pet$V1 == 49 & pres_pet$V2 == 2.5,]
pres_pet <- pres_pet[,3:367]

pres_tmax <- fread(file.path(present_clim_dir, "ERA5LAND_tmx_1970_dly.fit"))
pres_tmax <- pres_tmax[pres_tmax$V1 == 49 & pres_tmax$V2 == 2.5,]
pres_tmax <- pres_tmax[,3:367]

pres_data <- data.frame(d = 1:365, pre = t(pres_pre), glo = t(pres_glo), pet <- t(pres_pet), tmax = t(pres_tmax))


ggplot(data = pres_data) +
  geom_point(aes(x = d, y = glo)) +
  theme_minimal() +
  xlab("DOY") +
  ylab("Surface downwelling shortwave radiation")

ggplot(data = pres_data) +
  geom_point(aes(x = d, y = pet))

ggplot(data = pres_data) +
  geom_point(aes(x = d, y = tmax)) +
  theme_minimal() +
  xlab("DOY") +
  ylab("Daily maximum temperature")


past_clim_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate/outputs"

past_data <- fread(file.path(past_clim_dir, "20985_21015BP_glorad.csv"))
past_data <- past_data[past_data$id == 61 & past_data$year == 20985,]
past_data$d <- 1:365

past_data_toa <- fread(file.path(past_clim_dir, "20985_21015BP_toarad.csv"))
past_data_toa <- past_data_toa[past_data_toa$id == 3000 & past_data_toa$year == 21000,]
past_data$toa <- past_data_toa$toa


ggplot(data = past_data) + 
  geom_point(aes(x = d, y = glo)) +
  theme_minimal() +
  xlab("DOY") +
  ylab("Surface downwelling shortwave radiation")

ggplot(data = past_data) + 
  geom_point(aes(x = d, y = toa/1000))

ggplot(data = past_data) + 
  geom_point(aes(x = d, y = pet))

ggplot(data = past_data) + 
  geom_point(aes(x = d, y = netrad))

ggplot(data = past_data) + 
  geom_point(aes(x = d, y = tmax)) +
  theme_minimal() +
  xlab("DOY") +
  ylab("Daily maximum temperature")


source_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"
r_tmin <- rast(file.path(source_dir, "tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_24000_0kyr", 
                         paste0("tempmin_av_old_sims_1yrAvg_monthly_0.5degRes_noBias_Europe_", "22000_20001kyr", ".nc")))
extent <- extent(c(-14,40,34,72))
r_tmin <- crop(r_tmin, extent)
