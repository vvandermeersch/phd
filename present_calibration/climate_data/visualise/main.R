library(data.table)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(sp)
library(raster)
library(cowplot)

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/climate_data/visualise/"

source(paste0(wd, 'functions/create_time_series.R'))
source(paste0(wd, 'functions/create_climate_map.R'))

# processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed/"
# processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/fagus_sylvatica_extraction/1000pres_1000abs/subset_1/"
# rawdata_folder <- "D:/climate/ERA5-Land/raw/"
processeddata_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/05deg/"


tmean_ts <- create_time_series("tmp", 2000, processeddata_folder)
glo_ts <- create_time_series("glo", 2000, processeddata_folder)
pet_ts <- create_time_series("pet", 2000, processeddata_folder)
pre_ts <- create_time_series("pre", 2000, processeddata_folder)

tmean_map <- create_climate_map("tmp", 1000, days = c(1,90,180,270), processeddata_folder, citsw = F, clim_name = "HadCM3B_")
glo_map <- create_climate_map("glo", 1000, days = c(1,90,180,270), processeddata_folder, citsw = F, clim_name = "HadCM3B_")
pet_map <- create_climate_map("pet", 2000, days = c(1,90,180,270), processeddata_folder, method=NULL)
petPM_map <- create_climate_map("pet", 1000, days = c(1,90,180,270), processeddata_folder, citsw = F, clim_name = "HadCM3B_",
                                method=NULL)
pre_map <- create_climate_map("pre", 2000, days = c(1,90,180,270), processeddata_folder)

plot_grid(tmean_map, tmean_ts, ncol=2)
plot_grid(glo_map, glo_ts, ncol=2)
plot_grid(pre_map, pre_ts, ncol=2)
plot_grid(pet_ts, pet_map, petPM_map, ncol=1)

# highest value of precipitation : 169 mm in one day !
high_pre <- create_climate_map("pre", 2000, days = 287, processeddata_folder)








