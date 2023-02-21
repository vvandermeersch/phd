###########################################
# Script to extract climate for a species #
###########################################

# author : V. Van der Meersch
# date : 16/02/2022

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/climate_data/extract/"


## Functions needed

source(paste0(wd, 'functions/phenofit_extract_climate.R')) ## Extract climate 
#(changing decimal precision to reduce file size and transforming ET negative and NA values to zero (polar night, etc.))


## Settings ##

processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/"
speciesdata_folder <- "D:/species/"
out_folder <- paste0(processeddata_folder, "pinus_pinaster_extraction/1000pres_1000abs/subset_2/")
processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed/"


## Extract for some species (changing decimal precision to reduce file size and transforming ET negative and NA values to zero (polar night, etc.))

# load("D:/species/processed/fagus_sylvatica/fagussylvatica_occ_rs.Rdata")
# fagussylvatica_occ <- as.data.frame(fagussylvatica_occ_rs, xy=TRUE)
# fagussylvatica_occ <- na.omit(fagussylvatica_occ) %>% dplyr::select(-nb_src)
# fagussylvatica_occ$pres <- 1
# names(fagussylvatica_occ) <- c("lon", "lat", "pres")
# fagussylvatica_occ$lat <- round(fagussylvatica_occ$lat, 1)
# fagussylvatica_occ$lon <- round(fagussylvatica_occ$lon, 1)

load("D:/species/processed/pinus_pinaster/1000pres_1000abs/occurrence_subset_2.Rdata")

#species_occurrence <- data_frame(lon = c(8.5, 8.6, 8.5, 8.6), lat = c(36.1, 36.1, 36.0, 36.0), pres = c(1,1,1,1)) # points in TUnisia for F Mouillot

phenofit_extract_climate(1969, "Pinus pinaster", species_occurrence, processeddata_folder, out_folder, "PenmanMonteith")
for( yr in 1970:2000){
  print(yr)
  phenofit_extract_climate(yr, "Pinus pinaster", species_occurrence, processeddata_folder, out_folder, "PenmanMonteith")
}





## Extract for all Europe (changing decimal precision to reduce file size and transforming ET negative and NA values to zero (polar night, etc.))

ERA5land_grid <- raster(paste0("D:/climate/ERA5-Land/raw/","2m_dewpoint_temperature_1969_01.nc"))
filter1 <- raster("D:/soil/EU_SoilHydroGrids_1km/raw/FC_sl1.tif") %>% crop(ERA5land_grid)
filter1 <- resample(filter1, ERA5land_grid, method="bilinear")
filter2 <- raster("D:/soil/SoilGrids250m/BDRICM_M_250m_ll.tif") %>% crop(ERA5land_grid)
filter2 <- resample(filter2, ERA5land_grid, method="bilinear")
filter_df1 <- as.data.frame(filter1, xy=T)
names(filter_df1) <- c("lon", "lat", "value1") 
filter_df2 <- as.data.frame(filter2, xy=F)
names(filter_df2) <- c("value2")
filter_df <- cbind(filter_df1, filter_df2)
filter_df$value <- filter_df$value1 + filter_df$value2
filter_df$lat <- round(filter_df$lat, 1)
filter_df$lon <- round(filter_df$lon, 1)
filter_df <- filter_df %>% dplyr::select(-c("value1", "value2"))

out_folder <- paste0(processeddata_folder, "transformed/")
phenofit_extract_all_climate(1969:2000, processeddata_folder, out_folder, "PenmanMonteith", filter = filter_df)
