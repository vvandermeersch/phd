library(raster)

source_dir <- "D:/climate/HadCM3B_60Kyr_Climate/biasregrid_highres"

output_dir <- "C:/Users/vandermeersch/Documents/CEFE/thesis/hindcasting/climate_data/daily_weather_generator/first_test"

years <- c(0, 1) # in BP
# note: BP = before present, pre-industrial (equivalent to the year 1950)
# thus, 0 BP = 1950 AD

extent <- extent(c(7,8,44,45))

write_gwgen_csv(years, extent, source_dir, output_dir)
