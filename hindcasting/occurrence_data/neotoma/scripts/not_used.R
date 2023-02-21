# Calculate a yearly mean snow depth (500 yr window)

library(raster)

hadcm3b_dir <- "D:/climate/HadCM3B_60Kyr_Climate/raw/IceFrac"
file <- "regrid_icefrac_"

eu <- as(extent(-14, 40, 34, 72), 'SpatialPolygons')


years <- seq(21000, 500, by = -500)
file_years <- c("0_2.15", "2.5_5", "5_7.5", "7.5_10", "10_12.5", "12.5_15", "15_17.5", "17.5_20", "20_22.5")
min_years <- c(0, 2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000)
max_years <- c(2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000, 22500)



for(yr in years){
  print(yr)
  if(sum(max_years > yr & min_years < yr) == 1){
    # if the interval year+/-250is include in only one file
    ind <- which(max_years > yr & min_years < yr)
    r <- stack(file.path(hadcm3b_dir, paste0(file, file_years[ind], "kyr.nc")))
    # find the subset bound
    max_l <- max_years[ind] - yr + 250
    min_l <- max_years[ind] - yr - 250
    # keep only the needed layers 
    subr <- subset(r, min_l:max_l)
    # crop and keep only Europe
    crs(eu) <- crs(r)
    subr <- crop(subr, eu)
    
    # mean of this 500-year window
    ice_frac <- mean(subr)
    
  }else if(!all(max_years > yr & min_years < yr)){
    # if the interval year+/-250 is include in two files
    ind <- which(min_years < yr)
    r1 <- stack(file.path(hadcm3b_dir, paste0(file, file_years[ind], "kyr.nc")))
    r2 <- stack(file.path(hadcm3b_dir, paste0(file, file_years[ind+1], "kyr.nc")))
    
    
    
  }
  
  
  print(i)
  mn_data <- stack(file.path(hadcm3b_dir, paste0(file, i, "kyr.nc")))
  yr_data <- stackApply(mn_data, indices, fun = mean)
  
  yearly_snowdepth <- stack(yearly_snowdepth, yr_data)
  
}





# year 0 :
29989 # 12 * (2500-yr) - 11
30000 # 12 * (2500-yr)

# year 2499 BP
# 1 -> 12 * (2500-yr) - 11
# 12 -> 12 * (2500-yr)

yr <- 2500 #BP
