
# Script to downscale HadCM3B altitude from 30min to 15 min
# Temperatures are downscaled with altitude



wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_data/generate"

source(file.path(wd, "functions", "monthly_downscale_with_altitude.R"))
source(file.path(wd, "functions", "monthly_downscale.R"))
source(file.path(wd, "functions", "merge_rasters.R"))


# intervals <- list(c(0, 2000), c(2001, 4000), c(4001, 6000), c(6001, 8000), c(8001, 10000))
intervals <- list(c(10001, 12000), c(12001, 14000), c(14001, 16000), c(16001, 18000), c(18001, 20000))

years_lists <- lapply(intervals, function(i){
  yr <- max(i)-2000
  l <- lapply(seq(250,1750,250), function(j) return((j+yr)+c(15:-15)))
  if(yr == 0){
    l <- append(l, list(c(30:0)), after = 0)
    l <- append(l, list((2000+yr)+c(0:-15)))
  }else{
    l <- append(l, list((0+yr)+c(15:1)), after = 0)
    l <- append(l, list((2000+yr)+c(0:-15)))
  }
  return(l)
})


in_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"
out_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw_dscl_15min"
ncores <- 10

# Downscale temperatures (parallel computation)
for(i in 1:length(intervals)){
  
  monthly_downscale_with_altitude(yr_interval = intervals[[i]], years_list = years_lists[[i]],
                                  in_folder, out_folder, 
                                  extent, WHC_present,
                                  ncores, foc_extrapol = TRUE)
  
}

# Downscale other variables
for(i in 1:length(intervals)){
  
  monthly_downscale(yr_interval = intervals[[i]], years_list = years_lists[[i]][1],
                    in_folder, out_folder, 
                    extent, WHC_present,
                    foc_extrapol = TRUE)
  
}

# Merge rasters which fall between two periods
to_merge <- list(c("2015_2001kyr", "2000_1985kyr", "2015_1985kyr"), c("4015_4001kyr", "4000_3985kyr", "4015_3985kyr"),
                 c("6015_6001kyr", "6000_5985kyr", "6015_5985kyr"), c("8015_8001kyr", "8000_7985kyr", "8015_7985kyr"))

to_merge <- list(c("10015_10001kyr", "10000_9985kyr", "10015_9985kyr"), c("12015_12001kyr", "12000_11985kyr", "12015_11985kyr"),
                 c("14015_14001kyr", "14000_13985kyr", "14015_13985kyr"), c("16015_16001kyr", "16000_15985kyr", "16015_15985kyr"),
                 c("18015_18001kyr", "18000_17985kyr", "18015_17985kyr"))

for(i in to_merge){
  
  merge_rasters(i, out_folder)
  
}


