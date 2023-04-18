
intervals <- list(c(0, 2000), c(2001, 4000))

years_lists <- list(
  list(c((250+15):(250-15)), c((500+15):(500-15)), c((750+15):(750-15)), c((1000+15):(1000-15)), 
       c((1250+15):(1250-15)), c((1500+15):(1500-15)), c((1750+15):(1750-15)), c(2000:(2000-15))),
  list(c((2000+15):2001), c((2250+15):(2250-15)), c((2500+15):(2500-15)), c((2750+15):(2750-15)), c((3000+15):(3000-15)), 
       c((3250+15):(3250-15)), c((3500+15):(3500-15)), c((3750+15):(3750-15)), c(4000:(4000-15)))
)
                  


in_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw"
out_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/raw_dscl_15min"
ncores <- 10

for(i in 1:length(intervals)){
  
  monthly_downscale_with_altitude(yr_interval = intervals[[i]], years_list = years_lists[[i]],
                                  in_folder, out_folder, 
                                  extent, WHC_present,
                                  ncores, foc_extrapol = TRUE)
  
}



for(i in 1:length(intervals)){
  
  monthly_downscale(yr_interval = intervals[[i]], years_list = years_lists[[i]],
                    in_folder, out_folder, 
                    extent, WHC_present,
                    foc_extrapol = TRUE)
  
}
