library(terra)
library(future.apply)

# loading high-resolution altitude
alt_10min <- crop(rotate(rast("D:/climate/ICE-6G-C/I6_C.VM5a_10min.0.nc", subds = "Orog")), extent)
ldmsk_10min <- crop(rotate(rast("D:/climate/ICE-6G-C/I6_C.VM5a_10min.0.nc", subds = "sftlf")), extent)
ldmsk_10min[ldmsk_10min == 0] <- NA
alt_10min <- mask(alt_10min, ldmsk_10min)
res_alt <- alt_10min

# downscaling climate data with local information (height correction)
# only for temperatures and global radiation
# note that if there is no effect of altitude (i.e. regression slope = 0), this is equivalent to a simple bilinear interpolation
for(yr in -985:-1015){
  message(paste0("Doing year ", yr,"\n"))
  downscale_with_altitude2(yr, alt_HR = alt_10min, 
                           in_folder = file.path(out_clim_dir, paste0(1000, "BP")), 
                           out_folder = file.path(ds_out_clim_dir, paste0(1000, "BP")), 
                           ncores = 15, foc_extrapol = TRUE)
  
}

# downscaling precipitation, cloudiness and wind 
# just a bilinear interpolation
for(yr in -985:-1015){
  message(paste0("Doing year ", yr,"\n"))
  downscale(yr, alt_HR = alt_10min, 
            in_folder = file.path(out_clim_dir, paste0(1000, "BP")), 
            out_folder = file.path(ds_out_clim_dir, paste0(1000, "BP")), 
            foc_extrapol = TRUE)
  
}



# computing PET


