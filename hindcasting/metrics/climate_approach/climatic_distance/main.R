
#---------------------------------------------------#
# Script to compute several climate novelty metrics #
#---------------------------------------------------#




library(data.table)
library(terra)
library(future.apply)
library(pracma)


extent <- ext(c(-10,35,36,71))

#-----------------------#
# Burke et al. approach #
#-----------------------#

# Mahalanobis minimum distance

# Variables of interest:
# - three-month means of temperature
# - three-month sums of precipitation

# Ice-mask: less than 50% ice cover

# reference: ERA5-Land (1970-2000) - calibration period
era5land_ref <- average_to_three_month_means(1970:2000, name_sim = "ERA5LAND", data_dir = "D:/climate/ERA5-Land/phenofit_format/transformed")
era5land_ref <- crop(era5land_ref, extent)
.era5land_ref <- wrap(era5land_ref)

# calculate climate bovelty for each target period (in parallel)
plan(multisession, workers = 10)
burke_climatenovelty <- future_sapply(seq(18000,500,-500), function(yr){
  era5land_ref <- rast(.era5land_ref)
  hadcm3b_foc <- average_to_three_month_means((-yr-15):(-yr+15), name_sim = "HadCM3B", 
                                              data_dir = paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/", yr, "BP"))
  r_climdist <- climatic_dissimilarity(focal = hadcm3b_foc, baseline = era5land_ref,
                                       method = "mahalanobis")
  
  # ice filter (less than 50% ice cover, as in Burke et al. 2019)
  yr_ICE6G <- yr/1000
  extent <- ext(c(-10,35,36,71))
  ice_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), extent)
  ice_HR <- resample(ice_HR, hadcm3b_foc)
  ice_HR[ice_HR >= 0.5] <- NA
  r_climdist <- mask(r_climdist, ice_HR)
  
  return(c(year = yr, 
           mean = as.numeric(global(r_climdist, mean, na.rm = T)$mean), 
           median= as.numeric(global(r_climdist, median, na.rm = T)$global), 
           q25 = as.numeric(global(r_climdist, quantile, na.rm = T)$X25), 
           q75 = as.numeric(global(r_climdist, quantile, na.rm = T)$X75)))
})
plan(sequential)
gc()
burke_climatenovelty <- as.data.frame(t(burke_climatenovelty))
saveRDS(burke_climatenovelty, file ="C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/burke_climatenovelty.rds")






#-----------------------------------------------------------------------#
# PALEOCLIMATE: Burke et al. approach + uniformization with CRU dataset #
#-----------------------------------------------------------------------#

cru_baseline <- compute_CRU_baseline(from = 1921, to = 1980, extent, data_dir = "D:/climate/CRU")

# In the past: HadCM3B
yr <- 15
hadcm3b_present <- average_to_three_month_means((-yr-15):(-yr+15), name_sim = "HadCM3B", 
                                                data_dir = paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/", yr, "BP"))
hadcm3b_present <- resample(hadcm3b_present, cru_baseline, method = "average")

.cru_baseline <- wrap(cru_baseline )
.hadcm3b_present <- wrap(hadcm3b_present)

plan(multisession, workers = 4)
burke_climatenovelty <- future_sapply(c(seq(18000,250,-250), 15), function(yr){
  cru_baseline <- rast(.cru_baseline )
  hadcm3b_present <- rast(.hadcm3b_present)
  
  hadcm3b_past <- average_to_three_month_means((-yr-15):(-yr+15), name_sim = "HadCM3B", 
                                               data_dir = paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/", yr, "BP"))
  hadcm3b_past <- resample(hadcm3b_past, cru_baseline, method = "average")
  
  tmp_anom <- subset(hadcm3b_past - hadcm3b_present, 1:4) # difference
  pre_anom <- subset(hadcm3b_past/hadcm3b_present, 5:8) # ratio
  
  hadcm3b_cru <- c(subset(cru_baseline,1:4)+tmp_anom, subset(cru_baseline,5:8)*pre_anom)
  
  # ice filter (less than 50% ice cover, as in Burke et al. 2019)
  yr_ICE6G <- yr%/%500*0.5
  extent <- ext(c(-10,35,36,71))
  ice_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), extent)
  ice_HR <- resample(ice_HR, hadcm3b_cru)
  ice_HR[ice_HR >= 0.5] <- NA
  hadcm3b_cru <- mask(hadcm3b_cru, ice_HR)
  
  r_climdist <- climatic_dissimilarity(focal = hadcm3b_cru, baseline = cru_baseline,
                                       method = "mahalanobis")
  
  output <- c(yr, 
              as.numeric(global(r_climdist$mahalanobis_distance, mean, na.rm = T)$mean), 
              as.numeric(global(r_climdist$mahalanobis_distance, median, na.rm = T)$global), 
              as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X25), 
              as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X75),
              round(global((subset(r_climdist, 2:9)), mean, na.rm = T)$mean, 1),
              round(global(sum(subset(r_climdist, 2:5)), mean, na.rm = T)$mean, 1),
              round(global(sum(subset(r_climdist, 6:9)), mean, na.rm = T)$mean, 1))
  
  names(output) <- c("year", "mean", "median", "q25", "q75", names(subset(r_climdist, 2:9)), "contribution_tmp", "contribution_pre")
  
  return(output)
})
plan(sequential)
gc()
burke_climatenovelty <- as.data.frame(t(burke_climatenovelty))
saveRDS(burke_climatenovelty, file ="C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/burke_climatenovelty_CRUbaseline.rds")


#-------------------------------------------------------------------------#
# FUTURE CLIMATE: Burke et al. approach + uniformization with CRU dataset #
#-------------------------------------------------------------------------#

terraOptions(memfrac=0.9)
cmip6_dir <- "D:/climate/CMIP6_Adjust"

cru_baseline <- compute_CRU_baseline(from = 1921, to = 1980, extent, data_dir = "D:/climate/CRU")

scenario <- "ssp585"

# IPSL model
model <- "IPSL-CM6A-LR"
ipsl_present <- average_to_three_month_means(1951:1980, name_sim = model, 
                                                data_dir = file.path(cmip6_dir, scenario, model, "phenofit_format"))
ipsl_present <- resample(ipsl_present, cru_baseline, method = "average")

.cru_baseline <- wrap(cru_baseline )
.ipsl_present <- wrap(ipsl_present)

plan(multisession, workers = 4)
burke_climatenovelty <- future_sapply(seq(2020, 2090,10), function(yr){
  cru_baseline <- rast(.cru_baseline )
  ipsl_present <- rast(.ipsl_present)
  
  ipsl_past <- average_to_three_month_means((yr-10):(yr+10), name_sim = model, 
                                               data_dir = file.path(cmip6_dir, scenario, model, "phenofit_format"))
  ipsl_past <- resample(ipsl_past, cru_baseline, method = "average")
  
  tmp_anom <- subset(ipsl_past - ipsl_present, 1:4) # difference
  pre_anom <- subset(ipsl_past/ipsl_present, 5:8) # ratio
  
  ipsl_cru <- c(subset(cru_baseline,1:4)+tmp_anom, subset(cru_baseline,5:8)*pre_anom)
  
  r_climdist <- climatic_dissimilarity(focal = ipsl_cru, baseline = cru_baseline,
                                       method = "mahalanobis")
  
  saveRDS(r_climdist, file.path("C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/climate_data/climate_novelty", paste0("climdist_", yr, "BP.rds")))
  
  output <- c(yr, 
              as.numeric(global(r_climdist$mahalanobis_distance, mean, na.rm = T)$mean), 
              as.numeric(global(r_climdist$mahalanobis_distance, median, na.rm = T)$global), 
              as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X25), 
              as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X75),
              round(global((subset(r_climdist, 2:9)), mean, na.rm = T)$mean, 1),
              round(global(sum(subset(r_climdist, 2:5)), mean, na.rm = T)$mean, 1),
              round(global(sum(subset(r_climdist, 6:9)), mean, na.rm = T)$mean, 1))
  
  names(output) <- c("year", "mean", "median", "q25", "q75", names(subset(r_climdist, 2:9)), "contribution_tmp", "contribution_pre")
  
  return(output)
})
plan(sequential)
gc()
burke_futureclimatenovelty <- as.data.frame(t(burke_climatenovelty))
saveRDS(burke_futureclimatenovelty , file ="C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/metrics/climate_approach/data/burke_futureclimatenovelty_CRUbaseline.rds")
