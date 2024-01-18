
quercus_robur_280ppm <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/quercus_robur/cmaes_fit_subset1_rep1.rds")
biomass <- quercus_robur_280ppm$europe_pred

rbio <- rast(biomass[c(2,1,3)])
plot(rbio)
rbio[rbio<quercus_robur_280ppm$best_threshold] <- 0
plot(rbio)

rbio2 <- rbio
rbio2[rbio2>0] <- NA
sp_presabs2  <- vect(sp_presabs ,geom=c('lon','lat'))
NAvalues <- is.na(terra::extract(rbio2, sp_presabs2)$pred)
sp_presabs2 <- sp_presabs2[!NAvalues]

#plot(mask(crop(rbio,save),save))
points(sp_presabs2[sp_presabs2$pres ==1, c(2,3)])

occurrence_subset_1 <- readRDS("D:/species/processed/quercus_robur/1000pres_1000abs/occurrence_subset_1.rds")
calibration_points  <- vect(occurrence_subset_1 ,geom=c('lon','lat'))

points(calibration_points[calibration_points$pres ==1], col = "blue")





