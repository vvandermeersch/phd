
wd <- "D:/climate/ERA5/test_for_clearsky/europe"

library(future.apply)
library(ggplot2)
library(ggpubr)

# hly_rad <- raster::stack(file.path(wd, "rad_hourly_2000.nc"))
# days <- rep(1:366,each=24)
# dly_rad <-stackApply(hly_rad, days, fun = sum)
# writeRaster(dly_rad, filename = file.path(wd, "rad_daily_2000.nc"))

# hly_tmp <- raster::stack(file.path(wd, "tmp_hourly_2000.nc"))
# days <- rep(1:366,each=24)
# dly_tmn <-stackApply(hly_tmp, days, fun = min)
# dly_tmx <-stackApply(hly_tmp, days, fun = max)
# writeRaster(dly_tmn, filename = file.path(wd, "tmn_daily_2000.nc"))
# writeRaster(dly_tmx, filename = file.path(wd, "tmx_daily_2000.nc"))

# hly_cs_rad <- raster::stack(file.path(wd, "rad_clearsky_hourly_2000.nc"))
# days <- rep(1:366,each=24)
# dly_cs_rad <-stackApply(hly_cs_rad, days, fun = sum)
# writeRaster(dly_cs_rad, filename = file.path(wd, "rad_clearsky_daily_2000.nc"))

# hly_cc <- raster::stack(file.path(wd, "cloud_cover_hourly_2000.nc"))
# days <- rep(1:366,each=24)
# dly_cc <-stackApply(hly_cc, days, fun = mean)
# writeRaster(dly_cc, filename = file.path(wd, "cloud_cover_daily_2000.nc"))


# Daily global radiation, year 2000
dly_rad <- raster::stack(file.path(wd, "rad_daily_2000.nc"))
dly_rad <- as.data.frame(dly_rad, xy = T)
dly_rad <- as.data.frame(t(dly_rad[,3:368]))
dly_rad <- dly_rad/1000000 #J/m2 to mJ/m2
dly_rad$t <- 1:366
plot(dly_rad[,10000] ~ dly_rad$t, col = "orange")

# Daily clear-sky global radiation, year 2000
dly_cs_rad <- raster::stack(file.path(wd, "rad_clearsky_daily_2000.nc"))
dly_cs_rad <- as.data.frame(dly_cs_rad, xy = T)
dly_cs_rad <- as.data.frame(t(dly_cs_rad[,3:368]))
dly_cs_rad <- dly_cs_rad/1000000 #J/m2 to mJ/m2
dly_cs_rad$t <- 1:366
points(dly_cs_rad[,10000] ~ dly_cs_rad$t, col = "blue")

# Monthly global radiation, year 2000
rad <- raster::stack(file.path(wd, "rad_2000.nc"))
rad <- as.data.frame(rad, xy = T)
rad <- as.data.frame(t(rad[,3:14]))
rad <- rad/1000000 #J/m2 to mJ/m2
rad$t <- c(17, 47, 75, 105, 135, 162, 198, 228, 258, 288, 318, 344) 
points(rad[,1] ~ rad$t, col = "red", pch = 16)

# Monthly clear-sky radiation, year 2000
cs_rad <- raster::stack(file.path(wd, "rad_clearsky_2000.nc"))
cs_rad <- as.data.frame(cs_rad, xy = T)
cs_rad <- as.data.frame(t(cs_rad[,3:14]))
cs_rad <- cs_rad/1000000 #J/m2 to mJ/m2
cs_rad$t <- c(17, 47, 75, 105, 135, 162, 198, 228, 258, 288, 318, 344) 
points(cs_rad[,10000] ~ cs_rad$t, col = "blue", pch = 16)

# Daily cloudiness, year 2000
dly_cc <- raster::stack(file.path(wd, "cloud_cover_hourly_2000.nc"))
dly_cc <- as.data.frame(dly_cc, xy = T)
dly_cc <- as.data.frame(t(dly_cc[,3:368]))
dly_cc$t <- 1:366
# plot(dly_cc[,10000] ~ dly_cc$t, col = "blue")

# Simulated radiation from ERA5 clear-sky radiation (daily step)
sim_rad <- antoine_clearness(dly_cc[,10000])*dly_cs_rad[,10000]
plot(dly_rad[,10000] ~ dly_rad$t, col = "orange")
points(sim_rad ~ dly_rad$t, col = "red", pch = 16)

# Calculate errors
data <- c()
ncells <- 33201


plan(multisession, workers = 4)
rmse_percent <- future_sapply(1:ncells, function(col){
    
    cc_p <- dly_cc[,col]
    cs_rad_p <- dly_cs_rad[,col]
    rad_p <- dly_rad[,col]
    
    data <- data.frame(cs_rad_p, rad_p, cc_p)
    names(data) <- c("cs_rad", "rad", "cc")
    
    glo_rad_k <- kasten_clearness(data$cc)*data$cs_rad
    glo_rad_a <- antoine_clearness(data$cc)*data$cs_rad
    data$rad_kasten <- glo_rad_k
    data$rad_antoine <- glo_rad_a
    
    return(c(sqrt(mean((data$rad - data$rad_kasten)^2))/mean(data$rad), c(sqrt(mean((data$rad - data$rad_antoine)^2))/mean(data$rad))))
    
})
rmse_percent <- t(rmse_percent)
data_plot <- data.frame(c(rmse_percent[,1], rmse_percent[,2]), c(rep("Kasten", ncells), rep("Antoine", ncells)))
names(data_plot) <- c("RMSEp", "method")
ggplot(data = data_plot) +
  geom_boxplot(aes(y = RMSEp*100, x = method)) +
  labs(x = "Method", y = "RMSE (%)") +
  stat_compare_means(aes(y = RMSEp*100, x = method), method = 'kruskal.test') +
  theme_minimal()
