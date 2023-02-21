library(raster)
library(ggplot2)
library(future.apply)
library(ggpubr)

wd <- "D:/climate/ERA5/test_for_clearsky/europe"

cs_rad <- raster::stack(file.path(wd, "rad_clearsky_1960_2000.nc"))
cs_rad <- cs_rad/86400 
cs_rad_df <- as.data.frame(cs_rad, xy = F)
cs_rad_df <- as.data.frame(t(cs_rad_df))
cs_rad_df$year <- unlist(lapply(1960:2000, function(i){rep(i, 12)}))

rad <- raster::stack(file.path(wd, "rad_1960_2000.nc"))
rad <- rad/86400
rad_df <- as.data.frame(rad, xy = F)
rad_df <- as.data.frame(t(rad_df))
rad_df$year <- unlist(lapply(1960:2000, function(i){rep(i, 12)}))

cloudcover <- raster::stack(file.path(wd, "cloud_cover_1960_2000.nc"))
cloudcover_df <- as.data.frame(cloudcover, xy = F)
cloudcover_df <- as.data.frame(t(cloudcover_df))
cloudcover_df$year <- unlist(lapply(1960:2000, function(i){rep(i, 12)}))


plan(multisession, workers = 25)
options(future.globals.maxSize= 891289600)

# Calculate errors
data <- c()
for(yr in 1960:2000){
  
  ncells <- 300

  rmse_percent <- future_sapply(1:ncells, function(col){
    
    cc_p <- cloudcover_df[cloudcover_df$year == yr,col]
    cs_rad_p <- cs_rad_df[cs_rad_df$year == yr,col]
    rad_p <- rad_df[rad_df$year == yr,col]
    
    data <- data.frame(cs_rad_p, rad_p, cc_p)
    names(data) <- c("cs_rad", "rad", "cc")
    
    data$t <- c(17, 47, 75, 105, 135, 162, 198, 228, 258, 288, 318, 344)/30
    
    # calculate period
    ssp <- spectrum(data$cs_rad, plot=FALSE)  
    per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
    # fitting a sinusoidal with 1st and 2nd harmonics
    fit <- lm(cs_rad ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t), data = data)
    # interpolate daily values
    newdata <- data.frame(t =  c(17, 47, 75, 105, 135, 162, 198, 228, 258, 288, 318, 344)/30)
    newdata$cs_rad <- predict(fit, newdata = newdata, type = "response")
    
    
    # ggplot(data = data) +
    #   geom_line(aes(x = t, y = cs_rad), col = "darkblue") +
    #   geom_point(aes(x = t, y = rad), col = "darkred") +
    #   geom_line(data = newdata, aes(x = t, y = cs_rad), col = "blue") +
    #   theme_minimal()
    
    
    glo_rad_k <- kasten_clearness(data$cc)*newdata$cs_rad
    glo_rad_a <- antoine_clearness(data$cc)*newdata$cs_rad
    glo_rad_l <- luo_clearness(data$cc)*newdata$cs_rad
    newdata$rad_kasten <- glo_rad_k
    newdata$rad_antoine <- glo_rad_a
    newdata$rad_luo <- glo_rad_l
    
    ggplot(data = data) +
      geom_point(aes(x = t, y = cs_rad), col = "darkblue") +
      geom_point(aes(x = t, y = rad), col = "darkred") +
      geom_line(data = newdata, aes(x = t, y = cs_rad), col = "blue") +
      # geom_point(data = newdata, aes(x = t, y = rad_antoine), col = "red") +
      geom_line(data = newdata, aes(x = t, y = rad_antoine), col = "red") +
      theme_minimal()
    
    return(c(sqrt(mean((data$rad - newdata$rad_kasten)^2))/mean(data$rad), 
             sqrt(mean((data$rad - newdata$rad_antoine)^2))/mean(data$rad),
             sqrt(mean((data$rad - newdata$rad_luo)^2))/mean(data$rad)))
    
  })
  
  data <- rbind(data, cbind(rep(yr,ncells), t(rmse_percent)))

}




data_plot <- data.frame(rep(data[,1], 2), c(data[,2], data[,3]), c(rep("Kasten", nrow(data)), rep("Antoine", nrow(data))))
names(data_plot) <- c("year", "RMSEp", "method")
ggplot(data = data_plot) +
  geom_boxplot(aes(y = RMSEp*100, x = method)) +
  labs(x = "Method", y = "RMSE (%)") +
  stat_compare_means(aes(y = RMSEp*100, x = method), method = 'kruskal.test') +
  theme_minimal()
