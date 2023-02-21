wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/run/"

library(AUC)
library(ggplot2)
library(colorspace)

source(paste0(wd, "/functions/read_mean_outputvalue.R"))
climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/05deg/5000BP"
simulation_folder <- "D:/simulations/phenofit/paleo/test/5000BP"

# 
# output_folder <- paste0(wd, "output_4000_0404__")
# fitness_6000 <- read_mean_outputvalue(1, output_folder, output_var = "Fitness")
# flowerdate_6000 <- read_mean_outputvalue(1, output_folder, output_var = "FloweringDate")
# droughtsurv_6000 <- read_mean_outputvalue(1, output_folder, output_var = "DroughtSurvival")
# leafunfolddate_6000 <- read_mean_outputvalue(1, output_folder, output_var = "LeafUnfoldingDate")
# survival_6000 <- read_mean_outputvalue(1, output_folder, output_var = "DroughtSurvival")

output_folder <- paste0(simulation_folder, "fagus_sylvatica/forward")
fitness_4000 <- read_mean_outputvalue(1, simulation_folder, output_var = "Fitness")
flowerdate_4000 <- read_mean_outputvalue(1, output_folder, output_var = "FloweringDate")
droughtsurv_4000 <- read_mean_outputvalue(1, output_folder, output_var = "DroughtSurvival")
leafunfolddate_4000 <- read_mean_outputvalue(1, output_folder, output_var = "LeafUnfoldingDate")
survival_4000 <- read_mean_outputvalue(1, output_folder, output_var = "DroughtSurvival")

# Plots

alt_file <- paste0(climate_folder, "/HadCM3B_", "Altitude.fit")
alt <- fread(alt_file, showProgress=F)
colnames(alt) <- c("lat", "lon", "alt")
alt$lat <- round(alt$lat, 1)
alt$lon <- round(alt$lon, 1)

data_fitness_6000 <- as.data.frame(cbind(alt$lat, alt$lon,t(fitness_6000)))
colnames(data_fitness_6000) <- c("lat", "lon", "fitness")

data_fitness_4000 <- as.data.frame(cbind(alt$lat, alt$lon,t(fitness_4000)))
colnames(data_fitness_4000) <- c("lat", "lon", "fitness")

fitness_map_6000 <- ggplot(data=data_fitness_6000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fitness)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,1))

fitness_map_4000 <- ggplot(data=data_fitness_4000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fitness)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,1))


data_fdate_6000 <- as.data.frame(cbind(alt$lat, alt$lon,t(flowerdate_6000)))
colnames(data_fdate_6000) <- c("lat", "lon", "date")

data_fdate_4000 <- as.data.frame(cbind(alt$lat, alt$lon,t(flowerdate_4000)))
colnames(data_fdate_4000) <- c("lat", "lon", "date")

fdate_map_6000 <- ggplot(data=data_fdate_6000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = date)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,365))

fdate_map_4000 <- ggplot(data=data_fdate_4000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = date)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,365))


data_droughtsurv_6000 <- as.data.frame(cbind(alt$lat, alt$lon,t(droughtsurv_6000)))
colnames(data_droughtsurv_6000) <- c("lat", "lon", "value")

data_droughtsurv_4000 <- as.data.frame(cbind(alt$lat, alt$lon,t(droughtsurv_4000)))
colnames(data_droughtsurv_4000) <- c("lat", "lon", "value")

droughtsurv_map_6000 <- ggplot(data=data_droughtsurv_6000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = value)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,1))

droughtsurv_map_4000 <- ggplot(data=data_droughtsurv_4000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = value)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,1))

data_leafdate_6000 <- as.data.frame(cbind(alt$lat, alt$lon,t(leafunfolddate_6000)))
colnames(data_leafdate_6000) <- c("lat", "lon", "date")

data_leafdate_4000 <- as.data.frame(cbind(alt$lat, alt$lon,t(leafunfolddate_4000)))
colnames(data_leafdate_4000) <- c("lat", "lon", "date")

leaf_map_6000 <- ggplot(data=data_leafdate_6000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = date)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,365))

leaf_map_4000 <- ggplot(data=data_leafdate_4000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = date)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,365))

data_survival_6000 <- as.data.frame(cbind(alt$lat, alt$lon,t(survival_6000)))
colnames(data_survival_6000) <- c("lat", "lon", "value")

data_survival_4000 <- as.data.frame(cbind(alt$lat, alt$lon,t(survival_4000)))
colnames(data_survival_4000) <- c("lat", "lon", "value")

surv_map_6000 <- ggplot(data=data_survival_6000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = value)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,1))

surv_map_4000 <- ggplot(data=data_survival_4000, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = value)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,1))
