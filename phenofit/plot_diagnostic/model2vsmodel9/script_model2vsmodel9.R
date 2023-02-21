wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/calibration_report"

library(ggplot2)
library(cowplot)
library(data.table)

source(file.path(wd, "functions", "read_mean_outputvalue.R"))

# Way to get lat/lon
climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
alt_file <- paste0(climate_folder, "/ERA5LAND_", "Altitude.fit")
alt <- fread(alt_file, showProgress=F)
colnames(alt) <- c("lat", "lon", "alt")
alt$lat <- round(alt$lat, 1)
alt$lon <- round(alt$lon, 1)


# Load values
sim_dir_v <- "D:/simulations/phenofit/forward/pinus_sylvestris/VVanderMeersch"
sim_dir_j <- "D:/simulations/phenofit/forward/pinus_sylvestris/ADuputie"
leaf_v <- read_mean_outputvalue(sim_dir_v, "LeafUnfoldingDate")
leaf_j <- read_mean_outputvalue(sim_dir_j, "LeafUnfoldingDate")
flower_v <- read_mean_outputvalue(sim_dir_v, "FloweringDate")
flower_j <- read_mean_outputvalue(sim_dir_j, "FloweringDate")
fruit_v <- read_mean_outputvalue(sim_dir_v, "FruitMaturationDate")
fruit_j <- read_mean_outputvalue(sim_dir_j, "FruitMaturationDate")

survival_v <- read_mean_outputvalue(sim_dir_v, "Survival")
survival_j <- read_mean_outputvalue(sim_dir_j, "Survival")
fitness_v <- read_mean_outputvalue(sim_dir_v, "Fitness")
fitness_j <- read_mean_outputvalue(sim_dir_j, "Fitness")
leaf_ind_v <- read_mean_outputvalue(sim_dir_v, "LeafIndex")
leaf_ind_j <- read_mean_outputvalue(sim_dir_j, "LeafIndex")
fruit_ind_v <- read_mean_outputvalue(sim_dir_v, "FruitIndex")
fruit_ind_j <- read_mean_outputvalue(sim_dir_j, "FruitIndex")
maturation_ind_v <- read_mean_outputvalue(sim_dir_v, "MaturationIndex")
maturation_ind_j <- read_mean_outputvalue(sim_dir_j, "MaturationIndex")


data <- as.data.frame(cbind(alt$lat, alt$lon,t(leaf_v), t(leaf_j), t(flower_v), t(flower_j),
                            t(fruit_v), t(fruit_j)))
colnames(data) <- c("lat", "lon", "leaf_v", "leaj_j", "flower_v", "flower_j", "fruit_v", "fruit_j")

p_leaf_v <- ggplot(data=data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = leaf_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "green", high = "red", limits=c(0,366)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_leaf_j <- ggplot(data=data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = leaf_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "green", high = "red", limits=c(0,366)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_flower_v <- ggplot(data=data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = flower_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "green", high = "red", limits=c(0,366)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_flower_j <- ggplot(data=data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = flower_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "green", high = "red", limits=c(0,366)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_fruit_v <- ggplot(data=data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fruit_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "green", high = "red", limits=c(min(fruit_v),366)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_fruit_j <- ggplot(data=data, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fruit_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "green", high = "red", limits=c(min(fruit_j),366)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))


plot_grid(p_leaf_v, p_leaf_j, p_flower_v, p_flower_j, p_fruit_v, p_fruit_j, 
          ncol = 2)

data_2 <- as.data.frame(cbind(alt$lat, alt$lon, t(survival_v), t(survival_j), t(fitness_v), t(fitness_j)))
colnames(data_2) <- c("lat", "lon", "survival_v", "survival_j", "fitness_v", "fitness_j")

p_survival_v <- ggplot(data=data_2, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = survival_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_survival_j <- ggplot(data=data_2, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = survival_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_fitness_v <- ggplot(data=data_2, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fitness_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_fitness_j <- ggplot(data=data_2, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fitness_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

plot_grid(p_survival_v, p_survival_j, p_fitness_v, p_fitness_j,
          ncol = 2)

data_ind <- as.data.frame(cbind(alt$lat, alt$lon, t(leaf_ind_v), t(leaf_ind_j), t(fruit_ind_v), t(fruit_ind_j),
                                t(maturation_ind_v), t(maturation_ind_j)))
colnames(data_ind) <- c("lat", "lon", "leaf_ind_v", "leaf_ind_j", "fruit_ind_v", "fruit_ind_j", 
                      "maturation_ind_v", "maturation_ind_j")

p_leaf_ind_v <- ggplot(data=data_ind, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = leaf_ind_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_leaf_ind_j <- ggplot(data=data_ind, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = leaf_ind_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_fruit_ind_v <- ggplot(data=data_ind, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fruit_ind_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_fruit_ind_j <- ggplot(data=data_ind, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fruit_ind_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_maturation_ind_v <- ggplot(data=data_ind, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = maturation_ind_v), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

p_maturation_ind_j <- ggplot(data=data_ind, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = maturation_ind_j), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1)) +
  theme(legend.key.height= unit(0.2, 'cm'),
        legend.key.width= unit(1, 'cm'))

plot_grid(p_leaf_ind_v, p_leaf_ind_j, p_fruit_ind_v, p_fruit_ind_j, p_maturation_ind_v, p_maturation_ind_j,
          ncol = 2)
