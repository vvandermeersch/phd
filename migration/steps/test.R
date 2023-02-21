library(steps)
library(raster)
library(future.apply)

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/migration/steps" # working dir

# ## Loading fitness (// suitability) values, resolution 0.1deg
hab_suit <- readRDS("C:/Users/vandermeersch/Documents/fagus_fit.rds")
hab_suit_raster <- rasterFromXYZ(hab_suit[,c("lon", "lat", "fitness")])
hab_suit_raster <- crop(hab_suit_raster, extent(c(8.5,9.7,41.3,43)))
crs(hab_suit_raster) <- CRS("+init=epsg:4326")
hab_suit_raster_msys <- projectRaster(hab_suit_raster, crs = CRS("+init=epsg:3035")) # reproject to metric system
## downscaling
res100m <- hab_suit_raster_msys
res(res100m) <- c(100, 100)
hab_suit_raster_ds <- resample(hab_suit_raster_msys, res100m, method = "bilinear")
suit_threshold <- 0.6 # under which no tree can live
hab_suit_raster_ds[hab_suit_raster_ds < suit_threshold] <- 0

## hypothetical species distribution
sp_dist_adult <- hab_suit_raster_ds
sp_dist_adult[sp_dist_adult > 0.9] <- 1000
sp_dist_adult[sp_dist_adult <= 0.9] <- 0
sp_dist <- sp_dist_adult
sp_dist[!is.na(sp_dist)] <- 0
pop <- stack(sp_dist, sp_dist, sp_dist, sp_dist, sp_dist_adult)
names(pop) <- c("released_seeds", "seed_bank", "immature", "young adult", "adult")


# hab_suit_raster_ds <- readRDS(file.path(wd,"hab_suit_raster_ds.rds"))
# k_cap <- readRDS(file.path(wd,"k_cap.rds"))
# pop <- readRDS(file.path(wd,"pop.rds"))
pop$adult[pop$adult >0] <- 1000
k_cap <- sp_dist
k_cap[!is.na(k_cap)] <- 2000


# parameters
GERMp <- 0.71 # seed germination rate
mu <- 0.8 # loss of seed, not specific
FECmax <- 29 # maximum fecundity 
age_mat <- 40 # age to be mature
age_ad <- 80 # age to be adult
R1 <- 0.5 # ratio for seed production, young adult (~ LAI/LAImax)
R2 <- 0.9 # ratio for seed prod, adult
s_mor <- 0.01 # stochastic mortality


t_mat <- matrix(c(0.0,   0.0,               0.0,                        R1*FECmax,                   R2*FECmax,
                  1.0,   (1-GERMp)*(1-mu),  0.0,                        0.0,                         0.0,
                  0.0,   0.01*GERMp,        (1-s_mor)*(1-(1/age_mat)),  0.0,                         0.0,
                  0.0,   0.0,               (1/age_mat),                (1-s_mor)*(1-(1/age_ad)),    0.0,
                  0.0,   0.0,               0.0,                        (1/age_ad),                  (1-s_mor)),
                ncol = 5, byrow = TRUE)

colnames(t_mat) <- rownames(t_mat) <- c("released_seeds", "seed_bank", "immature", "young adult", "adult")



egk_landscape <- landscape(population = pop,
                           suitability = hab_suit_raster_ds,
                           carrying_capacity = k_cap)






egk_pop_dynamics <- population_dynamics(
  change = growth(transition_matrix = t_mat,
                  transition_function = modified_transition(survival_layer = NULL, # survival influenced or not by habitat suitability
                                                            fecundity_layer = NULL)),
  dispersal = fast_dispersal(
    dispersal_proportion = set_proportion_dispersing(proportions = c(1, 0, 0, 0 ,0)),
    dispersal_kernel = exponential_dispersal_kernel(distance_decay = 25/2)
  ),
  modification = NULL,
  density_dependence = ceiling_density_by_stage(stages = c(1, 2, 3, 4, 5), c_caps = c(10000, 10000, 4000, 2000, 2000)), # for now
  dynamics_order = c("dispersal", "change",  "modification", "density_dependence")
)


egk_pop_dynamics <- population_dynamics(
  change = growth(transition_matrix = t_mat,
                  transition_function = modified_transition(survival_layer = NULL, # survival influenced or not by habitat suitability
                                                            fecundity_layer = NULL)),
  dispersal = kernel_dispersal(
    max_distance = 300,
    dispersal_proportion = set_proportion_dispersing(proportions = c(1, 0, 0, 0 ,0)),
    dispersal_kernel = exponential_dispersal_kernel(distance_decay = 25/2),
    arrival_probability = "none"
  ),
  modification = NULL,
  density_dependence = ceiling_density_by_stage(stages = c(1, 2, 3, 4, 5), c_caps = c(10000, 10000, 4000, 2000, 2000)), # for now
  dynamics_order = c("dispersal", "change",  "modification", "density_dependence")
)


plan(multisession, workers = 5)
system.time(
egk_results <- simulation(landscape = egk_landscape,
                          population_dynamics = egk_pop_dynamics,
                          timesteps = 100,
                          replicates = 1,
                          verbose = TRUE)
)

# 314s pour 5 ans
# 377.84s pour 5ans * 10 /10 cores
# 4753.33s pour 100 ans

# plot result
individuals <- egk_results[[1]][[100]]$population$young.adult + 
  egk_results[[1]][[100]]$population$adult
individuals <- as.data.frame(individuals, xy=T)

pop_init <- egk_results[[1]][[1]]$population$immature + 
  egk_results[[1]][[1]]$population$young.adult + 
  egk_results[[1]][[1]]$population$adult
pop_init <- as.data.frame(pop_init, xy = T)


ggplot() +
  geom_raster(data=individuals, aes(x=x, y=y, fill=layer), na.rm=T) +
  scale_fill_gradient(low = "#a6cfa7", high = "#488B49", limits = c(1,2000), breaks = c(0,1000,2000), na.value = NA) +
  geom_raster(data=individuals[individuals$layer == 0, ], aes(x=x, y=y), fill="#EBEBD3", alpha = 0.5, na.rm = T) +
  #geom_raster(data=pop_init[pop_init$layer>0,], aes(x=x, y=y), fill="darkgreen") +
  theme_minimal()



