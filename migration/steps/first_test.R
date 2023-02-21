library(steps)
library(raster)
library(future.apply)

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/migration/steps" # working dir

# ## Loading fitness (// suitability) values, resolution 0.1deg
hab_suit <- readRDS("C:/Users/vandermeersch/Documents/fagus_fit.rds")
hab_suit_raster <- rasterFromXYZ(hab_suit[,c("lon", "lat", "fitness")])
hab_suit_raster <- crop(hab_suit_raster, extent(c(8.5,9.7,41.3,43)))
crs(hab_suit_raster) <- CRS("+init=epsg:4326")
hab_suit_raster_msys <- projectRaster(hab_suit_raster, crs = CRS("+init=epsg:3035")) # reproject to metric system
## downscaling
res1km <- hab_suit_raster_msys
res(res1km) <- c(100, 100)
hab_suit_raster_ds <- resample(hab_suit_raster_msys, res1km, method = "bilinear")
suit_threshold <- 0.6 # under which no tree can live
hab_suit_raster_ds[hab_suit_raster_ds < suit_threshold] <- NA

## hypothetical species distribution
sp_dist_adult <- hab_suit_raster_ds
sp_dist_adult[sp_dist_adult > 0.9] <- 100
sp_dist_adult[sp_dist_adult <= 0.9] <- 0
sp_dist <- sp_dist_adult
sp_dist[!is.na(sp_dist)] <- 0
pop <- stack(sp_dist, sp_dist, sp_dist, sp_dist, sp_dist_adult)
names(pop) <- c("released_seeds", "seed_bank", "immature", "young adult", "adult")
# 
# ##  carrying capacity
k_cap <- sp_dist
k_cap[!is.na(k_cap)] <- 200

# hab_suit_raster_ds <- readRDS(file.path(wd,"hab_suit_raster_ds.rds"))
# k_cap <- readRDS(file.path(wd,"k_cap.rds"))
# pop <- readRDS(file.path(wd,"pop.rds"))
pop$adult[pop$adult >0] <- 50000



# parameters
GERMp <- 0.71 # seed germination rate
mu <- 0.8 # loss of seed, not specific
FECmax <- 29 # maximum fecundity 
age_mat <- 20 # age to be mature // not used
age_s1 <- 40 # young adult => adult // not used
R1 <- 0.7 # ratio for seed production, young adult (~ LAI/LAImax)
R2 <- 0.9 # ratio for seed prod, adult


t_mat <- matrix(c(0.0,   0.0,               0.0,   R1*FECmax,   R2*FECmax,
                  1.0,   (1-GERMp)*(1-mu),  0.0,   0.0,         0.0,
                  0.0,   0.01*GERMp,        0,     0.0,         0.0,
                  0.0,   0.0,               1.0,   0,           0.0,
                  0.0,   0.0,               0.0,   1.0,         0.99),
                  ncol = 5, byrow = TRUE)

colnames(t_mat) <- rownames(t_mat) <- c("released_seeds", "seed_bank", "immature", "young adult", "adult")


egk_landscape <- landscape(population = pop,
                           suitability = hab_suit_raster_ds,
                           carrying_capacity = k_cap)

egk_pop_dynamics <- population_dynamics(
  change = growth(transition_matrix = t_mat,
                  transition_function = modified_transition(survival_layer = NULL, # survival influenced or not by habitat suitability
                                                            fecundity_layer = NULL)),
  dispersal = kernel_dispersal(
    arrival_probability = "none",
    max_distance = 3000, # 3*cell resolution ?
    dispersal_kernel = custom_dispersal_kernel(p = c(4.74e-4, 1.74e-7, 1.53e-10), t = c(1500, 2500, 3000)),
    dispersal_proportion = set_proportion_dispersing(proportions = c(1, 0, 0, 0 ,0)) # only seeds car disperse
  ),
  modification = NULL,
  density_dependence = ceiling_density_by_stage(stages = c(1, 2, 3, 4, 5), c_caps = c(200000, 100000, 400000, 200000, 200000)), # for now
  dynamics_order = c("dispersal", "change",  "modification", "density_dependence")
)




egk_pop_dynamics <- population_dynamics(
  change = growth(transition_matrix = t_mat,
                  transition_function = modified_transition(survival_layer = NULL, # survival influenced or not by habitat suitability
                                                            fecundity_layer = NULL)),
  dispersal = fast_dispersal(
    dispersal_proportion = set_proportion_dispersing(proportions = c(1, 0, 0, 0 ,0)),
    dispersal_kernel = exponential_dispersal_kernel(distance_decay = 25/2)
    ),
  modification = NULL,
  density_dependence = ceiling_density_by_stage(stages = c(1, 2, 3, 4, 5), c_caps = c(200, 100, 100, 100, 100)), # for now
  dynamics_order = c("dispersal", "change",  "modification", "density_dependence")
)



egk_results <- simulation(landscape = egk_landscape,
                          population_dynamics = egk_pop_dynamics,
                          timesteps = 10,
                          verbose = TRUE)




individuals <- egk_results[[1]][[10]]$population$immature + 
  egk_results[[1]][[10]]$population$young.adult + 
  egk_results[[1]][[10]]$population$adult
individuals <- as.data.frame(individuals, xy=T)

pop_init <- egk_results[[1]][[1]]$population$immature + 
  egk_results[[1]][[1]]$population$young.adult + 
  egk_results[[1]][[1]]$population$adult
pop_init <- as.data.frame(pop_init, xy = T)


ggplot() +
  geom_raster(data=individuals, aes(x=x, y=y, fill=layer), na.rm=T) +
  scale_fill_gradient(low = "#a6cfa7", high = "#488B49", limits = c(1,80000), breaks = c(0,40000,80000), na.value = NA) +
  geom_raster(data=individuals[individuals$layer == 0, ], aes(x=x, y=y), fill="#EBEBD3", na.rm = T) +
  geom_raster(data=pop_init[pop_init$layer>0,], aes(x=x, y=y), fill="darkgreen") +
  theme_minimal()





custom_dispersal_kernel <- function (p, t) 
{
  fun <- function(r){
    sapply(r, function(i){
      if(i==0){
        return(1-sum(p))
      }else if(i <= t[1]){
        return(p[1])
      }else if(i <= t[2]){
        return(p[2])
      }else if(i <= t[3]){
        return(p[3])
      }
    })
  }
  steps:::as.dispersal_function(fun)
}




custom_dispersal_kernel_2 <- function (p, t) 
{
  fun <- function(r){
    sapply(r, function(i){
      if(i <= t[1]){
        return(p[1])
      }else if(i <= t[2]){
        return(p[2])
      }else if(i <= t[3]){
        return(p[3])
      }
    })
  }
  steps:::as.dispersal_function(fun)
}
