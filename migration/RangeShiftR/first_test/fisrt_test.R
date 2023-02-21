library(RangeShiftR)
library(raster)

# Working directory 
dirpath = "C:/Users/vandermeersch/Documents/CEFE/thesis/migration/RangeShiftR/first_test"

dir.create(file.path(dirpath,"Inputs"), showWarnings = TRUE)
dir.create(file.path(dirpath,"Outputs"), showWarnings = TRUE)
dir.create(file.path(dirpath,"Output_Maps"), showWarnings = TRUE)


################################
# PROCESSING SUITABILITY FILES #
################################

## Loading fitness (// suitability) values, resolution 0.1deg
hab_suit <- readRDS("C:/Users/vandermeersch/Documents/fagus_fit.rds")
hab_suit_raster <- rasterFromXYZ(hab_suit[,c("lon", "lat", "fitness")])
hab_suit_raster <- crop(hab_suit_raster, extent(c(5,9,43,45)))
crs(hab_suit_raster) <- CRS("+init=epsg:4326")
hab_suit_raster_msys <- projectRaster(hab_suit_raster, crs = CRS("+init=epsg:3035")) # reproject to metric system
## downscaling
res1km <- hab_suit_raster_msys
res(res1km) <- c(1000, 1000)
hab_suit_raster_ds <- resample(hab_suit_raster_msys, res1km, method = "bilinear")
suit_threshold <- 0.6 # under which no tree can live
hab_suit_raster_ds[hab_suit_raster_ds < suit_threshold] <- 0

## hypothetical species distribution, at res 10km
#res10km <- hab_suit_raster_msys
#res(res10km) <- c(10000, 10000)
#sp_dist <- resample(hab_suit_raster_msys, res10km, method = "bilinear")
sp_dist <- hab_suit_raster_ds
sp_dist[sp_dist > 0.98] <- 1
sp_dist[sp_dist <= 0.98] <- NA

hab_suit_raster_ds <- hab_suit_raster_ds * 100 # continuous values are expected, ranging from 0.0 to 100.0, that represent percentages of habitat quality

## plot suitability map and highlight cells with initial species distribution
plot(hab_suit_raster_ds, axes=T)
plot(sp_dist, add=T)

hab_suit_df <- as.data.frame(hab_suit_raster_ds, xy = T)
sp_dist_df <- as.data.frame(sp_dist, xy = T)
ggplot() +
  geom_raster(data = hab_suit_df, aes(x = x, y = y, fill = fitness)) +
  geom_raster(data = sp_dist_df[sp_dist_df$fitness > 0,], aes(x = x, y = y), fill = "green")



## save rasters
writeRaster(hab_suit_raster_ds, filename = file.path(dirpath, "Inputs", "hab_suit_raster_ds.txt"), format = "ascii", overwrite = T)
writeRaster(sp_dist, filename = file.path(dirpath, "Inputs", "sp_dist.txt"), format = "ascii", overwrite = T)
rm(hab_suit, hab_suit_raster, hab_suit_raster_msys, res1km, sp_dist)
gc()


###############
# RANGESHIFTR #
###############

# Initialisation

## Landscape parameters
suit <- ImportedLandscape(LandscapeFile = "hab_suit_raster_ds.asc",
                          Resolution = 1000,
                          HabPercent = TRUE,
                          K_or_DensDep = 500, #number of indivuals per ha
                          SpDistFile = "sp_dist.asc",
                          SpDistResolution = 1000
                          )


## Demography parameters
demo <- Demography(Rmax = 1.5)

## Dispersal parameters
disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.1), 
                   Transfer = DispersalKernel(Distances = 2000), 
                   Settlement = Settlement() )

## Initial parameters
init <- Initialise(InitType = 1, # = initialisation from a loaded species distribution map
                   SpType = 0,   # = all suitable cells within all distribution presence cells
                   InitDens = 0) # = at carrying capacity

## Simulation parameters

sim_0 <- Simulation(Simulation = 0, 
                    Replicates = 1, 
                    Years = 10,
                    
                    Absorbing = TRUE,
                    
                    OutIntPop = 10,
                    OutIntRange = 10)

s <- RSsim(land = suit, demog = demo, dispersal = disp, simul = sim_0, init = init)

# Run the simulation !
RunRS(s, paste0(dirpath, "/"))





pop_df <- readPop(s, paste0(dirpath, "/"))
ext <- as.vector(extent(hab_suit_raster_ds))
res <- 1000
pop_wide_rep0 <- reshape(subset(pop_df,Rep==0)[,c('Year','x','y','NInd')], timevar='Year', v.names=c('NInd'), idvar=c('x','y'), direction='wide')
stack_years_rep0 <- rasterFromXYZ(pop_wide_rep0)
