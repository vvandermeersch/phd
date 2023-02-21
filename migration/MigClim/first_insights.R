
#-----------------------#
# First test of MigClim #
#-----------------------#                         

library(MigClim)
library(ggplot2)
library(terra)

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/migration/MigClim"

# little modification of MigClim R function
source(file.path(wd, "functions", "migclim.migrate.custom.R"))
environment(MigClim.migrate.custom) <- asNamespace('MigClim')
assignInNamespace("MigClim.migrate", MigClim.migrate.custom, ns = "MigClim")



# MigClim.migrate(iniDist="InitialDist", # initial distribution
#                 
#                 hsMap="HSmap", # habitat suitability, between 0 and 1000
#                 rcThreshold=0, # binary or continuous mode
#                 
#                 envChgSteps=1, # number of times the hsMap should be updated (max 295)
#                 dispSteps=1,  # number of dispersal steps per hsMap (max 99) 
#                               # if the interval between hsMap is 5 years, and the species disperse once a year
#                               # disSteps must be set to 5 
#                 
#                 dispKernel=c(1.0,1.0), 
#                 
#                 barrier="", barrierType="strong", # cells across which dispersal cannot occur, only affect SD events
#                 
#                 iniMatAge=1, propaguleProd=c(1.0), # propagule production probability as a function of time
#                                                    # since the cell became colonized
#                 # note that cell age is measured in dispersal steps
#                 
#                 lddFreq=0.0, lddMinDist=NULL, lddMaxDist=NULL, # long distance dispersal events
#                 
#                 simulName="MigClimTest", replicateNb=1, 
#                 overWrite=TRUE, 
#                 
#                 testMode=FALSE, fullOutput=FALSE, keepTempFiles=FALSE)



#---------------------#  
# 1. Dispersal kernel #
#---------------------#  

## one-dimensional kernal or dispersal distance kernel sensu Nathan et al. (2008, 2012)

## 2Dt kernel for F. sylvatica according to Zani et al.
## SDD = 25m, b = 2

pdf_2dt = function(SDD, b = 0){
  
  a = 2/pi*gamma(b-1)/gamma(b-3/2)*SDD
  
  f2 = function(r)
    2*pi*r*(b-1)/(pi*a^2)*(1 + r^2/a^2)^(-b) # kD(r) = 2*pi*r*kL(r), see Nathan et al. (2012)
  
  return(f2)
}

int_2dt = function (SDD, b = 0, d) {
  integrate(pdf_2dt(SDD, b),  lower = d, upper = Inf)$value
}

##   integrate to unity
int_2dt(SDD = 25, b = 2, d = 0)

## e.g. for 4 cells
res = 25 # resolution of the grid, in meters - is it enough // SDD ~ 25 ?
ncells <- 4 # max: 100m
sapply(1:ncells, function(i) int_2dt(SDD = 25, b = 2, d = (i-1)*res))


disp_kernel <- sapply(1:ncells, function(i) int_2dt(SDD = 25, b = 2, d = (i-1)*res))



#---------------#  
# 2. Input data #
#---------------#  

sim_dir <- "D:/simulations/phenofit/paleo/test/1000BP"
fitness_map <- readRDS(file.path(sim_dir, "fitness.rds"))

terraOptions(memfrac = 0.9)

## change resolution
rtemp <- rast(fitness_map[, c("lon", "lat", "value")], crs = "EPSG:4326")
extent <- extent(c(-10,10,40,50))
rtemp <- crop(rtemp, extent)
rtemp <- project(rtemp, "EPSG:3035") # in meters

# change to 25m resolution // dispersal
rcopy <- rtemp
res(rcopy) <- c(res, res)
rtemp <- terra::resample(rtemp, rcopy, method="bilinear") 

init_dist <- rtemp
init_dist[init_dist >= 0.8] <- 1
init_dist[init_dist < 0.8] <- 0
terra::writeRaster(init_dist , file.path(wd, "simulation_test/init_dist.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")


hs_map1 <- rtemp*1000
hs_map1[hs_map1 < 600] <- 0
terra::writeRaster(hs_map1 , file.path(wd, "simulation_test/hs_map1.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")



#--------------#  
# 2. Migrate ! #
#--------------#  

setwd(file.path(wd, "simulation_test"))

MigClim.migrate.custom(iniDist= "init_dist", # initial distribution

                hsMap= "hs_map", # habitat suitability, between 0 and 1000
                rcThreshold=0, # continuous mode

                envChgSteps=1, # number of times the hsMap should be updated (max 295)
                dispSteps=200,  # number of dispersal steps per hsMap (max 99)
                              # if the interval between hsMap is 5 years, and the species disperse once a year
                              # disSteps must be set to 5

                dispKernel= disp_kernel,

                barrier="", barrierType="strong", # cells across which dispersal cannot occur, only affect SD events

                iniMatAge= 40, # just for example
                propaguleProd=c(1.0), # propagule production probability as a function of time
                                      # since the cell became colonized
                # note that cell age is measured in dispersal steps

                lddFreq=0.0, lddMinDist=NULL, lddMaxDist=NULL, # long distance dispersal events

                simulName="MigClimTest", replicateNb=10,
                overWrite=TRUE,

                testMode=FALSE, fullOutput=FALSE, keepTempFiles=FALSE)


Rst <- raster("MigClimTest/MigClimTest1_raster.asc")
Df <- as.data.frame(Rst, xy= TRUE)
ggplot() +
  geom_raster(data = Df[Df$MigClimTest1_raster == 0,], 
              aes(x = x, y = y), fill = "#edede1") + #unsuitable
  geom_raster(data = Df[Df$MigClimTest1_raster == 1,], 
              aes(x = x, y = y), fill = "#488B49") + # initial distribution
  geom_raster(data = Df[Df$MigClimTest1_raster > 1 & Df$MigClimTest1_raster < 30000,], 
              aes(x = x, y = y), fill = "#B4E0AA") + # colonized
  geom_raster(data = Df[Df$MigClimTest1_raster == 30000,], 
              aes(x = x, y = y), fill = "#FFCD6A") + # suitable but not colonized
  geom_raster(data = Df[Df$MigClimTest1_raster < 0,], 
              aes(x = x, y = y), fill = "#C75C5A") + # decolonized
  theme_void() +
  ylab("") +
  xlab("")
  


