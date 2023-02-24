
#-----------------------#
# First test of MigClim #
#-----------------------#     

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/migration/MigClim"

install.packages(file.path(wd, "migclim_custom"), 
                 repos = NULL, 
                 type = "source")

library(MigClimCustom)
library(ggplot2)
library(terra)



# little modification of MigClim R function
# source(file.path(wd, "functions", "migclim.migrate.custom.R"))
# environment(MigClim.migrate.custom) <- asNamespace('MigClim')
# assignInNamespace("MigClim.migrate", MigClim.migrate.custom, ns = "MigClim")



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
  integrate(pdf_2dt(SDD, b),  lower = d, upper = Inf, rel.tol = .Machine$double.eps^0.5)$value
}

##   integrate to unity
int_2dt(SDD = 25, b = 2, d = 0)

## e.g. for 4 cells
res = 25 # resolution of the grid, in meters - is it enough // SDD ~ 25 ?
ncells <- 40 # max: 200m
sapply(1:ncells, function(i) int_2dt(SDD = 25, b = 2, d = (i-1)*res+12.5))
disp_kernel <- sapply(1:ncells, function(i) int_2dt(SDD = 25, b = 2, d = (i-1)*res))




## linear combination of SSD and LDDD kernels
ncells <- which(sapply(1:42, function(i) int_2dt(SDD = 200, b = 2, d = (i-1)*res))<0.005)-1 
sapply(1:ncells, function(i) 0.99*int_2dt(SDD = 25, b = 2, d = (i-1)*res+12.5)+0.01*int_2dt(SDD = 200, b = 2, d = (i-1)*res+12.5))
disp_kernel_cb <- sapply(1:ncells, function(i) 0.99*int_2dt(SDD = 25, b = 2, d = (i-1)*res+res/2)+0.01*int_2dt(SDD = 200, b = 2, d = (i-1)*res+res/2))

ncells <- 8 # max 200m
disp_kernel_SD <- sapply(1:ncells, function(i) int_2dt(SDD = 25, b = 2, d = (i-1)*res))

ncells <- 80 # max: 200*10 = 2000m ?
disp_kernel_LD <- sapply(1:ncells, function(i) int_2dt(SDD = 200, b = 2, d = (i-1)*res))


kernels <-data.frame(d = 1:40, disp_kernel, disp_kernel_LD)
ggplot(data = kernels) +
  geom_line(aes(x =d*25, y = disp_kernel, color = "SD kernel"), size = 1) +
  geom_line(aes(x =d*25, y = disp_kernel_LD, color = "LD kernel"), size = 1) +
  theme_minimal() +
  labs(x = "Distance (m)", y = "Prob. of dispersion") +
  scale_color_manual(values = c("LD kernel" = "#e76f51", "SD kernel" = "#e9c46a"))


#
#
#

# adapted from Boisvert-Marsh et al. (2022)

iniMatAge <- 10
fullMatAge <- 50

x<-seq(0,fullMatAge-iniMatAge, 1)

sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}

shape<- 0.5
relInflecAge <- 15

params <-c(1, shape, relInflecAge) 

propaguleProd <- sigmoid(params,x)

plot(x, sigmoid(params,x), col='blue')






#---------------#  
# 2. Input data #
#---------------#  

sim_dir <- "D:/simulations/phenofit/paleo/test/1000BP"
fitness_map <- readRDS(file.path(sim_dir, "fitness.rds"))

terraOptions(memfrac = 0.9)

# load data
rtemp <- rast(fitness_map[, c("lon", "lat", "value")], crs = "EPSG:4326")
#extent <- extent(c(-10,10,40,50))
extent <- extent(c(5,10,45,50))
rtemp <- crop(rtemp, extent)
rtemp <- project(rtemp, "EPSG:3035") # in meters

# change to 25m resolution // dispersal
rcopy <- rtemp
res(rcopy) <- c(res, res)
rtemp <- terra::resample(rtemp, rcopy, method="bilinear") 
rm(rcopy)

# small dataset for test
sm_ext <- extent(c(4120000,4180000,2745000,2746000))
rtemp2 <- crop(rtemp, sm_ext)

# initial distribution
init_dist <- rtemp2
init_dist[init_dist >= 0.8] <- 1
init_dist[init_dist < 0.8] <- 0
terra::writeRaster(init_dist , file.path(wd, "small_test/init_dist.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")

# three similar habitat suitability layers
hs_map1 <- rtemp2*1000
hs_map1[hs_map1 < 600] <- 0
terra::writeRaster(hs_map1 , file.path(wd, "small_test/hs_map1.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")
terra::writeRaster(hs_map1 , file.path(wd, "small_test/hs_map2.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")
terra::writeRaster(hs_map1 , file.path(wd, "small_test/hs_map3.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")
terra::writeRaster(hs_map1 , file.path(wd, "small_test/hs_map4.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")
terra::writeRaster(hs_map1 , file.path(wd, "small_test/hs_map5.asc"), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")




#--------------#  
# 2. Migrate ! #
#--------------#  

setwd(file.path(wd, "small_test"))

MigClimCustom.migrate(iniDist= "init_dist", # initial distribution
                      
                      hsMap= "hs_map", # habitat suitability, between 0 and 1000
                      rcThreshold=0, # continuous mode
                      
                      envChgSteps=5, # number of times the hsMap should be updated (max 295)
                      dispSteps=200,  # number of dispersal steps per hsMap (max 200)
                      # if the interval between hsMap is 5 years, and the species disperse once a year
                      # disSteps must be set to 5
                      
                      dispKernel= disp_kernel_SD,
                      
                      barrier="", barrierType="strong", # cells across which dispersal cannot occur, only affect SD events
                      
                      iniMatAge = iniMatAge, # just for example
                      propaguleProd = propaguleProd, # propagule production probability as a function of time
                      # since the cell became colonized
                      # note that cell age is measured in dispersal steps
                      
                      lddFreq=0.01, # long distance dispersal events
                      dispKernel_LDD = disp_kernel_LD,
                      
                      simulName="MigClimTest", replicateNb=1,
                      overWrite=TRUE,
                      
                      testMode=FALSE, fullOutput=FALSE, keepTempFiles=FALSE)



#---------#  
# 3. Plot #
#---------#  

rst <- terra::rast("MigClimTest/MigClimTest_raster.asc")
crs(rst) <- "EPSG:3035"

rstcopy <- rst
m <- c(-30000, 0, 0,
       1, 1, 1,
       2, 29999, 2,
       30000, 30001, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rstcopy <- classify(rstcopy, rclmat, right=NA)
plot(rstcopy)



# rstcopy  <- subst(rst, c(0,30000), c(0,0))
# rstcopy[rstcopy == 30000 | rstcopy < 0] <- 0 # remove {suitable but not colonized} and {decolonized}
# change to 10km resolution // plotting resolution
rstcopy2 <- terra::aggregate(rstcopy, fact = 10000/res, fun="mean")



Df <- as.data.frame(rst, xy= TRUE)
ggplot() +
  geom_raster(data = Df[Df$MigClimTest1_raster == 0,], 
              aes(x = x, y = y), fill = "#edede1") + #unsuitable
  geom_raster(data = Df[Df$MigClimTest1_raster == 1,], 
              aes(x = x, y = y), fill = "#488B49") + # initial distribution
  geom_raster(data = Df[Df$MigClimTest1_raster > 1 & Df$MigClimTest1_raster < 30000,], 
              aes(x = x, y = y), fill = "#B4E0AA") + # colonized
  geom_raster(data = Df[Df$MigClimTest1_raster == 30000,], 
              aes(x = x, y = y), fill = "#FFCD6A")  # suitable but not colonized
geom_raster(data = Df[Df$MigClimTest1_raster < 0,], 
            aes(x = x, y = y), fill = "#C75C5A") + # decolonized
  theme_void() +
  ylab("") +
  xlab("")




# 
# ggplot() +
#   geom_spatraster(data = rst) +
#   binned_scale(aesthetics = "fill",
#                scale_name = "stepsn", 
#                palette = function(x) c("#edede1", "#488B49", "#B4E0AA", "#FFCD6A"),
#                breaks = c(0, 1, 29999, 30000),
#                limits = c(0, 30000),
#                show.limits = TRUE, 
#                guide = "colorsteps"
#   )

data <- as.data.frame(rstcopy, xy= TRUE)
names(data) <- c("x", "y", "val")
ggplot() +
  geom_raster(data = data, 
              aes(x = x-4120000, y = y, fill = as.factor(val))) + 
  theme_minimal() +
  scale_fill_manual(
    labels = c("Uncolonized", "Initial distribution", "Colonized"),
    values = c("0" = "#f6f4d2",  "1" = "#1a7431", "2" = "#25a244")) + 
  labs( y ="", x = "", fill = "") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") 

