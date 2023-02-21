
wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/animated_convergence"

source(paste0(wd, "/functions/read_species_file.R"))
source(paste0(wd, "/functions/read_mean_outputvalue.R"))
library(dplyr)
library(AUC)
library(ggplot2)
library(readr)
library(magick)
library(gtools)

# Parameter inital values and bounds
param_init <- read_species_file(paste0(wd, "/input_files/quercus_ilex/Quercus_ilex_init.species"))
param_fixed <- read_species_file(paste0(wd, "/input_files/quercus_ilex/Quercus_ilex_lb.species"))
param_lb <- read_species_file(paste0(wd, "/input_files/quercus_ilex/Quercus_ilex_lb.species"))
param_ub <- read_species_file(paste0(wd, "/input_files/quercus_ilex/Quercus_ilex_ub.species"))
scale_factor <- 10
parameters=parameters=list(param_init=param_init, param_fixed=param_fixed, 
                           param_lb=param_lb, param_ub=param_ub, scale_factor=scale_factor)
structure_file <- paste0(wd, "/input_files/Quercus_ilex.species")

# List of parameter sets
rds_files <- mixedsort(list.files(path = file.path(wd, "calibration_files") , pattern = "\\.rds$", full.names = F))

# Run Phenofit for every set
for(rds in rds_files){
  x <- readRDS(file.path(wd, "calibration_files", rds))
  source(file.path(wd, "scripts","generate_species_file.R"))
  output_folder <- file.path(wd, "output_files", paste0(filename, "_"))
  source(file.path(wd, "scripts","run_phenofit.R"))
}

# Load presence/pseudo-absence points
species <- "quercus_ilex"
points_presabs <- readRDS(paste0("D:/species/processed/", species, "/", species, "_presabs.rds"))

# Compute AUC and generate fitness plots
for(rds in rds_files){
  print(rds)
  filename <- substring(rds, 1, nchar(rds)-4)
  sim_dir <- file.path(wd, "output_files", filename)
  fitness <- read_mean_outputvalue(sim_dir, "Fitness") # all Europe
  fitness_presabs <- read_mean_outputvalue(sim_dir, "Fitness", points = points_presabs) # only on pres/abs points
  source(file.path(wd, "scripts","generate_plot.R"))
}

fitness_imgs <- mixedsort(list.files(path = file.path(wd, "img") , pattern = "\\_fitness.png$", full.names = T))
img_list <- lapply(fitness_imgs, magick::image_read)
img_joined <- image_join(img_list)
fitness_animated <- image_animate(img_joined, delay = 50)
image_write(image = fitness_animated, path = file.path(wd, "img", "fitness_animated.gif"))
presabs_imgs <- mixedsort(list.files(path = file.path(wd, "img") , pattern = "\\_presabs.png$", full.names = T))
img_list <- lapply(presabs_imgs, magick::image_read)
img_joined <- image_join(img_list)
presabs_animated <- image_animate(img_joined, delay = 50)
image_write(image = presabs_animated, path = file.path(wd, "img", "presabs_animated.gif"))
