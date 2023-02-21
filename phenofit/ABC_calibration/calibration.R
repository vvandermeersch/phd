wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/phenofit/ABC_calibration"

source(paste0(wd, "/functions/phenofit_ABC.R"))
source(paste0(wd, "/functions/create_init_species_file.R"))
source(paste0(wd, "/functions/command_file_setup.R"))
source(paste0(wd, "/functions/read_species_file.R"))
source(paste0(wd, "/functions/read_mean_fitness.R"))

library(EasyABC)
library(parallel)




structure_file <- paste0(wd, "/input_files/Fagus_sylvatica_EvolLett2019.species")

climate_folder <- "D:/climate/ERA5-Land/phenofit_format/fagus_sylvatica_extraction/1000pres_1000abs/subset_1"

capsis_settings=list(cd_java8 ="cd C:/Program Files/Java/scripts", java8 ="java8.cmd",
                     cd ="cd/d D:/applications/capsis4 && setmem 2000")

load("D:/species/processed/fagus_sylvatica/1000pres_1000abs/occurrence_subset_1.Rdata")
Yobs <- species_occurrence


x <- c(4578, -61, 3.5, -1.024, 12.5, 75.5, 60.5, 60.5, -1.025, 12.5, 30.5, 12.5, 60.5, 5.005, 0.5, 10.5,
       10, 1, 1, 60.5, 5.005, -7.5, -20, -5, -5, 40, -15, -8.5, -8.5, -12.5, -12.5, 10.5, 10.5, 375,
       600, 2000, 3000)




n=10
p=0.2
sum_stat_obs=c(0)

phenofit_priors <- define_priors(param_lb, param_ub, param_distrib, param_fixed)

# ABC_rej <- ABC_rejection(model=phenofit_ABC, prior=phenofit_priors,
#                          nb_simul=n, summary_stat_target=sum_stat_obs, tol=p, n_cluster=20,
#                          use_seed=TRUE)

ABC_mcmc <- ABC_mcmc(method="Marjoram", 
                     model=phenofit_ABC, prior=phenofit_priors, 
                     summary_stat_target = sum_stat_obs, n_rec=n, n_between_sampling=10,
                     n_calibration = 1000, tolerance_quantile = 0.1, proposal_phi = 1,
                     n_cluster = 2, 
                     use_seed = TRUE)
