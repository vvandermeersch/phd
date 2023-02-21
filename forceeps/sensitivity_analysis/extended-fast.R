
# Setup
wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/forceeps"
source(file.path(wd, "sensitivity_analysis", "setup.R"))





output_path <- "C:/Users/vandermeersch/Documents/CEFE/thesis/forceeps/sensitivity_analysis/fast99_model_simulations"

# commandfile_options$grid <- grid_points[100]
# mat <- matrix(c(50, 30, 80, 100, 100, -40, -10, 20, 45, 0.2), nrow=1)
#site <- grid_points[100]
var <- "adultTreeBasalArea"


# observations
load("D:/species/processed/fagus_sylvatica/fagussylvatica_occ_rs.Rdata")
species_occurrence <- as.data.frame(fagussylvatica_occ_rs, xy=T)
names(species_occurrence)[1:2] <- c("lon", "lat")
species_occurrence$lat <- round(species_occurrence$lat, 1)
species_occurrence$lon <- round(species_occurrence$lon, 1)
species_occurrence <- inner_join(alt_subset, species_occurrence, by = c("lat", "lon"))
species_occurrence$pres <- 0
species_occurrence[species_occurrence$nb_src>0, "pres"] <- 1
Yobs <- species_occurrence


# parameters 
n_parameters <- 11
parameters <- c("kS", "kHMax", "kAMax", "kG", "kDDMin", "kWiTN", "kWiTX", "kTHot", "kTCo2Hot", "kDrTol", "kLa")
parameters_lb <- c(20, 10, 50, 10, 100, -50, -20, 10, 40, 0.01, 1)
parameters_ub <- c(200, 100, 3000, 1000, 2000, 10, 30, 40, 50, 1, 9)
parameters_fixed <- c("kType", "kNTol", "kBrow", "kLy",  "kLQ", "kImmT", "ageMaturityForReproduction")

library(sensitivity)
x <- fast99(forceeps_model_gsa_auc, factors = parameters, n = 800, q = rep("qunif", 11), 
            q.arg = lapply(1:n_parameters, function(i){return(list(min = parameters_lb[i], max = parameters_ub[i]))}))



y <- morris(model = forceeps_model_gsa_auc, factors = parameters, r = 2,
            binf = parameters_lb, bsup = parameters_ub,
            design = list(type = "simplex", scale.factor = 1))


# notes : adapter l'analyse de sensibilit? ? l'AUC
# avec m?thode de Morris, possibilit? que le mod?le sorte des matrices... mais + long ? tourner forc?ment
# pour les param?tres entiers (comme kNtol), possibilit? de tirer dans une loi binomiale 
# 
