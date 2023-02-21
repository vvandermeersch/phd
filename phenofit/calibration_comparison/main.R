
wd <- 'C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/calibration_comparison'
source(file.path(wd, "functions", "read_species_file.R"))



# List of CMAES calibrations 
cal_folder <- 'D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs'
cal_list <- c("15-04", "22-04a", "22-04b", "22-04c", "22-04d")

# Bounds
bd_folder <- 'C:/Users/vandermeersch/Dropbox/Thèse_Victor/Phenofit4/species/backward/fagus_sylvatica'
species_lb  <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_lb.species')))
species_ub <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_ub.species')))
species_init <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_init.species')))

species_parameters <- lapply(cal_list, function(x){
  cal_folder <- file.path(cal_folder,x)
  species_file <- list.files(path = cal_folder, pattern = "\\.species$", full.names = T)
  species_values <- data.frame(value = read_species_file(species_file)) %>% 
    mutate_all(as.numeric)
  species_values$var <- rownames(species_values)
  species_values$lb <- species_lb
  species_values$ub <- species_ub
  species_values$init <- species_init
  species_values
  })
species_parameters <- do.call(rbind, species_parameters)
species_parameters <- na.omit(species_parameters) #remove fixed parameters (bounds = NA)

data <- species_parameters %>%
  dplyr::filter(grepl("fruit", species_parameters$var))
par_names <- c("aa", "bb", "Fcrit", "Top", "matmoy", "sigma", "pfe50")
names(par_names) <- unique(data$var)

data %>%
  ggplot(aes(y = value, x = factor(1))) +
  geom_violin(fill = 'lightgrey', alpha= 0.1) +
  geom_point(alpha=0.5, col = 'darkred') + 
  geom_hline(aes(yintercept = init), linetype = 'dashed', col = 'darkred') +
  facet_wrap("var", scales="free",
             labeller= labeller(var = par_names)) +
  geom_blank(aes(y = lb)) +
  geom_blank(aes(y = ub)) +
  theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Parameter value")
  
data <- species_parameters %>%
  dplyr::filter(grepl("fruit", species_parameters$var))
par_names <- c("aa", "bb", "Fcrit", "Top", "matmoy", "sigma", "pfe50")
names(par_names) <- unique(data$var)


data %>%
  ggplot(aes(y = value, x = factor(1))) +
  geom_point(alpha=0.5, col = 'darkred') + 
  geom_violin(fill = 'lightgrey', alpha= 0.1) +
  geom_hline(aes(yintercept = init), linetype = 'dashed', col = 'darkred') +
  facet_wrap("var", scales="free",
             labeller= labeller(var = par_names)) +
  geom_blank(aes(y = lb)) +
  geom_blank(aes(y = ub)) +
  theme_hc() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
