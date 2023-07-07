source(file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation", "functions/read_mean_outputvalue.R"))

# extents
carpats <- ext(c(22,27,45,48))
ore_mountains <- ext(c(11,14,49.5,51))
cantabrian_range <- ext(-7, -4, 42.5, 43.5)

regions <- c("Carpats" = carpats, "Ore Mountains" = ore_mountains, "Cantabrian range" = cantabrian_range)

# presence
presence <- rast(readRDS("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds"))
presence[presence == 0 ] <- NA

# forward
sim_folder <- "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch"
best_threshold <- 0.162
fitness <- rast(read_mean_outputvalue(sim_folder, output_var = "Fitness")[c(2,1,3)])
fitness[fitness >= best_threshold] <- NA # remove areas where species is predicted as present

survival <- mask(crop(rast(read_mean_outputvalue(sim_folder, output_var = "Survival")[c(2,1,3)]), presence), presence)
fruitindex <- mask(crop(rast(read_mean_outputvalue(sim_folder, output_var = "FruitIndex")[c(2,1,3)]), presence), presence)
maturationindex <- mask(crop(rast(read_mean_outputvalue(sim_folder, output_var = "MaturationIndex")[c(2,1,3)]), presence), presence)

expert_data <- do.call(rbind, lapply(1:length(regions), function(i){
  surv <- as.data.frame(crop(survival, regions[[i]]))
  surv$metric <- "1_Survival"
  surv$mod <- "Expert"
  surv$region <- names(regions[i])
  
  survfruit <- as.data.frame(crop(survival*fruitindex, regions[[i]]))
  survfruit$metric <- "2_Survival_x_FruitIndex"
  survfruit$mod <- "Expert"
  survfruit$region <- names(regions[i])
  
  fitn <- as.data.frame(crop(survival*fruitindex*maturationindex, regions[[i]]))
  fitn$metric <- "3_Fitness"
  fitn$mod <- "Expert"
  fitn$region <- names(regions[i])
  
  return(rbind(surv, survfruit, fitn))
}))





# backward
sim_folder_fitted <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES/subset4_rep1"
best_threshold_fitted <- 0.785
survival_fitted <- mask(crop(rast(read_mean_outputvalue(sim_folder_fitted, output_var = "Survival")[c(2,1,3)]), presence), presence)
fruitindex_fitted <- mask(crop(rast(read_mean_outputvalue(sim_folder_fitted, output_var = "FruitIndex")[c(2,1,3)]), presence), presence)
maturationindex_fitted <- mask(crop(rast(read_mean_outputvalue(sim_folder_fitted, output_var = "MaturationIndex")[c(2,1,3)]), presence), presence)

fitted_data <- do.call(rbind, lapply(1:length(regions), function(i){
  surv <- as.data.frame(crop(survival_fitted, regions[[i]]))
  surv$metric <- "1_Survival"
  surv$mod <- "Fitted"
  surv$region <- names(regions[i])
  
  survfruit <- as.data.frame(crop(survival_fitted*fruitindex_fitted, regions[[i]]))
  survfruit$metric <- "2_Survival_x_FruitIndex"
  survfruit$mod <- "Fitted"
  survfruit$region <- names(regions[i])
  
  fitn <- as.data.frame(crop(survival_fitted*fruitindex_fitted*maturationindex_fitted, regions[[i]]))
  fitn$metric <- "3_Fitness"
  fitn$mod <- "Fitted"
  fitn$region <- names(regions[i])
  
  return(rbind(surv, survfruit, fitn))
}))


data_plot <- rbind(expert_data, fitted_data)


# code by Lizzie
threemetricsplot <- ggplot(data_plot, aes(y=value, x=as.character(mod))) +
  geom_violin(trim=FALSE, col="lightgray") + 
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "dodgerblue", lwd=1
  ) +
  facet_wrap(region~metric, scales="free") +
  geom_segment(x = 0.55, y = best_threshold, xend = 1.45, yend = best_threshold, linetype='dotted', col = 'red') +
  geom_segment(x = 1.55, y = best_threshold_fitted, xend = 2.45, yend = best_threshold_fitted, linetype='dotted', col = 'red') +
  ylim(-0.4, 1.4) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename="C:/Users/vandermeersch/Documents/CEFE/phd/present_calibration/contrast_paper/regions/graphs/threemetricsplot_presencemask.pdf", 
       plot=threemetricsplot, height=8, width=12)
