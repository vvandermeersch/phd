# Script to generate plots from fitness values

# AUC on pres/abs points 
auc_presabs <- auc(roc(fitness_presabs, as.factor(points_presabs$pres)))

# Way to get lat/lon
climate_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed"
alt_file <- paste0(climate_folder, "/ERA5LAND_", "Altitude.fit")
alt <- fread(alt_file, showProgress=F)
colnames(alt) <- c("lat", "lon", "alt")
alt$lat <- round(alt$lat, 1)
alt$lon <- round(alt$lon, 1)

# Data to plot
data_fitness <- as.data.frame(cbind(alt$lat, alt$lon,t(fitness)))
colnames(data_fitness) <- c("lat", "lon", "fitness")

# Create fitness plot
plot <- ggplot(data=data_fitness, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = fitness), color = NA) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_gradient(low = "#EBEBD3", high = "#488B49", limits=c(0,1)) +
  theme(legend.key.height= unit(0.1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  annotate("text", x = -7, y = 77, label = paste("Iteration", parse_number(rds)), colour = 'darkgrey') +
  annotate("text", x = -7, y = 75, label = paste("AUC =", round(auc_presabs, 2)), colour = 'darkgrey') 
  
# Save plot
plotname <- file.path(wd, "img", paste0(strsplit(filename, "_")[[1]][3], "._fitness.png"))
ggsave(plot = plot, 
       filename = plotname, 
       device = "png",
       height = 5.71,
       width = 5.6,
       dpi = 100
       )

# Best threshold for AUC
youden_index <- sensitivity(fitness_presabs, as.factor(points_presabs$pres))$measure +
  specificity(fitness_presabs, as.factor(points_presabs$pres))$measure -1
thresholds <- roc(fitness_presabs, as.factor(points_presabs$pres))$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

# Presence/absence plot
# data_presence <- data_fitness
# data_presence[data_presence$fitness < best_threshold, 'presence'] <- 0
# data_presence[data_presence$fitness >= best_threshold, 'presence'] <- 1
# plot <- ggplot(data=data_presence, aes(x = lon, y = lat)) + 
#   geom_tile(aes(fill = presence), color = NA) +
#   theme_void() +
#   theme(legend.position="none") +
#   ylab("") +
#   xlab("") +
#   scale_fill_gradient(low = "#EBEBD3", high = "#488B49") +
#   annotate("text", x = -7, y = 77, label = paste("Iteration", parse_number(rds)), colour = 'darkgrey') +
#   annotate("text", x = -7, y = 75, label = paste("AUC =", round(auc_presabs, 2)), colour = 'darkgrey')
# 
# # Save plot
# plotname <- file.path(wd, "img", paste0(strsplit(filename, "_")[[1]][3], "_presabs.png"))
# ggsave(plot = plot, 
#        filename = plotname, 
#        device = "png",
#        height = 5.71,
#        width = 5.6,
#        dpi = 100
# )
