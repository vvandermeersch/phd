# Best threshold
wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/phenofit/run/"

load("D:/species/processed/fagus_sylvatica/2000pres_2000abs/occurrence_subset_1.Rdata")

output_folder <- paste0(wd, "output_4000_0404_cal_")
fitness <- read_mean_outputvalue(1, output_folder, output_var = "Fitness")

auc(roc(fitness, factor(fagussylvatica_occ$pres)))

youden_index <- sensitivity(fitness, factor(fagussylvatica_occ$pres))$measure +
  specificity(fitness, factor(fagussylvatica_occ$pres))$measure -1

thresholds <- roc(fitness, factor(fagussylvatica_occ$pres))$cutoffs
best_threshold <- thresholds[which(youden_index == max(youden_index))]

data_pres_map <- data_fitness_4000
data_pres_map[data_pres_map$fitness < best_threshold, 'value'] <- 0
data_pres_map[data_pres_map$fitness >= best_threshold, 'value'] <- 1

presence_map <- ggplot(data=data_pres_map, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = factor(value))) +
  geom_tile(data = fagussylvatica_occ[fagussylvatica_occ$pres==1,], aes(x = lon, y = lat), fill = "blue", alpha=1) +
  geom_tile(data = fagussylvatica_occ[fagussylvatica_occ$pres==0,], aes(x = lon, y = lat), fill = "black", alpha=1) +
  theme_void() +
   theme(legend.position="bottom") +
   theme(legend.title=element_blank()) +
   ylab("") +
   xlab("") +
   scale_fill_manual(values = alpha(c("grey", "darkgreen"), c(0.3,0.4)))
