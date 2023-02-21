wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/castanea"
source(file.path(wd, "functions", "read_mean_outputvalue.R"))
library(ggplot2)
library(colorspace)

#grid_points <- 1:101510
#reserve_biomass <- read_mean_outputvalue(grid_points, lambda = 1, output_folder = 'D:/simulations/castanea/first_tests/30-03', var = "BiomassOfReserves")
#reserve_biomass <- t(reserve_biomass)



# Temporary : when all points did not run (because of memory error)
load("D:/soil/processed/data_soil.Rdata")

filenames <- list.files('D:/simulations/castanea/first_tests/30-03', pattern="\\yearlyResults.log", full.names=TRUE)
biomass <- sapply(filenames, function(file_to_read){
  ind <- paste0("ind", l, "_")
  #ind <- "Test_V_"
  var_table <- read.table(file_to_read, sep=";", header=T)
  return(mean(var_table[, var]))
})
reserve_biomass <- t(as.matrix(biomass))
rownames(reserve_biomass)<-NULL



data_plot <- data.frame(data$lat, data$lon, reserve_biomass)
names(data_plot) <- c("lat", "lon", "reserve_biomass")


rbio_map <- ggplot(data=data_plot, aes(x = lon, y = lat)) + 
  geom_tile(aes(fill = reserve_biomass)) +
  theme_void() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) +
  ylab("") +
  xlab("") +
  scale_fill_continuous_divergingx(palette = 'Fall', rev=TRUE, mid = 0, limits=c(0,1))