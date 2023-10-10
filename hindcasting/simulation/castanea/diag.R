year <- 500

pres <- readRDS("D:/species/pollen/processed/fagus/025deg/001thr_500yrunc/pres_500BP.rds")

output_folder <- "D:/simulations/castanea/paleo/fitted/025deg/fagus_sylvatica"
biomass_fitted <- read_mean_outputvalue(output_folder = file.path(output_folder, paste0(year, "BP_15yrinv")),
                                 model = "CASTANEA", output_var = "BiomassOfReserves",
                                 year = year, num_years = 30)

output_folder <- "D:/simulations/castanea/paleo/expert/025deg/quercus_robur/32yr_inventory"
biomass_expert <- read_mean_outputvalue(output_folder = file.path(output_folder, paste0(year, "BP")),
                                        model = "CASTANEA", output_var = "rw",
                                        year = year, num_years = 30)


plot(rast(biomass_expert[c(2,1,3)]))
points(pres[pres$pres == 1,])
points(pres[pres$pres == 0,], pch = 4)
plot(rast(biomass_fitted[c(2,1,4)]))

fitness <- biomass_expert
fitness$lat <- round(fitness$lat,2)
fitness$lon <- round(fitness$lon,2)
fitness <- na.omit(dplyr::left_join(fitness, pres, by = c("lat", "lon")))
eval_obj <- precrec::evalmod(scores = fitness$value, labels = fitness$pres)
aucroc <- precrec::auc(eval_obj)



# points near Orleans
year <- 500
output_folder <- "D:/simulations/castanea/paleo/expert/025deg/quercus_robur/32yr_inventory"
output_folder <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/castanea/clim_test/false/output"
points(x = 1.96, y = 47.87, pch = 21, bg="blue", cex = 2) # cell 8905 for 500BP, cell 8864 for 500BP
data_past <- fread(file.path(output_folder,  paste0(year, "BP"), "8905_ind1_yearlyResults.log"))
cat(paste0(data_past$Y[1], " ", data_past$X[1]))

output_folder <- "D:/simulations/castanea/forward/quercus_robur"
points(x = 1.9, y = 47.8, pch = 21, bg="blue", cex = 2) # cell 67763
data_present <- fread(file.path(output_folder,   "67763_ind1_yearlyResults.log")) 
cat(paste0(data_present$Y[1], " ", data_present$X[1]))

ET_past <- ggplot(data = data_past) +
  geom_line(aes(x = -year, y = ETP, col = "#DBD56E"), linewidth = 1) +
  geom_line(aes(x = -year, y = ETR, col = "#88AB75"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("ETR", "ETP")) +
  scale_x_reverse() + 
  labs(x = "Years BP", y = "Evapotranspiration", color = NULL) +
  ylim(200, 1600) +
  theme(legend.margin=margin(t = 0, b=0.1, r = 0.2, l = 0.2, unit='cm'),
        legend.position=c(.2,.85),
        legend.background = element_rect(colour="black", fill="white"))

ET_present <- ggplot(data = data_present) +
  geom_line(aes(x = year, y = ETP, col = "#DBD56E"), linewidth = 1) +
  geom_line(aes(x = year, y = ETR, col = "#88AB75"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("ETR", "ETP")) +
  labs(x = "Years", y = "Evapotranspiration", color = "") +
  ylim(200, 1600)

pre_past <- ggplot(data = data_past) +
  geom_line(aes(x = year, y = PRI, col = "#DBD56E"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("PRI")) +
  labs(x = "Years BP", y = "Precipitation", color = "") +
  ylim(350, 1000)

pre_present <- ggplot(data = data_present) +
  geom_line(aes(x = year, y = PRI, col = "#DBD56E"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("PRI")) +
  labs(x = "Years", y = "Precipitation", color = "") +
  ylim(350, 1000) 

biomass_past <- ggplot(data = data_past) +
  geom_line(aes(x = -year, y = BiomassOfReserves, col = "#DBD56E"), linewidth = 1) +
  geom_line(aes(x = -year, y = AliveWoodBiomass, col = "#88AB75"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("Alive wood", "Reserves")) +
  scale_x_reverse() + 
  labs(x = "Years BP", y = "Biomass", color = NULL) +
  ylim(-50, 150) +
  theme(legend.margin=margin(t = 0, b=0.1, r = 0.2, l = 0.2, unit='cm'),
        legend.position=c(.25,.85),
        legend.background = element_rect(colour="black", fill="white"))

biomass_present <- ggplot(data = data_present) +
    geom_line(aes(x = year, y = BiomassOfReserves, col = "#DBD56E"), linewidth = 1) +
    geom_line(aes(x = year, y = AliveWoodBiomass, col = "#88AB75"), linewidth = 1) +
    theme_bw() +
    scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("Alive wood", "Reserves")) +
    labs(x = "Years", y = "Biomass", color = "") +
  ylim(-50, 150)

stress_past <- ggplot(data = data_past) +
  geom_line(aes(x = -year, y = StressLevel, col = "#DBD56E"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("Stress level")) +
  scale_x_reverse() + 
  labs(x = "Years BP", y = "Soil water stress index", color = "") +
  ylim(-150, -20)


stress_present <- ggplot(data = data_present) +
    geom_line(aes(x = year, y = StressLevel, col = "#DBD56E"), linewidth = 1) +
    theme_bw() +
    scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("Stress level")) +
    labs(x = "Years", y = "Soil water stress index", color = "") +
  ylim(-150, -20)


plot <- plot_grid(ET_past, pre_past + theme(legend.position = "none"), 
          biomass_past, stress_past + theme(legend.position = "none"),
          ET_present + theme(legend.position = "none"), pre_present + theme(legend.position = "none"), 
          biomass_present + theme(legend.position = "none"), stress_present + theme(legend.position = "none"), 
          rel_heights = c(1,1),
          ncol = 4)

ggsave(filename="C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/castanea/diag_orleans_quercus_32yr_modcode.pdf", 
       plot=plot , height=8, width=14)



climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/castanea_format/025deg/500BP"
data <- fread(file.path(climate_folder, "8905.txt")) %>%
  group_by(`# y`) %>%
  summarize(ws_max = max(ws), ws = mean(ws))
ggplot(data = data) +
  geom_line(aes(x = -`# y`, y = ws, col = "#DBD56E"), linewidth = 1) +
  geom_line(aes(x = -`# y`, y = ws_max, col = "#88AB75"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("Max", "Mean")) +
  scale_x_reverse() + 
  labs(x = "Years BP", y = "Wind", color = "")


climate_folder <- "D:/climate/ERA5-Land/castanea_format"
data <- fread(file.path(climate_folder, "67763.txt")) %>%
  group_by(`# y`) %>%
  summarize(ws_max = max(ws), ws = mean(ws))
ggplot(data = data) +
  geom_line(aes(x = `# y`, y = ws, col = "#DBD56E"), linewidth = 1) +
  geom_line(aes(x = `# y`, y = ws_max, col = "#88AB75"), linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("#DBD56E", "#88AB75"), labels = c("Max", "Mean")) +
  labs(x = "Years BP", y = "Wind", color = "")

