#-------------------------------------------------------------------------------#
# Script to prepare figure with mean temperature, climatic distance and CO2 ppm #
#-------------------------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(cowplot)
library(extrafont)

years <- c(15, seq(1000,15000,1000), 17000, 19000, 21000)
hadcm3b_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format"


# load climatic distance
climdist_df <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_novelty/save/predictors_climdist.rds")
climdist_df[climdist_df$year == 15, "year"] <- 0


# load temperature
past_temp <- sapply(years, function(yr){
  data <- readRDS(file.path(hadcm3b_dir, paste0("predictors_", yr, "BP.rds"))) %>% 
    dplyr::select(bio1)
  return(c(mean = mean(data$bio1), 
           q25 = as.numeric(quantile(data$bio1, 0.25)), 
           q75 = as.numeric(quantile(data$bio1, 0.75))))
})
pasttemp_df <- as.data.frame(cbind(year = years, t(past_temp)))
pasttemp_df[pasttemp_df$year == 15, "year"] <- 0


# CO2 PPM used as baseline conditions in Armstrong et al. (2019)
CO2_data <- data.frame(year = seq(0,21000,1000),
                       conc = c(280, 279, 277, 275, 273,268, 265, 261, 261, 265, 267, 264,
                                245, 238, 237, 224, 210, 194, 189, 188, 188, 186))


# main part of the plot
main_plot <- ggplot() +
  geom_rect(data = climdist_df, aes(xmin = year-500, xmax = year+500,
                                    ymin = -23, ymax = 11,
                                    fill = median, color = median), alpha = 1) +
  scale_fill_gradientn(colours = c("#cee5f2", "#accbe1", "#7c98b3", "#637081"),
                       limits = c(0,1.5),
                       breaks = c(0,0.5,1,1.5)) +
  scale_color_gradientn(colours = c("#cee5f2", "#accbe1", "#7c98b3", "#637081"),
                       limits = c(0,1.5),
                       breaks = c(0,0.5,1,1.5), guide = 'none') +
  geom_hline(aes(yintercept= c(0, -10, -20, 10)), color = "white", alpha = 0.5) +
  geom_segment(aes(x = 14700, xend = 14700, y = -23, yend = 11), 
               color ="white", size = 0.8, linetype = "dashed") +
  geom_segment(aes(x = 12900, xend = 12900, y = -23, yend = 11), 
               color ="white", size = 0.8, linetype = "dashed") +
  geom_segment(aes(x = 11700, xend = 11700, y = -23, yend = 13.7), color ="white", size = 1) +
  geom_segment(aes(x = 11700, xend = 11400, y = 11, yend = 12.4), color ="grey70", size = 0.8) +
  geom_segment(aes(x = 11400, xend = 11700, y = 12.3, yend = 13.7), color ="grey70", size = 0.8) +
  geom_ribbon(data = pasttemp_df, aes(x = year, ymin = q25, ymax = q75), fill = "#b7efc5", alpha = 0.5) +
  geom_line(data = pasttemp_df, aes(x = year, y = q25), col = "#25a244", size = 0.5) +
  geom_line(data = pasttemp_df, aes(x = year, y = q75), col = "#25a244", size = 0.5) +
  geom_line(data = pasttemp_df, aes(x = year, y = mean), col = "#2c6e49", size = 1) +
  coord_cartesian(xlim=c(20050, 950)) +
  scale_y_continuous(expand = c(0, 0), labels = ~sub("-", "-", .x)) +
  scale_x_reverse(breaks = c(20000, 15000, 10000, 5000, 0)) +
  labs(y = "ANNUAL MEAN TEMPERATURE  (°C)", x = "YEARS (BP)", fill = "Climatic distance") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x = element_line(colour = "grey70", size = 0.7, linetype = "solid"),
        axis.ticks.y = element_line(colour = "grey70", size = 0.7, linetype = "solid"),
        axis.line.x = element_line(colour = "grey70", size = 0.7, linetype = "solid"),
        axis.ticks.length=unit(.1, "cm"),
        axis.text = element_text(colour = "grey50", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "grey50", family= "Noto Sans", size = 9),
        axis.title.y = element_text(margin = margin(r = 0)),
        legend.text = element_text(colour = "grey50", family= "Noto Sans", size = 8),
        legend.title = element_text(colour = "grey50", family= "Noto Sans", size = 8),
        legend.title.align = 0.5, legend.position = "bottom",
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(1, 'cm'),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  annotate("text", x = 16500, y = 12.4, label = "LATE PLEISTOCENE", color = "grey60", family= "Noto Sans", size = 4) +
  annotate("text", x = 6000, y = 12.4, label = "HOLOCENE", color = "grey60", family= "Noto Sans", size = 4) +
  annotate("text", x = 12400, y = -15.3, label = "Younger Dryas", color = "white", angle = 90,
           family= "Noto Sans") +
  annotate("text", x = 13900, y = -15.3, label = "Bølling-Allerød", color = "white", angle = 90,
           family= "Noto Sans") +
  geom_rect(aes(xmin=0, xmax=21000, ymin=-23, ymax= 13.7), fill = NA, color = "grey70", size = 0.8) +
  geom_segment(aes(x = 0, xend = 21000, y = 11, yend = 11), color ="grey70", size = 0.6) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "grey70", frame.linewidth = 0.3,
                               ticks = FALSE))



# evolutino of CO2 ppm
CO2_plot <- ggplot() +
  geom_line(data = CO2_data, aes(y = conc, x = year)) +
  geom_point(data = CO2_data, aes(y = conc, x = year), size = 1)+
  geom_rect(aes(xmin=0, xmax=21000, ymin=180, ymax= 285), fill = NA, color = "grey70", size = 0.8) +
  labs(y = "C02 (ppmv)", x = "") +
  scale_y_continuous(limits = c(175, 285), position = "right", breaks = c(275, 250, 225, 200),
                     expand = c(0, 0)) +
  scale_x_reverse(breaks = c(20000, 15000, 10000, 5000, 0)) +
  coord_cartesian(xlim=c(20050, 950)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = "grey70", size = 0.7, linetype = "solid"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(colour = "grey50", family= "Noto Sans", size = 9),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(colour = "grey50", family= "Noto Sans", size = 9,
                                          margin = margin(l = 5), hjust = 0.25))


climateoverview_figure <- plot_grid(
  CO2_plot,
  NULL,
  main_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(0.5, -0.154, 1.5)
) 




