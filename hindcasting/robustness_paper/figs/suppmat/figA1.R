#---------------------------------#
# Figure A1: past climate overview #
#---------------------------------#

#------------------------#
# 1. Setup and load data #
#------------------------#

years <- c(15, seq(250,18000,250))

# 1.1. Load temperature
hadcm3b_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/025deg"
past_temp <- sapply(years, function(yr){
  data <- readRDS(file.path(hadcm3b_dir, paste0("predictors_", yr, "BP.rds"))) %>% 
    dplyr::select(bio1, bio5)
  return(c(mean = mean(data$bio1), 
           q25 = as.numeric(quantile(data$bio1, 0.25)), 
           q75 = as.numeric(quantile(data$bio1, 0.75)),
           meanmax = mean(data$bio5)))
})
pasttemp_df <- as.data.frame(cbind(year = years, t(past_temp)))
pasttemp_df[pasttemp_df$year == 15, "year"] <- 0

# 1.2. Load climatic distance
burke_climatenovelty <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/past_climatenovelty.rds")
burke_climatenovelty[burke_climatenovelty$year == 15, "year"] <- 0
CRU_reference <- burke_climatenovelty[burke_climatenovelty$year == 0,]

# 1.3. Load NGRIP data
d18O_data <- data.frame(fread("D:/climate/NGRIP/ngrip-d18o-50yr.txt", skip = 79, dec = "."))
d18O_data$Age <- as.numeric(gsub(",", "", d18O_data$Age)) # comma as thousand sep
d18O_data$Age <- as.numeric(d18O_data$Age)-50 #ageBP
d18O_data$d18O <- as.numeric(d18O_data$d18O)
d18O_data <- d18O_data[d18O_data$Age <= 21000 & d18O_data$Age >= 0,] %>%
  group_by(Age) %>%
  dplyr::summarize(d18O = mean(d18O, na.rm=TRUE))


#-------------------#
# 2. Prepare figure #
#-------------------#

## 2.1. Main part of the plot
main_plot <- ggplot() +
  
  geom_rect(data = past_climdiss, aes(xmin = clim_hpv_sorensen.year-125, xmax = clim_hpv_sorensen.year+125,
                                             ymin = -3.4, ymax = 12,
                                             fill = 1-clim_hpv_sorensen.median, 
                                      color = 1-clim_hpv_sorensen.median), alpha = 1) +
  scale_fill_gradientn(colours = c("#cee5f2", "#accbe1", "#7c98b3", "#637081"),
                       limits = c(0,0.7),
                       breaks = c(0,0.25, 0.5, 0.7)) +
  scale_color_gradientn(colours = c("#cee5f2", "#accbe1", "#7c98b3", "#637081"),
                        limits = c(0,0.7),
                        breaks = c(0,0.25, 0.5, 0.7), guide = 'none') +
  
  geom_hline(aes(yintercept= c(0, 5, 10)), color = "white", alpha = 0.5, linetype = "dotted", size = 0.3) +
  
  geom_segment(aes(x = 14700, xend = 14700, y = -3.4, yend = 13), 
               color ="white", size = 0.7, linetype = "dashed") +
  geom_segment(aes(x = 12900, xend = 12900, y = -3.4, yend = 13), 
               color ="white", size = 0.7, linetype = "dashed") +
  geom_segment(aes(x = 11700, xend = 11700, y = -3.4, yend = 13.2), color ="white", size = 0.85) +
  
  geom_segment(aes(x = 11700, xend = 11480, y = 12, yend = 12.85), color ="black", size = 0.8) +
  geom_segment(aes(x = 11480, xend = 11700, y = 12.75, yend = 13.7), color ="black", size = 0.8) +
  
  geom_ribbon(data = burke_climatenovelty, aes(x = year, ymin = tmean_q25, ymax = tmean_q75), fill = "#b7efc5", alpha = 0.5) +
  geom_line(data = burke_climatenovelty, aes(x = year, y = tmean_q25), col = "#25a244", size = 0.5) +
  geom_line(data = burke_climatenovelty, aes(x = year, y = tmean_q75), col = "#25a244", size = 0.5) +
  geom_line(data = burke_climatenovelty, aes(x = year, y = tmean), col = "#2c6e49", size = 1) +
  
  coord_cartesian(xlim=c(17050, 950)) +
  
  scale_y_continuous(expand = c(0, 0), labels = ~sub("-", "-", .x)) +
  scale_x_reverse(breaks = seq(16000, 1000, -2000)) +
  
  labs(y = "Annual mean temperature  (deg. C)", x = "Years (BP)", fill = "Climatic dissimilarity") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 0.7, linetype = "solid"),
        axis.ticks.y = element_line(colour = "black", size = 0.7, linetype = "solid"),
        axis.line.x = element_line(colour = "black", size = 0.6, linetype = "solid"),
        axis.ticks.length=unit(.1, "cm"),
        axis.text = element_text(colour = "black", family= "Helvetica", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica", size = 8),
        axis.title.y = element_text(margin = margin(r = 4.5)),
        axis.title.x = element_text(margin = margin(t = 4.5)),
        legend.text = element_text(colour = "black", family= "Helvetica", size = 7, margin = margin(t = -2)),
        legend.title = element_text(colour = "black", family= "Helvetica", size = 7),
        legend.title.align = 0.5, 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(0.8, 'cm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.3, l = 0.3, unit='cm'),
        legend.position=c(.827,.135),
        legend.background = element_rect(colour="black", fill="white")) +
  
  annotate("text", x = 14800, y = 12.8, label = "Late Pleistocene", color = "black", family= "Helvetica", size = 3.3) +
  annotate("text", x = 6000, y = 12.8, label = "Holocene", color = "black", family= "Helvetica", size = 3.3) +
  
  annotate("text", x = 12300, y = -1, label = "Younger\nDryas", color = "white", angle = 90,
           family= "Helvetica Narrow", size = 3.4) +
  annotate("text", x = 13750, y = -1, label = "Bolling-\nAllerod", color = "white", angle = 90,
           family= "Helvetica Narrow", size = 3.4) +
  
  geom_rect(aes(xmin=150, xmax=17850, ymin=-3.4, ymax= 13.7), fill = NA, color = "black", size = 0.8) +
  geom_segment(aes(x = 0, xend = 18000, y = 12, yend = 12), color ="black", size = 0.6) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                               ticks = FALSE))


# 2.2. Evolution of delta O18
d18O_plot <- ggplot() +
  geom_segment(aes(x = 14700, xend = 14700, y = -44, yend = -32.5), 
               color ="grey", size = 0.7, linetype = "dashed") +
  geom_segment(aes(x = 12900, xend = 12900, y = -44, yend = -32.5), 
               color ="grey", size = 0.7, linetype = "dashed") +
  geom_segment(aes(x = 11700, xend = 11700, y = -44, yend = -32.5), 
               color ="grey", size = 0.85) +
  geom_line(data = d18O_data, aes(y = d18O, x = Age), color = "#A22482") +
  geom_rect(aes(xmin=150, xmax=17850, ymin=-44, ymax= -32.5), fill = NA, color = "black", size = 0.8) +
  labs(y = expression(delta^18*O), x = "") +
  scale_y_continuous(limits = c(-44, -32.5), position = "right", breaks = c(-42, -40, -38,  -36, -34),
                     expand = c(0, 0)) +
  scale_x_reverse(breaks = c(15000, 10000, 5000, 0)) +
  coord_cartesian(xlim=c(17050, 950)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = "black", size = 0.7, linetype = "solid"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(colour = "black", family= "Helvetica", size = 8),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(colour = "black", family= "Helvetica", size = 8,
                                          margin = margin(l = 5)))

# 2.3. Assemble!
figA1_main <- plot_grid(
  d18O_plot,
  NULL,
  main_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(0.7, -0.15, 1.5)
) 
