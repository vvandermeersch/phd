
#-------------------------------#
# Fig.1: climatic dissimilarity #
#-------------------------------#

# past climatic hypervolume overlap
past_climdiss <- readRDS(file.path(wd, "data/climate/metrics","HadCM3B_hypervolume_similarity_CRUbaseline.rds"))
names(past_climdiss) <- paste0("clim_hpv_sorensen.",names(past_climdiss))

# future climatic hypervolume overlap, per GCM per scenario
future_climdiss_gcm <- readRDS(file.path(wd, "data/climate/metrics","CMIP6_hypervolume_similarity_CRUbaseline.rds")) %>% 
  dplyr::filter(model != "TaiESM1" & model != "EC-Earth3")

# future climatic hypervolume overlap, per scenario
# future_climdiss_ssp <- readRDS(file.path(wd, "data/climate/metrics","CMIP6_hypervolume_similarity_perscenario_CRUbaseline.rds"))
future_climdiss_ssp <- future_climdiss_gcm %>%
  group_by(year, scenario) %>%
  dplyr::summarise(median_ssp = median(median), q5 = quantile(median, 0.05), q95 = quantile(median, 0.95))

scenario_names <- c(
  `ssp245` = "SSP245",
  `ssp585` = "SSP585"
)

past_plot <- ggplot() +
  # geom_hline(aes(yintercept = c(0.25)), linetype = "dashed", color = "#f69320", size = 0.4)  +
  # geom_hline(aes(yintercept = c(0.32)), linetype = "dashed", color = "#bf1d1e", size = 0.4) +
  
  geom_ribbon(data = past_climdiss, aes(x = clim_hpv_sorensen.year, 
                                        ymin = 1-clim_hpv_sorensen.q5, ymax = 1-clim_hpv_sorensen.q95), 
              fill = "#6867ac", alpha = 0.2) + 
  geom_line(data = past_climdiss,
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.median), col = "#6867ac") +
  coord_cartesian(xlim = c(12000, 400), 
                  ylim =  c(0, 0.6),
                  clip = "on") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "Years (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,0.6,0.1),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank()) +
  
  # ssp2 - 2060 (using geom_star to rotate triangle...)
  geom_text(aes(x = 5000, 0.256, label = "SSP2"), col = "#f69320", size = 2.7, family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 11800, y = 0.279), col = "black", fill = "#f69320", angle = -90, starshape = 11, size = 2.4) +
  
  # ssp5 - 2060
  geom_text(aes(x = 5000, 0.355, label = "SSP5"), col = "#bf1d1e", size = 2.7, family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 11800, y = 0.332), col = "black", fill = "#bf1d1e", angle = -90, starshape = 11, size = 2.4) +
  
  # early holocene
  geom_text(aes(x = 10200, 0.08, label = "Early\nHolocene"), col = "#6867ac", size = 2.5, family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 11800, y = 0.13), col = "black", fill = "#6867ac", angle = -90, starshape = 11, size = 2.4)



future_plot_ssp <- ggplot() +
  geom_segment(aes(y = 0.279, yend = 0.279, x = 1866, xend = 2060), 
               linetype = "dashed", color = "#f69320", size = 0.3, alpha = 0.7) +
  geom_segment(aes(y = 0.332, yend = 0.332, x = 1866, xend = 2060), 
               linetype = "dashed", color = "#bf1d1e", size = 0.3, alpha = 0.7) +
  geom_segment(aes(y = 0.13, yend = 0.13, x = 1866, xend = 1907), 
               linetype = "dashed", color = "#6867ac", size = 0.3, alpha = 0.7) +
  
  geom_segment(aes(y = 0.004, yend = 0.332, x = 2060, xend = 2060), 
               linetype = "dotted", color = "#737272", size = 0.3, alpha = 0.7) +
  geom_segment(aes(y = 0.004, yend = 0.13, x = 1906, xend = 1906), 
               linetype = "dotted", color = "#6867ac", size = 0.3, alpha = 0.7) +
  
  geom_ribbon(data = future_climdiss_ssp, aes(x = year, ymin = 1-q5, ymax = 1-q95, fill = scenario), 
              alpha = 0.2) + 
  geom_line(data = future_climdiss_ssp, aes(x = year, y = 1-median_ssp, col = scenario)) +
  # geom_line(data = future_climdiss_ssp, aes(x = year, y = 1-mean, col = scenario), linetype = "dashed") +
  scale_color_manual(breaks= c('ssp245', "ssp585"),
                     values= c("#f69320", "#bf1d1e")) +
  scale_fill_manual(breaks= c('ssp245', "ssp585"),
                    values= c("#f69320", "#bf1d1e")) +
  # geom_hline(yintercept = trunc((2-critpoint_csdm)*100)/100, linetype = "dotted") + 
  # geom_bar(data = future_contribution,
  #          aes(x = year, fill = var, weight = contrib), position = "fill",
  #          width = 1) +
  coord_cartesian(xlim = c(2020-2, 2100), 
                  ylim =  c(0, 0.6),
                  clip = "off") +
  scale_x_continuous(breaks = seq(2020,2100, 20),
                     expand = c(0, 0),
                     name = "Years (AD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,0.75, 0.25),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        plot.margin = margin(r = 8.5, b = 5.5, t = 5.5, l = 5.5),
        legend.position="none", legend.title=element_blank())


fig1_main <- ggdraw() +
  draw_plot(past_plot, x = 0, y = 0, width = 0.65, height = 1) +
  draw_plot(future_plot_ssp, x = 0.65, y = 0, width = 0.35, height = 1) +
  annotate("segment", x = 0.646, xend = 0.654,  y = 0.148, yend = 0.172, color = "black") +
  annotate("segment", x = 0.652, xend = 0.66,  y = 0.148, yend = 0.172, color = "black")
