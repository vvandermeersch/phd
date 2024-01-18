
# past climatic hypervolume overlap
past_climdiss <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/hypervolume_similarity_statistics_CRUbaseline.rds")
names(past_climdiss) <- paste0("clim_hpv_sorensen.",names(past_climdiss))

# future climatic hypervolume overlap, per GCM per scenario
future_climdiss_gcm <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/metrics/climate_approach/data/hypervolume_similarity_statistics_CRUbaseline.rds")

# future climatic hypervolume overlap, per scenario
future_climdiss_ssp <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/metrics/climate_approach/data/hypervolume_similarity_statistics_scenario_CRUbaseline.rds")

scenario_names <- c(
  `ssp245` = "SSP245",
  `ssp585` = "SSP585"
)

past_plot <- ggplot() +
  geom_ribbon(data = past_climdiss, aes(x = clim_hpv_sorensen.year, 
                                        ymin = 1-clim_hpv_sorensen.q2.5, ymax = 1-clim_hpv_sorensen.q97.5), 
              fill = "#6867ac", alpha = 0.2) + 
  geom_line(data = past_climdiss,
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "#6867ac") +
  coord_cartesian(xlim = c(12000, 400), 
                  ylim =  c(0, 0.7),
                  clip = "on") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "Years (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank())


future_plot_ssp <- ggplot() +
  geom_ribbon(data = future_climdiss_ssp, aes(x = year, ymin = 1-q2.5, ymax = 1-q97.5, fill = scenario), 
              alpha = 0.2) + 
  geom_line(data = future_climdiss_ssp, aes(x = year, y = 1-mean, col = scenario)) +
  scale_color_manual(breaks= c('ssp245', "ssp585"),
                     values= c("#cb6d18", "#a32a2e")) +
  scale_fill_manual(breaks= c('ssp245', "ssp585"),
                    values= c("#cb6d18", "#a32a2e")) +
  # geom_hline(yintercept = trunc((2-critpoint_csdm)*100)/100, linetype = "dotted") + 
  # geom_bar(data = future_contribution,
  #          aes(x = year, fill = var, weight = contrib), position = "fill",
  #          width = 1) +
  coord_cartesian(xlim = c(2005-2, 2095+3), 
                  ylim =  c(0, 0.7),
                  clip = "off") +
  scale_x_continuous(breaks = seq(2010,2095, 20),
                     expand = c(0, 0),
                     name = "Years (AD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank())


fig1_final <- ggdraw() +
  draw_plot(past_plot, x = 0, y = 0, width = 0.65, height = 1) +
  draw_plot(future_plot_ssp, x = 0.65, y = 0, width = 0.35, height = 1) +
  annotate("segment", x = 0.646, xend = 0.654,  y = 0.148, yend = 0.172, color = "black") +
  annotate("segment", x = 0.652, xend = 0.66,  y = 0.148, yend = 0.172, color = "black") 
  #annotate("segment", x = 0.085, xend = 0.98, y = 0.53, yend = 0.53, color = "grey", linetype = "dashed") +
  #annotate("segment", x = 0.085, xend = 0.98, y = 0.79, yend = 0.79, color = "grey", linetype = "dashed")


future_plot_gcm <- ggplot() +
  geom_ribbon(data = future_climdiss_gcm, aes(x = year, ymin = 1-q2.5, ymax = 1-q97.5, fill = model), 
              alpha = 0.2) + 
  geom_line(data = future_climdiss_gcm, aes(x = year, y = 1-median, col = model)) +
  facet_wrap(~ scenario, labeller = as_labeller(scenario_names)) +
  # scale_color_manual(breaks= c('ssp245', "ssp585"),
  #                    values= c("#f7b763", "#ac6867")) +
  # scale_fill_manual(breaks= c('ssp245', "ssp585"),
  #                   values= c("#f7b763", "#ac6867")) +
  # geom_hline(yintercept = trunc((2-critpoint_csdm)*100)/100, linetype = "dotted") + 
  # geom_bar(data = future_contribution,
  #          aes(x = year, fill = var, weight = contrib), position = "fill",
  #          width = 1) +
  coord_cartesian(xlim = c(2005-2, 2095+3), 
                  ylim =  c(0, 0.8),
                  clip = "off") +
  scale_x_continuous(breaks = seq(2010,2095, 20),
                     expand = c(0, 0),
                     name = "Years (AD)") +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                     expand = expansion(mult = c(0, 0)),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        legend.position="bottom", legend.title=element_blank(), legend.key.size = unit(0.4, "cm"),
        legend.margin=margin(l = -0.3, unit='cm'))



past_plotbis <- ggplot() +
  geom_ribbon(data = past_climdiss, aes(x = clim_hpv_sorensen.year, 
                                        ymin = 1-clim_hpv_sorensen.q2.5, ymax = 1-clim_hpv_sorensen.q97.5), 
              fill = "#6867ac", alpha = 0.2) + 
  geom_line(data = past_climdiss,
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.median), col = "#6867ac") +
  coord_cartesian(xlim = c(11500, 400), 
                  ylim =  c(0, 0.8),
                  clip = "off") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "Years (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank())

future_plot_sspgcm <- ggplot() +
  geom_ribbon(data = future_climdiss_gcm, aes(x = year, ymin = 1-q2.5, ymax = 1-q97.5, fill = scenario, group = paste0(scenario,model)),
              alpha = 0.2) +
  geom_line(data = future_climdiss_gcm, aes(x = year, y = 1-median, col = scenario, group = paste0(scenario,model))) +
  scale_color_manual(breaks= c('ssp245', "ssp585"),
                     values= c("#f7b763", "#ac6867")) +
  scale_fill_manual(breaks= c('ssp245', "ssp585"),
                    values= c("#f7b763", "#ac6867")) +
  # geom_hline(yintercept = trunc((2-critpoint_csdm)*100)/100, linetype = "dotted") + 
  # geom_bar(data = future_contribution,
  #          aes(x = year, fill = var, weight = contrib), position = "fill",
  #          width = 1) +
  coord_cartesian(xlim = c(2005-2, 2095+3), 
                  ylim =  c(0, 0.8),
                  clip = "off") +
  scale_x_continuous(breaks = seq(2010,2095, 20),
                     expand = c(0, 0),
                     name = "Years (AD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank())


fig1_final_bis <- ggdraw() +
  draw_plot(past_plotbis, x = 0, y = 0, width = 0.65, height = 1) +
  draw_plot(future_plot_sspgcm, x = 0.65, y = 0, width = 0.35, height = 1) +
  annotate("segment", x = 0.646, xend = 0.654,  y = 0.148, yend = 0.172, color = "black") +
  annotate("segment", x = 0.652, xend = 0.66,  y = 0.148, yend = 0.172, color = "black") 
#annotate("segment", x = 0.085, xend = 0.98, y = 0.53, yend = 0.53, color = "grey", linetype = "dashed") +
#annotate("segment", x = 0.085, xend = 0.98, y = 0.79, yend = 0.79, color = "grey", linetype = "dashed")