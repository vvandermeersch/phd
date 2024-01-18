
#----------------------------------------#
# Fig.A1: climate dissimilarity, per GCM #
#----------------------------------------#

future_climdiss_gcm <- future_climdiss_gcm %>%
  mutate(label = if_else(year == max(year), as.character(model), NA_character_))

future_plot_sspgcm <- ggplot() +
  geom_ribbon(data = future_climdiss_gcm, aes(x = year, ymin = 1-q2.5, ymax = 1-q97.5, fill = scenario, group = paste0(scenario,model)),
              alpha = 0.2) +
  geom_line(data = future_climdiss_gcm, aes(x = year, y = 1-median, col = scenario, group = paste0(scenario,model))) +
  scale_color_manual(breaks= c('ssp245', "ssp585"),
                     values= c("#f7b763", "#a32a2e")) +
  scale_fill_manual(breaks= c('ssp245', "ssp585"),
                    values= c("#f7b763", "#a32a2e")) +
  coord_cartesian(xlim = c(2005-2, 2095+3), 
                  ylim =  c(0, 0.75),
                  clip = "off") +
  scale_x_continuous(breaks = seq(2010,2095, 20),
                     expand = c(0, 0),
                     name = "Years (AD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climatic dissimilarity") +
  geom_text_repel(data = future_climdiss_gcm,
                  aes(label = label, x = year, y = 1-median, col = scenario),
                  nudge_x = 1,
                  size = 2, 
                  segment.size = 0.2,
                  na.rm = TRUE,
                  xlim = c(2100, 2130)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        plot.margin = margin(r = 110, b = 5.5, t = 5.5, l = 5.5),
        legend.position="none", legend.title=element_blank())

figA2_main <- ggdraw() +
  draw_plot(past_plot, x = 0, y = 0, width = 0.55, height = 1) +
  draw_plot(future_plot_sspgcm, x = 0.55, y = 0, width = 0.55, height = 1) +
  annotate("segment", x = 0.596, xend = 0.604,  y = 0.148, yend = 0.172, color = "black") +
  annotate("segment", x = 0.602, xend = 0.61,  y = 0.148, yend = 0.172, color = "black")
