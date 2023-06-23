# main part of the plot
main_plot <- ggplot() +
  geom_rect(data = climdist_df, aes(xmin = year-250, xmax = year+250,
                                    ymin = -15, ymax = 11,
                                    fill = median, color = median), alpha = 1) +
  scale_fill_gradientn(colours = c("#cee5f2", "#accbe1", "#7c98b3", "#637081"),
                       limits = c(1,3),
                       breaks = c(1,2,3)) +
  scale_color_gradientn(colours = c("#cee5f2", "#accbe1", "#7c98b3", "#637081"),
                        limits = c(1,3),
                        breaks = c(1,2,3), guide = 'none') +
  geom_segment(aes(x = 14700, xend = 14700, y = -15, yend = 11), 
               color ="white", size = 0.8, linetype = "dashed") +
  geom_segment(aes(x = 12900, xend = 12900, y = -15, yend = 11), 
               color ="white", size = 0.8, linetype = "dashed") +
  geom_segment(aes(x = 11700, xend = 11700, y = -15, yend = 13.2), color ="white", size = 1) +
  geom_segment(aes(x = 11700, xend = 11600, y = 11, yend = 12.4), color ="black", size = 0.8) +
  geom_segment(aes(x = 11600, xend = 11700, y = 12.3, yend = 13.7), color ="black", size = 0.8) +
  geom_ribbon(data = pasttemp_df[pasttemp_df$year<=15500 & pasttemp_df$year>=9750,], aes(x = year, ymin = q25, ymax = q75), fill = "#b7efc5", alpha = 0.5) +
  geom_line(data = pasttemp_df[pasttemp_df$year<=15500,], aes(x = year, y = q25), col = "#25a244", size = 0.5) +
  geom_line(data = pasttemp_df[pasttemp_df$year<=15500,], aes(x = year, y = q75), col = "#25a244", size = 0.5) +
  geom_line(data = pasttemp_df[pasttemp_df$year<=15500,], aes(x = year, y = mean), col = "#2c6e49", size = 1) +
  coord_cartesian(xlim=c(15050, 10050), ylim = c(-15,14)) +
  scale_y_continuous(expand = c(0, 0), labels = ~sub("-", "-", .x)) +
  scale_x_reverse(breaks =  c(10500, 12000, 13000, 14500, 15000)) +
  labs(y = "ANNUAL MEAN TEMPERATURE  (°C)", x = "YEARS (BP)", fill = "Climatic distance") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x = element_line(colour = "black", size = 0.7, linetype = "solid"),
        axis.ticks.y = element_line(colour = "black", size = 0.7, linetype = "solid"),
        axis.line.x = element_line(colour = "black", size = 0.6, linetype = "solid"),
        axis.ticks.length=unit(.1, "cm"),
        axis.text.y = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.text.x = element_text(colour = "black", family= "Noto Sans", size = 11),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title.y = element_text(margin = margin(r = 0)),
        legend.text = element_text(colour = "black", family= "Noto Sans", size = 8),
        legend.title = element_text(colour = "black", family= "Noto Sans", size = 8),
        legend.title.align = 0.5, legend.position = "bottom",
        legend.key.height = unit(0.3, 'cm'), legend.key.width = unit(1, 'cm'),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  annotate("text", x = 13500, y = 12.4, label = "LATE PLEISTOCENE", color = "black", family= "Noto Sans", size = 4) +
  annotate("text", x = 10700, y = 12.4, label = "HOLOCENE", color = "black", family= "Noto Sans", size = 4) +
  annotate("text", x = 12300, y = -11.3, label = "Younger\nDryas", color = "white", angle = 0,
           family= "Noto Sans") +
  annotate("text", x = 13800, y = -11.3, label = "Bølling-Allerød", color = "white", angle = 0,
           family= "Noto Sans") +
  geom_rect(aes(xmin=9800, xmax=15300, ymin=-15, ymax= 13.7), fill = NA, color = "black", size = 0.8) +
  geom_segment(aes(x = 0, xend = 18000, y = 11, yend = 11), color ="black", size = 0.6) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                               ticks = FALSE))


d18O_plot <- ggplot() +
  geom_segment(aes(x = 14700, xend = 14700, y = -44, yend = -32.5), 
               color ="grey", size = 0.8, linetype = "dashed") +
  geom_segment(aes(x = 12900, xend = 12900, y = -44, yend = -32.5), 
               color ="grey", size = 0.8, linetype = "dashed") +
  geom_segment(aes(x = 11700, xend = 11700, y = -44, yend = -32.5), 
               color ="grey", size = 1) +
  geom_line(data = d18O_data, aes(y = d18O, x = Age), color = "#A22482") +
  geom_rect(aes(xmin=9800, xmax=15300, ymin=-44, ymax= -32.5), fill = NA, color = "black", size = 0.8) +
  labs(y = expression(delta^18*O), x = "") +
  scale_y_continuous(limits = c(-44, -32.5), position = "right", breaks = c(-42, -40, -38,  -36, -34),
                     expand = c(0, 0)) +
  scale_x_reverse(breaks = c(10500, 12000, 13000, 14500, 15000)) +
  coord_cartesian(xlim=c(15050, 10050)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = "black", size = 0.7, linetype = "solid"),
        axis.line.x = element_blank(),
        axis.text.y = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.text.x = element_blank(),
        axis.title.y.right = element_text(colour = "black", family= "Noto Sans", size = 9,
                                          margin = margin(l = 5)))

climateoverview_figure <- plot_grid(
  d18O_plot,
  NULL,
  main_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(0.6, -0.191, 1.5)
) 
