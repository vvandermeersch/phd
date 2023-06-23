
# illustration purpose
future <- data.frame(year = seq(2030,2090, 10), distance = c(1.47, 1.57, 1.69, 1.82, 1.88, 1.99, 2.07))

future_contribution <- data.frame(year = c(2030, 2030, 2040, 2040, 2050, 2050, 2060, 2060, 2070, 2070, 2080, 2080, 2090, 2090), 
                                  var = c("pre", "temp", "pre", "temp", "pre", "temp", "pre", "temp", "pre", "temp", "pre", "temp", "pre", "temp"), 
                                  contrib = c(0.2, 0.8, 0.36, 0.64, 0.18, 1-0.18, 0.23, 1-0.23, 0.19, 1-0.19, 0.27, 1-0.27, 0.23, 1-0.23))




past_plot <- ggplot() +
  geom_line(data = burke_climatenovelty[burke_climatenovelty$year > 250 & burke_climatenovelty$year < 11750,],
                                        aes(x = year, y = median), col = "#6867ac") +
  coord_cartesian(xlim = c(11500, 400), 
                  ylim =  c(0.65, 2.1),
                  clip = "off") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climate novelty") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


future_plot <- ggplot() +
  geom_line(data = burke_futureclimatenovelty, aes(x = year, y = median), col = "#ac6867") +
  # geom_bar(data = future_contribution,
  #          aes(x = year, fill = var, weight = contrib), position = "fill",
  #          width = 1) +
  coord_cartesian(xlim = c(2020-2, max(future$year)+3), 
                  ylim =  c(0.65, 2.1),
                  clip = "off") +
  scale_x_continuous(breaks = seq(2020,max(future$year), 20),
                  expand = c(0, 0),
                  name = "YEARS (AD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climate novelty") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title.x = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())


distance_futurepast <- plot_grid(
  past_plot, NULL, future_plot, 
  ncol = 3,
  align = "hv",
  rel_widths = c(1, -0.09, 0.6)
)


distance_futurepast <- distance_futurepast + 
  annotate("segment", x = 0.51, xend = 0.56,  y = 0.1, yend = 0.13, color = "black") +
  annotate("segment", x = 0.556, xend = 0.566,  y = 0.1, yend = 0.13, color = "black")


plot_null <- ggplot() +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )

distance_futurepast2 <- plot_grid(
  past_plot, future_plot,
  ncol = 2,
  align = "hv",
  rel_widths = c(1, 0.6)
)


distance_futurepast2 <- distance_futurepast2 + 
  annotate("segment", x = 0.63, xend = 0.64,  y = 0.1, yend = 0.13, color = "black") +
  annotate("segment", x = 0.636, xend = 0.646,  y = 0.1, yend = 0.13, color = "black")


distance_futurepast3 <- ggdraw() +
  draw_plot(past_plot, x = 0, y = 0, width = 0.7, height = 1) +
  draw_plot(future_plot, x = 0.7, y = 0, width = 0.3, height = 1) +
  annotate("segment", x = 0.696, xend = 0.704,  y = 0.128, yend = 0.152, color = "black") +
  annotate("segment", x = 0.702, xend = 0.71,  y = 0.128, yend = 0.152, color = "black") +
  annotate("segment", x = 0.085, xend = 0.98, y = 0.53, yend = 0.53, color = "grey", linetype = "dashed") +
  annotate("segment", x = 0.085, xend = 0.98, y = 0.79, yend = 0.79, color = "grey", linetype = "dashed")





past_plot <- ggplot() +
  geom_rect(data = burke_climatenovelty[burke_climatenovelty$year > 250 & burke_climatenovelty$year < 11750,], 
            aes(xmin = year-125, xmax = year+125,
                ymin = (0.65+1.45*(1-contribution_pre/100)), 
                ymax = (0.65+1.45*(1-contribution_pre/100))+1.45*contribution_pre/100),
            fill = "#AEC5EB", alpha = 0.4, color = NA) +
  geom_rect(data = burke_climatenovelty[burke_climatenovelty$year > 250 & burke_climatenovelty$year < 11750,], 
            aes(xmin = year-125, xmax = year+125,
                ymin = 0.65, ymax = 0.65+1.45*contribution_tmp/100),
            fill = "#E9AFA3", alpha = 0.4, color = NA) +
  geom_line(data = burke_climatenovelty[burke_climatenovelty$year > 250 & burke_climatenovelty$year < 11750,],
            aes(x = year, y = median), col = "#6867ac") +
  coord_cartesian(xlim = c(11500, 400), 
                  ylim =  c(0.65, 2.1),
                  clip = "on") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climate novelty") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())







future_plot <- ggplot() +
  geom_rect(data = burke_futureclimatenovelty, aes(xmin = year-5, xmax = year+5,
                                                   ymin = (0.65+1.45*(1-contribution_pre/100)), 
                                                   ymax = (0.65+1.45*(1-contribution_pre/100))+1.45*contribution_pre/100),
            fill = "#AEC5EB", alpha = 0.4, color = NA) +
  geom_rect(data = burke_futureclimatenovelty, aes(xmin = year-5, xmax = year+5,
                                                   ymin = 0.65, ymax = 0.65+1.45*contribution_tmp/100),
            fill = "#E9AFA3", alpha = 0.4, color = NA) +
  geom_line(data = burke_futureclimatenovelty, aes(x = year, y = median), col = "#ac6867") +
  coord_cartesian(xlim = c(2020-1, max(future$year)+1), 
                  ylim =  c(0.65, 2.1),
                  clip = "on") +
  scale_x_continuous(breaks = seq(2030,max(future$year), 20),
                     expand = c(0, 0),
                     name = "YEARS (AD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     name = "Climate novelty",
                     sec.axis = sec_axis( trans= ~ (.-0.65)/1.45, name="Variable contribution", breaks = c(0.25, 0.5, 0.75, 1))) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line.y.left = element_blank(), axis.text.y.left = element_blank(), 
        axis.ticks.y.left = element_blank(), axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title.y.right = element_text(colour = "black", family= "Noto Sans", size = 9, margin = margin(l = 4)),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title.x = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="none", legend.title=element_blank())

distance_futurepast4 <- ggdraw() +
  draw_plot(past_plot, x = 0, y = 0, width = 0.7, height = 1) +
  draw_plot(future_plot, x = 0.7, y = 0, width = 0.3, height = 1) +
  annotate("segment", x = 0.696, xend = 0.704,  y = 0.128, yend = 0.152, color = "black") +
  annotate("segment", x = 0.702, xend = 0.71,  y = 0.128, yend = 0.152, color = "black") +
  annotate("segment", x = 0.085, xend = 0.91, y = 0.61, yend = 0.61, color = "grey", linetype = "dashed") +
  annotate("segment", x = 0.085, xend = 0.91, y = 0.79, yend = 0.79, color = "grey", linetype = "dashed")
