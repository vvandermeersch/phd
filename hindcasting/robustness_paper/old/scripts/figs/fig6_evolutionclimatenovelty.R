
future_climatenovelty <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/metrics/climate_approach/data/future_climatenovelty.rds")
future_climatenovelty_summ <- future_climatenovelty %>%
  group_by(scenario, year) %>%
  summarize(mean_cn = mean(median), 
            median_cn = median(median), 
            min_cn = min(median), max_cn = max(median),
            q005_cn = quantile(median, 0.05), q95_cn = quantile(median, 0.95),
            q25_cn = quantile(median, 0.25), q75_cn = quantile(median, 0.75))


past_plot <- ggplot() +
  geom_line(data = burke_climatenovelty[burke_climatenovelty$year > 250 & burke_climatenovelty$year < 11750,],
            aes(x = year, y = median), col = "#6867ac") +
  # geom_hline(yintercept = trunc((2-critpoint_csdm)*100)/100, linetype = "dotted") +
  coord_cartesian(xlim = c(11500, 400), 
                  ylim =  c(0.65, 2.1),
                  clip = "off") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     name = "CLIMATE NOVELTY") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.x = element_text(margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank())


future_plot <- ggplot() +
  geom_ribbon(data = future_climatenovelty_summ, aes(x = year, ymin = q005_cn, ymax = q95_cn, fill = scenario), 
              alpha = 0.2) + 
  geom_ribbon(data = future_climatenovelty_summ, aes(x = year, ymin = q25_cn, ymax = q75_cn, fill = scenario), 
              alpha = 0.3) + 
  geom_line(data = future_climatenovelty_summ, aes(x = year, y = mean_cn, col = scenario)) +
  scale_color_manual(breaks= c('ssp245', "ssp585"),
                     values= c("#f7b763", "#ac6867")) +
  scale_fill_manual(breaks= c('ssp245', "ssp585"),
                     values= c("#f7b763", "#ac6867")) +
  # geom_hline(yintercept = trunc((2-critpoint_csdm)*100)/100, linetype = "dotted") + 
  # geom_bar(data = future_contribution,
  #          aes(x = year, fill = var, weight = contrib), position = "fill",
  #          width = 1) +
  coord_cartesian(xlim = c(2005-2, max(future_climatenovelty_summ$year)+3), 
                  ylim =  c(0.65, 2.1),
                  clip = "off") +
  scale_x_continuous(breaks = seq(2010,max(future_climatenovelty_summ$year), 20),
                     expand = c(0, 0),
                     name = "YEARS (AD)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     name = "Climate novelty") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        axis.line.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8, margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank())


distance_futurepast3 <- ggdraw() +
  draw_plot(past_plot, x = 0, y = 0, width = 0.65, height = 1) +
  draw_plot(future_plot, x = 0.65, y = 0, width = 0.35, height = 1) +
  annotate("segment", x = 0.646, xend = 0.654,  y = 0.128, yend = 0.152, color = "black") +
  annotate("segment", x = 0.652, xend = 0.66,  y = 0.128, yend = 0.152, color = "black") +
  annotate("segment", x = 0.085, xend = 0.98, y = 0.53, yend = 0.53, color = "grey", linetype = "dashed") +
  annotate("segment", x = 0.085, xend = 0.98, y = 0.79, yend = 0.79, color = "grey", linetype = "dashed")





distance_futurepast4 <-ggdraw() +
  draw_plot(past_plot, x = 0, y = 0, width = 0.65, height = 1) +
  draw_plot(future_plot, x = 0.65, y = 0, width = 0.35, height = 1) +
  annotate("text", x = 0.654, y = 0.135, label = "//", color = "black", family= "Helvetica Narrow", size = 3.5) +
  # annotate("segment", x = 0.1, xend = 0.98, y = 0.53, yend = 0.53, color = "grey", linetype = "dashed") +
  annotate("segment", x = 0.1, xend = 0.98, y = 0.73, yend = 0.73, color = "grey", linetype = "dashed")





library(ggh4x)

CRU_reference <- burke_climatenovelty[burke_climatenovelty$year == 0,]

climvar <- ggplot() +
  geom_point(
    data = burke_climatenovelty[burke_climatenovelty$year != 0 & burke_climatenovelty$year < 11750,], 
    aes(x = tmean-CRU_reference$tmean, y = pre-CRU_reference$pre),
    size = 1, col = "red", shape = 1) +
  geom_point(
    data = burke_climatenovelty[burke_climatenovelty$year != 0 & burke_climatenovelty$year < 18250,], 
    aes(x = tmean-CRU_reference$tmean, y = pre-CRU_reference$pre, col = median),
    size = 0.7) +
  geom_point(
    data = future_climatenovelty, 
    aes(x = tmean-CRU_reference$tmean, y = pre-CRU_reference$pre, col = median),
    size = 0.7, shape = 15) + 
  theme(axis.line = element_line()) +
  coord_axes_inside(labels_inside = TRUE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = c(-9,-6,-3,-1,1,3,6,9)) +
  scale_color_gradient2(high = "#ac67ab", low = "#67ac68", mid = "#abac67", midpoint = 1.6, limits = c(0.69, 2.5)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 7, hjust = 0.72),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.position="bottom",
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.title = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.title.align = 0.5, 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(0.7, 'cm'),
        legend.margin=margin(t = -0.2, b = 0, r = 0, l = 0, unit='cm'),
        plot.margin=margin(t = 0, b = 0.2, r = 0, l = 0, unit='cm')) +
  guides(col = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                              ticks = FALSE)) +
  ylab(expression(Delta*Temperature)) +
  xlab(expression(Delta*Precipitation)) +
  labs(col = "Climate novelty")



distance_futurepast5 <- plot_grid(distance_futurepast4, climvar,
          ncol = 2,
          rel_widths = c(0.7, 0.3))


climvar2 <- ggplot() +
  geom_point(
    data = burke_climatenovelty[burke_climatenovelty$year != 0 & burke_climatenovelty$year < 11750,], 
    aes(x = tmean-CRU_reference$tmean, y = pre-CRU_reference$pre),
    size = 1.3, col = "red", shape = 1) +
  geom_point(
    data = burke_climatenovelty[burke_climatenovelty$year != 0 & burke_climatenovelty$year < 18250,], 
    aes(x = tmean-CRU_reference$tmean, y = pre-CRU_reference$pre, col = median),
    size = 1) +
  geom_text(
    data = burke_climatenovelty[burke_climatenovelty$year != 0 & burke_climatenovelty$year < 18250,], 
    aes(x = tmean-CRU_reference$tmean, y = pre-CRU_reference$pre, col = median),
    label= as.character(burke_climatenovelty[burke_climatenovelty$year != 0 & burke_climatenovelty$year < 18250,]$year),
    nudge_x=1, nudge_y=1,
    check_overlap=T, size = 2
  ) +
  
  geom_point(
    data = future_climatenovelty, 
    aes(x = tmean-CRU_reference$tmean, y = pre-CRU_reference$pre, col = median),
    size = 1, shape = 15) + 
  theme(axis.line = element_line()) +
  coord_axes_inside(labels_inside = TRUE) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = c(-9,-6,-3,-1,1,3,6,9)) +
  scale_y_continuous(breaks = c(-100, -50, 50)) +
  scale_color_gradient2(high = "#ac67ab", low = "#67ac68", mid = "#abac67", midpoint = 1.6, limits = c(0.69, 2.5)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 7, hjust = 0.72),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.position="bottom",
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.title = element_text(colour = "black", family= "Helvetica Narrow", size = 7),
        legend.title.align = 0.5, 
        legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(0.7, 'cm'),
        legend.margin=margin(t = -0.2, b = 0, r = 0, l = 0, unit='cm'),
        plot.margin=margin(t = 0, b = 0.2, r = 0, l = 0, unit='cm')) +
  guides(col = guide_colorbar(title.position = "top", direction = "horizontal", frame.colour = "black", frame.linewidth = 0.3,
                              ticks = FALSE)) +
  ylab(expression(Delta*Temperature)) +
  xlab(expression(Delta*Precipitation)) +
  labs(col = "Climate novelty")
