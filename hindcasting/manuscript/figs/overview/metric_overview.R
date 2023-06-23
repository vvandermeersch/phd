

# distance_from_nichecentroid <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/performance/hypervolume/distance_from_calibration_nichecentroid.rds")
# centroid_distance <- distance_from_nichecentroid %>%
#   group_by(year) %>%
#   dplyr::summarize(median = median(distance, na.rm=TRUE))

# 
# data_metrics <- data.frame(
#   year = c(centroid_distance$year, climatic_distance$year),
#   median = c(centroid_distance$median, climatic_distance$median),
#   metric = c(rep("Distance from niche centroid", nrow(centroid_distance)),
#                                       rep("Climatic distance", nrow(climatic_distance))))

clim_dist <- clim_dist[clim_dist$year %% 500 == 0,]

data_metrics <- data.frame(
  year = c(clim_hypv_similarity$year, 
           clim_dist$year,
           clim_dist$year,
           clim_dist_pollen$year,
           dist_from_nichecentroid$year,
           burke_climatenovelty$year),
  mean = c(clim_hypv_similarity$hypv_sorensen, 
           clim_dist$mean,
           clim_dist$mean_icefilter,
           clim_dist_pollen$mean,
           dist_from_nichecentroid$mean,
           burke_climatenovelty$mean),
  metric = c(rep("Clim. hypervolume similarity\n(Sørensen)", nrow(clim_hypv_similarity)),
             rep("Climatic distance\n(Mahalanobis)", nrow(clim_dist)),
             rep("Climatic distance\n(Mahalanobis)", nrow(clim_dist)),
             rep("Climatic distance - only pollen\nrecords (Mahalanobis)", nrow(clim_dist)),
             rep("Climatic distance from niche\ncentroid (Euclidean)", nrow(dist_from_nichecentroid)),
             rep("Climate novelty\n(as in Burke et al.)", nrow(burke_climatenovelty))
             ),
  linetype = c(rep("With icesheet", nrow(clim_hypv_similarity)),
             rep("With icesheet", nrow(clim_dist)),
             rep("Without icesheet", nrow(clim_dist)),
             rep("With icesheet", nrow(clim_dist)),
             rep("With icesheet", nrow(dist_from_nichecentroid)),
             rep("Without icesheet", nrow(burke_climatenovelty)))
  )


metrics_plot <- ggplot() +
  geom_line(data = data_metrics, aes(x = year, y = mean, col = metric, linetype = linetype)) +
  scale_x_reverse(breaks = seq(0,18000,3000),
                  expand = c(0, 500),
                  name = "YEARS (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     name = "") +
  scale_color_manual(breaks= c('Clim. hypervolume similarity\n(Sørensen)', 
                               'Climatic distance\n(Mahalanobis)', 
                               'Climatic distance from niche\ncentroid (Euclidean)',
                               'Climatic distance - only pollen\nrecords (Mahalanobis)',
                               "Climate novelty\n(as in Burke et al.)"),
                     values= c("#67ac68","#6867ac","#ac6867", "#9998c7", "#c798c6")) +
  scale_linetype_manual(breaks= c("With icesheet", "Without icesheet"),
                     values= c("solid", "dashed")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Noto Sans", size = 9),
        axis.title = element_text(colour = "black", family= "Noto Sans", size = 9),
        legend.position="bottom", legend.title=element_blank())

