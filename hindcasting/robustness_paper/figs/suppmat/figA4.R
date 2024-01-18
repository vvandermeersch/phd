
dir <- file.path(wd, "data/simulations/migration/phenofit/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_from12000/example")

initial_distribution <- readRDS(file.path(dir, "initial_distribution.rds"))
initial_distribution <- as.factor(initial_distribution)
levels(initial_distribution) <- data.frame(value=0:1, desc=c("0", "1"))
  
migration_steps <- terra::rast(file.path(dir, "steps_250.asc"))
migration_steps <- ifel(migration_steps > 1 & migration_steps <= 250, migration_steps, NA)

extent_zoom <- ext(c(4.38e06,4.445e06, 2.38e06, 2.46e06))
migration_steps_zoom <- crop(migration_steps, extent_zoom)

zoom_plot <- ggplot() +
  geom_spatraster(data = initial_distribution) +
  scale_fill_manual(values = c("#ededed", "#4f928e"), na.value = NA , guide = 'none') +
  new_scale_fill() +
  geom_spatraster(data = migration_steps_zoom) +
  scale_fill_whitebox_c(palette = "bl_yl_rd", direction = 1, guide = 'none') +
  theme_bw() +
  coord_sf(xlim = c(extent_zoom[1],extent_zoom[2]), ylim = c(extent_zoom[3], extent_zoom[4])) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))
  
figA4_main <- ggplot() +
  geom_spatraster(data = initial_distribution) +
  scale_fill_manual(values = c("#ededed", "#4f928e"), na.value = NA , guide = 'none') +
  new_scale_fill() +
  geom_spatraster(data = migration_steps) +
  scale_fill_whitebox_c(palette = "bl_yl_rd", direction = 1, limits = c(0,250),
                        ) +
  labs(fill = "Number of years since 12000BP") +
  coord_sf(xlim = c(2.7e06,5.3e6), ylim = c(1.5e06, 4.3e06)) +
  annotate("rect", xmin = extent_zoom[1], xmax = extent_zoom[2], ymin = extent_zoom[3], ymax = extent_zoom[4],
           color = "black", fill = NA) +
  annotate(geom = "segment", x = extent_zoom[1], xend = 2.92e06, y = extent_zoom[4], yend = 3.7e06, color = "black") + 
  annotation_custom(
    ggplotGrob(zoom_plot), 
    xmin = 2.75e06, xmax = 4.2e06, ymin = 2.6e06, ymax = 4.25e06
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom", 
        legend.title = element_text(colour = "black", family= "Helvetica", size = 8),
        legend.title.align = 0.5,
        legend.key.height = unit(0.2, 'cm'), 
        legend.key.width = unit(0.8, 'cm')) +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.3,
                               ticks = FALSE))
  
