y <- 11750

sim_dir <- "D:/simulations/phenofit/paleo/expert/025deg/fagus_sylvatica"
rast_old <- readRDS(file.path(sim_dir, paste0(y,"BP.rds")))[c(2,1,3)] %>% rast()
plot(rast_old)
rast_old <- ifel(rast_old < 0.162,0 ,1)
plot(rast_old)

sim_dir <- "D:/simulations/phenofit/paleo/expert/025deg/fagus_sylvatica_EBaron"
rast_new<- readRDS(file.path(sim_dir, paste0(y,"BP.rds")))[c(2,1,3)] %>% rast()
plot(rast_new)
rast_new <- ifel(rast_new < 0.5, 0, 1)
plot(rast_new)




sim_folder <- "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch_EBaron"
fitness <- read_mean_outputvalue(sim_folder, output_var = "Fitness")
newfit <- rast(fitness[c(2,1,3)])
plot(newfit)

sim_folder <- "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch"
fitness <- read_mean_outputvalue(sim_folder, output_var = "Fitness")
oldfit <- rast(fitness[c(2,1,3)])
plot(oldfit)

phenosim <- c(oldfit, newfit)
names(phenosim) <- c("old", "new")

plot <- ggplot() +
  facet_wrap(~lyr, labeller = labeller(lyr =c("old" = "Old parameter value", "new" = "Updated parameter value"))) + 
  geom_spatraster(data = phenosim) +
  scale_fill_gradient2(
    low = "#fee0d2",
    mid = "#fb6a4a",
    high = "#a50f15",
    na.value = "transparent", 
    limits = c(0, 1),
    breaks = seq(0, 1, 0.5), 
    labels = c("0", "0.5", "1"), midpoint = 0.25
  ) +
  theme_void() +
  guides(
    fill = guide_colorbar(order = 2,
                          frame.colour = "grey30", ticks.colour = NA,
                          frame.linewidth = 0.2,
                          theme = theme(legend.key.height  = unit(5, "pt"),
                                        legend.key.width  = unit(120, "pt"),
                                        legend.text = element_text(size = 6.5, margin = margin(t = 3.5))))) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5),
    legend.position = "none", legend.box="vertical",
    legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
    plot.margin = margin(t=5.5,b=0,l=0,r=0),
    strip.text = element_text(size = 9, margin = margin(t=0,b=5,l=0,r=0)),
    panel.border = element_rect(colour = "grey30", fill=NA, linewidth=0.4),
    panel.grid.major = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

ggsave(plot, filename = file.path("C:/Users/vandermeersch/Documents/CEFE/phd/manuscript/img/schemas", "phenofit_updatedparam.pdf"),
       width = 4, height = 2)
