

dir <- file.path(wd, "data/climate/hypervolume")

present <- readRDS(file.path(dir, "present_domain.rds"))
past12000BP <- readRDS(file.path(dir, "past", "projection_domain_12000BP.rds"))
past11500BP <- readRDS(file.path(dir, "past", "projection_domain_11500BP.rds"))
models <- c(
  "ACCESS-CM2", "ACCESS-ESM1-5",
  "BCC-CSM2-MR",
  "CanESM5",
  # "CESM2", "CESM2-WACCM",
  "CMCC-CM2-SR5", "CMCC-ESM2",
  "CNRM-CM6-1",
  "CNRM-ESM2-1",
  # "EC-Earth3", 
  "EC-Earth3-Veg-LR",
  "FGOALS-g3",
  "GFDL-CM4", "GFDL-CM4_gr2", "GFDL-ESM4",
  "GISS-E2-1-G",
  "HadGEM3-GC31-LL", # "HadGEM3-GC31-MM",
  "IITM-ESM",
  "INM-CM4-8", "INM-CM5-0",
  "IPSL-CM6A-LR",
  "KACE-1-0-G", 
  "KIOST-ESM",
  "MIROC6", "MIROC-ES2L",
  "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", 
  "MRI-ESM2-0",
  "NESM3", 
  "NorESM2-LM", "NorESM2-MM", 
  # "TaiESM1", 
  "UKESM1-0-LL")
ssp2 <- lapply(models, function(m){
  hypervolume_to_data_frame(readRDS(file.path(dir, "ssp245", m, "projection_domain_2090.rds")))
})
ssp5 <- lapply(models, function(m){
  hypervolume_to_data_frame(readRDS(file.path(dir, "ssp585", m, "projection_domain_2090.rds")))
})

ssp2 <- as.data.frame(do.call(rbind, ssp2)) %>%
  dplyr::mutate(period = "4ssp2") %>%
  dplyr::sample_n(20000)
ssp5 <- as.data.frame(do.call(rbind, ssp5)) %>%
  dplyr::mutate(period = "5ssp5") %>%
  dplyr::sample_n(20000)
present <- hypervolume_to_data_frame(present) %>%
  dplyr::mutate(period = "3present")
past12000BP <- hypervolume_to_data_frame(past12000BP) %>%
  dplyr::mutate(period = "1past12000BP")
past11500BP <- hypervolume_to_data_frame(past11500BP) %>%
  dplyr::mutate(period = "2past11500BP")

hypervolumes <- rbind(past12000BP, past11500BP, present, ssp2, ssp5)

centroid <- hypervolumes %>%
  group_by(period) %>%
  dplyr::summarise(PC1 = mean(PC1), PC2 = mean(PC2), PC3 = mean(PC3))


pc12 <- ggplot(data = hypervolumes, aes(x = PC1, y = PC2, color = period, fill = period)) +
  geom_point(alpha = 0.2, shape = 16) +
  geom_density_2d(breaks = 1e-3) +
  geom_point(data = centroid, size = 3, shape = 21, color = "black") +
  geom_point(data = centroid, size = 3, shape = 10, color = "black") +
  scale_color_manual(breaks= c('1past12000BP','2past11500BP', "3present", "4ssp2", "5ssp5"),
                     values= c("#504f91", "#8988be", "#67ac68", "#f69320", "#bf1d1e")) +
  scale_fill_manual(breaks= c('1past12000BP','2past11500BP', "3present", "4ssp2", "5ssp5"),
                    values= c("#504f91", "#8988be", "#67ac68", "#f69320", "#bf1d1e")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank()) +
  labs(x = "PC1 (52.5%)", y = "PC2 (36.2%)")

pc13 <- ggplot(data = hypervolumes, aes(x = PC1, y = PC3, color = period, fill = period)) +
  geom_point(alpha = 0.2, shape = 16) +
  geom_density_2d(breaks = 1e-3) +
  geom_point(data = centroid, size = 3, shape = 21, color = "black") +
  geom_point(data = centroid, size = 3, shape = 10, color = "black") +
  scale_color_manual(breaks= c('1past12000BP','2past11500BP', "3present", "4ssp2", "5ssp5"),
                     values= c("#504f91", "#8988be", "#67ac68", "#f69320", "#bf1d1e")) +
  scale_fill_manual(breaks= c('1past12000BP','2past11500BP', "3present", "4ssp2", "5ssp5"),
                    values= c("#504f91", "#8988be", "#67ac68", "#f69320", "#bf1d1e")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank()) +
  labs(x = "PC1 (52.5%)", y = "PC3 (7.3%)")


figA15_main <- plot_grid(pc12, pc13, ncol = 2)
