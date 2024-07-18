library(hypervolume)
library(terra)
# library("FactoMineR")
# library("factoextra")
library(data.table)
library(future.apply)
library(dplyr)
library(ggplot2)
set.seed(1997)

dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/manuscript/img/schemas/fagus"

# era5land_r <- average_to_three_month_means(1970:2000, name_sim = "ERA5LAND",
#                                      data_dir = "D:/climate/ERA5-Land/phenofit_format/transformed")
# pca_present <- prcomp(as.data.frame(era5land_r), center = TRUE, scale = TRUE)
# saveRDS(era5land_r, file.path(dir, "era5land_r.rds"))
# saveRDS(pca_present, file.path(dir, "pca_present.rds"))
pca_present <- readRDS(file.path(dir, "pca_present.rds"))
era5land_r <- readRDS(file.path(dir, "era5land_r.rds"))


# PHENOFIT
sim_dir <- "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch"
fit <- rast(read_mean_outputvalue(sim_dir, years = c(1970:2000))[c(2,1,3)])
ths <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/fagus_sylvatica/Fagus_sylvatica_VVanderMeersch.rds")$best_threshold
fit <- ifel(fit < ths, NA, 1)
# pcaaxis_phenofit <- predict(pca_present, as.data.frame(mask(era5land_r,fit)))[,1:3] # three first axis of PCA
# saveRDS(pcaaxis_phenofit, file.path(dir, "pcaaxis_phenofit.rds"))
pcaaxis_phenofit <- readRDS(file.path(dir, "pcaaxis_phenofit.rds"))
# fit_domain = hypervolume(pcaaxis_phenofit, method='gaussian', verbose = TRUE)
hypervolume_phenofit <- readRDS(file.path(dir,"hypervolume_phenofit.rds")) %>%
  hypervolume_to_data_frame() %>%
  mutate(model = "PHENOFIT")

# PHENOFIT v2
sim_dir <- "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch_EBaron"
fit <- rast(read_mean_outputvalue(sim_dir, years = c(1970:2000))[c(2,1,3)])
ths <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert/fagus_sylvatica/Fagus_sylvatica_VVanderMeersch_EBaron.rds")$best_threshold
fit <- ifel(fit < ths, NA, 1)
# pcaaxis <- predict(pca_present, as.data.frame(mask(era5land_r,fit)))[,1:3] # three first axis of PCA
# saveRDS(pcaaxis, file.path(dir, "pcaaxis_phenofit2.rds"))
# fit_domain = hypervolume(pcaaxis, method='gaussian', verbose = TRUE)
hypervolume_phenofit2 <- readRDS(file.path(dir,"hypervolume_phenofit2.rds")) %>%
  hypervolume_to_data_frame() %>%
  mutate(model = "PHENOFIT2")

# PHENOFIT fitted
sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES/subset4_rep1"
fit <- rast(read_mean_outputvalue(sim_dir, years = c(1970:2000))[c(2,1,3)])
ths <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/fagus_sylvatica/cmaes_fit_subset4_rep1.rds")$best_threshold
fit <- ifel(fit < ths, NA, 1)
pcaaxis <- predict(pca_present, as.data.frame(mask(era5land_r,fit)))[,1:3] # three first axis of PCA
saveRDS(pcaaxis, file.path(dir, "pcaaxis_phenofit_fitted.rds"))
# fit_domain = hypervolume(pcaaxis, method='gaussian', verbose = TRUE)
hypervolume_phenofit_fitted <- readRDS(file.path(dir,"hypervolume_phenofit_fitted.rds")) %>%
  hypervolume_to_data_frame() %>%
  mutate(model = "PHENOFITfitted")

# Lasso GLM
sim_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/fagus_sylvatica/lasso_glm_finalcov_fullmodel.rds"
fit <- rast(readRDS(sim_dir)$europe_pred[c(2,1,3)])
ths <- readRDS(sim_dir)$best_threshold
fit <- ifel(fit < ths, NA, 1)
# pcaaxis_lassoglm <- predict(pca_present, as.data.frame(mask(era5land_r,fit)))[,1:3] # three first axis of PCA
# saveRDS(pcaaxis_lassoglm, file.path(dir, "pcaaxis_lassoglm.rds"))
pcaaxis_lassoglm <- readRDS(file.path(dir, "pcaaxis_lassoglm.rds"))
# fit_domain = hypervolume(pcaaxis_lassoglm, method='gaussian', verbose = TRUE)
hypervolume_lassoglm <- readRDS(file.path(dir,"hypervolume_lassoglm.rds")) %>%
  hypervolume_to_data_frame() %>%
  mutate(model = "Lasso GLM")

climate_domain <- readRDS(file.path(dir,"hypervolume_climate.rds")) %>%
  hypervolume_to_data_frame()

hypervolumes <- rbind(hypervolume_lassoglm, hypervolume_phenofit, hypervolume_phenofit2, hypervolume_phenofit_fitted)

fig <- ggplot() +
  # geom_point(data = pcaaxis_lassoglm, aes(x = PC1, y = PC2), alpha = 0.2, shape = 16) +
  geom_density_2d(data = hypervolumes, aes(x = PC1, y = PC2, color = model), breaks = 5e-3) +
  geom_density_2d(data = climate_domain, aes(x = PC1, y = PC2), breaks = 1e-4) +
  scale_x_continuous(limits = c(-7,7.1), expand = c(0,0), breaks = c(-5,0,5)) +
  scale_y_continuous(limits = c(-5,10), expand = c(0,0)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 8),
        axis.title = element_text(colour = "black",size = 9),
        legend.position="none", legend.title=element_blank())

ggsave(fig, filename = file.path(dir,"niches_wclimatelarge.pdf"))
