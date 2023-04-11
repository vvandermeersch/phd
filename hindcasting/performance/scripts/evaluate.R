
#----------------------------#
# Evaluate model performance #
#----------------------------#

library(ecospat)
library(dplyr)
library(precrec)
library(ggplot2)
library(modEvA)
library(terra)


pollen_folder <- "D:/species/pollen/processed/fagus_sylvatica"

models <- data.frame(name = c("phenofit",
                               "phenofit_fitted",
                               "random_forest",
                               "lasso_glm",
                               "brt",
                               "gam"),
                     simfolder = c("D:/simulations/phenofit/paleo/05deg",
                                "D:/simulations/phenofit/paleo/05deg_fitted",
                                "D:/simulations/csdm/random_forest/paleo/fagus_sylvatica",
                                "D:/simulations/csdm/lasso_glm/paleo/fagus_sylvatica",
                                "D:/simulations/csdm/brt/paleo/fagus_sylvatica",
                                "D:/simulations/csdm/gam/paleo/fagus_sylvatica"),
                     modfolder = c("C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/forward/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/backward/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/random_forest/fit/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/lasso_glm/fit/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/brt/fit/fagus_sylvatica",
                                   "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/gam/fit/fagus_sylvatica"),
                     mod = c("Fagus_sylvatica_VVanderMeersch.rds",
                             "cmaes_fit_subset4_rep1.rds",
                             "random_forest_finalcov_fullmodel.rds",
                             "lasso_glm_finalcov_fullmodel.rds",
                             "brt_finalcov_fullmodel.rds",
                             "gam_finalcov_fullmodel.rds"))


model_performance <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(seq(500,9000,500), function(year){
    fitness <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
  
    fitness <- left_join(fitness, pollen, by = c("lat", "lon"))
    
    # boyce index
    # boyce_ind <- ecospat.boyce(fit = fitness$pred, obs = na.omit(fitness[fitness$pres == 1,]$pred), 
    #                                   nclass=0, window.w="default", res=100, 
    #                                   PEplot = FALSE, rm.duplicate = TRUE,  method = 'spearman' )$cor
    
    boyce_ind <- as.numeric(Boyce(obs = fitness[fitness$pres == 1, c("lon", "lat")], pred = rast(fitness[,c(2,1,3)]), 
                                  main = "Boyce index", plot= FALSE,
                                  res = 100)$Boyce)
  
    # auc
    fitness <- na.omit(fitness)
    eval_obj <- evalmod(scores = fitness$pred, labels = fitness$pres)
    aucroc <- precrec::auc(eval_obj)
    
    # binarized predictions
    ths <- readRDS(file.path(mod$modfolder, mod$mod))$best_threshold
    fitness$bin_pred <- 0
    fitness[fitness$pred >= ths, "bin_pred"] <- 1
    
    # confusion matrix
    tp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 1,])
    fp <- nrow(fitness[fitness$bin_pred == 1 & fitness$pres == 0,])
    tn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 0,])
    fn <- nrow(fitness[fitness$bin_pred == 0 & fitness$pres == 1,])
    
    # metrics
    sens = tp/(tp+fn)
    spec = tn/(tn+fp)
    opr = fp/(tp+fp) # overprediction rate
    upr = fn/(tp+fn) # underprediction rate
    tss = sens + spec - 1
    sorensen = 2*tp/(fn + 2*tp + fp)
  
    return(data.frame(year = year, 
                      bi = boyce_ind, auc = round(aucroc$aucs[1],2), 
                      sens = sens, spec= spec, opr = opr, upr = upr, tss = tss,
                      sorensen = sorensen,
                      npollen_pres = nrow(pollen[pollen$pres == 1,]), 
                      npollen = nrow(pollen), mod = mod$name))})
  return(do.call(rbind.data.frame, perf))}
)

model_performance <- do.call(rbind.data.frame, model_performance)

auc_plot <- ggplot(data = model_performance) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_line(aes(x = year, y = auc, col = mod)) +
  geom_point(aes(x = year, y = auc, col = mod)) +
  scale_x_reverse(breaks = seq(0,9000,1000),
                  name = "Years (BP)") +
  scale_y_continuous(
    name = "AUC") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

tss_plot <- ggplot(data = model_performance) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_line(aes(x = year, y = tss, col = mod)) +
  geom_point(aes(x = year, y = tss, col = mod)) +
  scale_x_reverse(breaks = seq(0,9000,1000),
                  name = "Years (BP)") + 
  scale_y_continuous(
    name = "TSS") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

boyceindex_plot <- ggplot(data = model_performance) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_line(aes(x = year, y = bi, col = mod)) +
  geom_point(aes(x = year, y = bi, col = mod)) +
  scale_x_reverse(breaks = seq(0,9000,1000),
                  name = "Years (BP)") +
  scale_y_continuous(
    name = "Boyce index") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

sorensen_plot <- ggplot(data = model_performance) +
  geom_line(aes(x = year, y = sorensen, col = mod)) +
  geom_point(aes(x = year, y = sorensen, col = mod)) +
  scale_x_reverse(breaks = seq(0,9000,1000),
                  name = "Years (BP)") +
  scale_y_continuous(
    name = "Sørensen index") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



  
  
# Load climate novelty
# climdist_df <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_novelty/save/ERA5Land_baseline.rds")
# mod_perf_climdist <- left_join(model_performance, climdist_df)
# 
# ggplot(data = mod_perf_climdist[mod_perf_climdist$year <= 17000,]) +
#   geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
#   geom_line(aes(x = median, y = auc, col = mod)) +
#   geom_point(aes(x = median, y = auc, col = mod)) +
#   xlab("Climate novelty") +
#   scale_y_continuous(
#     name = "AUC") +
#   theme_minimal()
  

