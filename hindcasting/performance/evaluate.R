
#----------------------------#
# Evaluate model performance #
#----------------------------#

library(ecospat)
library(dplyr)
library(precrec)
library(ggplot2)

sim_folder <- "D:/simulations/phenofit/paleo/test2"
sim_folder <- "D:/simulations/csdm/random_forest/paleo/fagus_sylvatica"
pollen_folder <- "D:/species/pollen/processed/fagus_sylvatica"

models <- data.frame(model = c("phenofit",
                               "phenofit_inverse", 
                               "random_forest",
                               "lasso_glm"),
               folder = c("D:/simulations/phenofit/paleo/test2",
                          "D:/simulations/phenofit/paleo/test2_inverse", 
                          "D:/simulations/csdm/random_forest/paleo/fagus_sylvatica",
                          "D:/simulations/csdm/lasso_glm/paleo/fagus_sylvatica"))

model_performance <- lapply(1:nrow(models),function(i){
  mod <- models[i,]
  perf <-lapply(seq(17000,1000,-2000), function(year){
    fitness <- readRDS(file.path(mod$folder, paste0(year, "BP.rds")))
    pollen <- readRDS(file.path(pollen_folder, paste0("pres_", year, "BP.rds")))
  
    fitness <- left_join(fitness, pollen, by = c("lat", "lon"))

    boyce_accumulate <- ecospat.boyce(fit = fitness$pred, obs = na.omit(fitness[fitness$pres == 1,]$pred), 
                                      nclass=0, window.w="default", res=100, 
                                      PEplot = F, rm.duplicate = F,  method = 'pearson' )
  
    eval_obj <- evalmod(scores = na.omit(fitness)$pred, labels = na.omit(fitness)$pres)
    auc_accumulate <- precrec::auc(eval_obj)
  
    return(data.frame(year = year, bi = boyce_accumulate$cor, auc = round(auc_accumulate$aucs[1],2), 
              npollen_pres = nrow(pollen[pollen$pres == 1,]), npollen = nrow(pollen), mod = mod$model))})
  return(do.call(rbind.data.frame, perf))
  })

model_performance <- do.call(rbind.data.frame, model_performance)

ggplot(data = model_performance) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_line(aes(x = year, y = auc, col = mod)) +
  geom_point(aes(x = year, y = auc, col = mod)) +
  scale_x_reverse() +
  scale_y_continuous(
    name = "AUC") +
  theme_minimal()

ggplot(data = model_performance) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_line(aes(x = year, y = bi, col = mod)) +
  geom_point(aes(x = year, y = bi, col = mod)) +
  scale_x_reverse() +
  scale_y_continuous(
    name = "Boyce index") +
  theme_minimal()


ggplot(data = model_performance_phenofit) +
  geom_line(aes(x = year, y = npollen)) +
  geom_point(aes(x = year, y = npollen)) +
  geom_line(aes(x = year, y = npollen_pres), col ="green") +
  geom_point(aes(x = year, y = npollen_pres), col ="green") +
  scale_x_reverse() +
  scale_y_continuous(
    name = "Num. pollen obs.") +
  theme_minimal()
  
  
# Load climate novelty
climdist_df <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/climate_novelty/save/ERA5Land_baseline.rds")
mod_perf_climdist <- left_join(model_performance, climdist_df)

ggplot(data = mod_perf_climdist[mod_perf_climdist$year <= 17000,]) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_line(aes(x = median, y = auc, col = mod)) +
  geom_point(aes(x = median, y = auc, col = mod)) +
  xlab("Climate novelty") +
  scale_y_continuous(
    name = "AUC") +
  theme_minimal()
  

