# note: this model should be run after all the component 
# models, MaxEnt, Lasso, GAM, BRT, RF down-sampled
library(tidyverse)
library(ROCR)
library(scales)

# path to the folder contain all models output (in separate folders). 
wdir <- "models_output"
outdir <- "models_output/Ensemble"
if(!file.exists(outdir)){
  dir.create(file.path(outdir))
  print("The directory is created")
}
# get the list of species in one of the models; here I use GAM
sp_preds <- list.files(file.path(wdir, "gam"), pattern = ".csv$", full.names = FALSE)
# get the list of regions and species names
splist <- purrr::map_chr(strsplit(sp_preds, "_"), purrr::pluck, 1)

for(i in splist){
  gm <- read.csv(sprintf("%s/gam/%s_gam.csv", wdir, i)) %>%
    mutate(pred = rescale(pred, to = c(0,1)))
  lasso <- read.csv(sprintf("%s/lasso/%s_lasso.csv", wdir, i)) %>% 
    mutate(pred = rescale(pred, to = c(0,1)))
  brt <- read.csv(sprintf("%s/brt/%s_brt.csv", wdir, i)) %>% 
    mutate(pred = rescale(pred, to = c(0,1)))
  rf <- read.csv(sprintf("%s/rf_downsampled/%s_rf_downsampled.csv", wdir,i)) %>%
    mutate(pred = rescale(prediction, to = c(0,1)))
  maxt <- read.csv(sprintf("%s/maxent/%s_maxent.csv", wdir, i)) %>%
    mutate(pred = rescale(prediction, to = c(0,1)))
  ensm <- rf[, 1:6]
  ensm$prediction <- rowMeans(cbind(gm[,"pred", drop = FALSE],
                                    lasso[, "pred", drop = FALSE],
                                    brt[, "pred", drop = FALSE],
                                    rf[, "pred", drop = FALSE],
                                    maxt[, "pred", drop = FALSE]))
  
  ensm$time <- sum(gm$time[1], lasso$time[1], brt$time[1], rf$time[1], maxt$time[1])
  write.csv(ensm, sprintf("%s/%s_ensemble.csv", outdir, i), row.names = FALSE)
}

