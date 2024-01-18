# Investigate migration starting period

models <- c("phenofit", "phenofitfitted", "castanea", "castaneafitted",
            "brt", "randomforest", "gam", "lassoglm")
species <- c("abies_alba", "fagus_sylvatica", "quercus_ilex", "quercus_robur", "quercus_petraea")

# Thresholds
phenofit_simdir <- "D:/simulations/phenofit/paleo/expert/025deg"
phenofit_abies_alba <- 0.100
phenofit_fagus_sylvatica <- 0.162 
phenofit_quercus_ilex <- 0.172
phenofit_quercus_robur <- 0.70
phenofit_quercus_petraea <- 0.65

phenofitfitted_simdir <- "D:/simulations/phenofit/paleo/fitted/025deg"
phenofitfitted_abies_alba <- 0.663
phenofitfitted_fagus_sylvatica <- 0.785
phenofitfitted_quercus_ilex <- 0.604
phenofitfitted_quercus_robur <- 0.214
phenofitfitted_quercus_petraea <- 0.297

castanea_simdir_abies_alba <- "D:/simulations/castanea/paleo/expert/025deg/abies_alba/24yr_inventory_modcode/NPP"
castanea_simdir_fagus_sylvatica <- "D:/simulations/castanea/paleo/expert/025deg/fagus_sylvatica/30yr_inventory_modcode/NPP"
castanea_simdir_quercus_ilex <- "D:/simulations/castanea/paleo/expert/025deg/quercus_ilex/70yr_inventory_modcode/NPP"
castanea_simdir_quercus_robur <- "D:/simulations/castanea/paleo/expert/025deg/quercus_robur/32yr_inventory_modcode/NPP"
castanea_simdir_quercus_petraea <- "D:/simulations/castanea/paleo/expert/025deg/quercus_petraea/32yr_inventory_modcode/NPP"
castanea_abies_alba <- 132.2
castanea_fagus_sylvatica <- 347.7
castanea_quercus_ilex <- 353.6
castanea_quercus_robur <- 486.4
castanea_quercus_petraea <- 482.5

castaneafitted_simdir_abies_alba <- "D:/simulations/castanea/paleo/fitted/025deg/abies_alba/24yr_inventory_modcode/NPP"
castaneafitted_simdir_fagus_sylvatica <- "D:/simulations/castanea/paleo/fitted/025deg/fagus_sylvatica/30yr_inventory_modcode/NPP"
castaneafitted_simdir_quercus_ilex <- "D:/simulations/castanea/paleo/fitted/025deg/quercus_ilex/70yr_inventory_modcode/NPP"
castaneafitted_simdir_quercus_robur <- "D:/simulations/castanea/paleo/fitted/025deg/quercus_robur/32yr_inventory_modcode/NPP"
castaneafitted_simdir_quercus_petraea <- "D:/simulations/castanea/paleo/fitted/025deg/quercus_petraea/32yr_inventory_modcode/NPP"
castaneafitted_abies_alba <- 3567
castaneafitted_fagus_sylvatica <- 55.6
castaneafitted_quercus_ilex <- 1663
castaneafitted_quercus_robur <- 28.08
castaneafitted_quercus_petraea <- 4.69

brt_simdir <- "D:/simulations/csdm/brt/paleo/025deg"
brt_abies_alba <- 0.484
brt_fagus_sylvatica <- 0.463
brt_quercus_ilex <- 0.525
brt_quercus_robur <- 0.507
brt_quercus_petraea <- 0.465

randomforest_simdir <- "D:/simulations/csdm/random_forest/paleo/025deg"
randomforest_abies_alba <- 0.651
randomforest_fagus_sylvatica <- 0.540
randomforest_quercus_ilex <- 0.708
randomforest_quercus_robur <- 0.518
randomforest_quercus_petraea <- 0.612

gam_simdir <- "D:/simulations/csdm/gam/paleo/025deg"
gam_abies_alba <- 0.484
gam_fagus_sylvatica <- 0.400
gam_quercus_ilex <- 0.547
gam_quercus_robur <- 0.455
gam_quercus_petraea <- 0.455

lassoglm_simdir <- "D:/simulations/csdm/lasso_glm/paleo/025deg"
lassoglm_abies_alba <- 0.50
lassoglm_fagus_sylvatica <- 0.45
lassoglm_quercus_ilex <- 0.554
lassoglm_quercus_robur <- 0.492
lassoglm_quercus_petraea <- 0.467

# Plots (12k or 11.75k BP)
plots <- lapply(species, function(s){
  cowplot::plot_grid(cowplot::ggdraw() + cowplot::draw_label(s, fontface='bold'),
                     cowplot::plot_grid(plotlist = unlist(lapply(models, function(m){
    print(file.path(get(paste0(m, "_simdir")), s))
    lapply(c(12000, 11750), function(y){
      if(m %in% c("castanea", "castaneafitted")){
        out <- readRDS(file.path(get(paste0(m, "_simdir_", s)), paste0(y,"BP.rds")))
      }else{out <- readRDS(file.path(get(paste0(m, "_simdir")), s, paste0(y,"BP.rds")))}
      
      
      out$pred <- ifelse(out$pred < get(paste0(m, "_", s)), 0, 1)
      ggplot(data = out, aes(x = lon, y = lat, fill = as.factor(pred))) +
        geom_raster() +
        theme_void() +
        ggtitle(paste(m, y)) + 
        theme(legend.position = 'none')
      
    })
  }), recursive = F), ncol = 4), ncol=1, rel_heights=c(0.1, 1))
})

grid <- cowplot::plot_grid(plotlist = plots, ncol = 1)
ggsave(filename= file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/consensus", "grid_end_of_Holocene.pdf"), 
       plot= grid, height=44, width=10)




