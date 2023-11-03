
#------------------------------------------#
# Plot Quercus distinct pollen occurrences #
#------------------------------------------#

library(ggplot2)

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/occurrence_data/herzschuhetal2021"

dec_folder <- "D:/species/pollen/processed/quercus_deciduoustype/025deg/0025thr_500yrunc"
evg_folder <- "D:/species/pollen/processed/quercus_evergreentype/025deg/0025thr_500yrunc"
ind_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc"
grid_folder <- "D:/simulations/phenofit/paleo/expert/025deg/fagus_sylvatica"

  

# Raw plots
plot_list <- lapply(seq(12000,1000,-1000), function(yr){
  ggplot() +
    # europe background
    geom_raster(data = readRDS(file.path(grid_folder, paste0(yr, "BP.rds"))), aes(x = lon, y =lat), fill = "lightgrey") +
    # deciduous quercus
    geom_point(data = readRDS(file.path(dec_folder, paste0("pres_", yr, "BP.rds"))) %>% dplyr::filter(pres > 0),
               aes(x = lon, y = lat), shape = 24, col = "black", fill = "#90be6d", size = 2.5, alpha = 0.8) +
    # evergreen quercus
    geom_point(data = readRDS(file.path(evg_folder, paste0("pres_", yr, "BP.rds"))) %>% dplyr::filter(pres > 0),
               aes(x = lon, y = lat), shape = 21, col = "black", fill = "#ee9b00", size = 2, alpha = 0.8) +
    # undetermined quercus
    geom_text(data = readRDS(file.path(ind_folder, paste0("pres_", yr, "BP.rds"))) %>% dplyr::filter(pres > 0),
              aes(x = lon, y = lat), label = "?", col = "#ae2012", fontface = "bold", size = 2.5) +
    geom_text(aes(x = -5, y = 65), label = paste0(yr,"BP")) +
    theme_void()
})
ggsave(filename= file.path(wd, "output", "quercus_distinction.pdf"), 
       plot = cowplot::plot_grid(plotlist = plot_list, nrow = 3), height=10, width=14)




# With current Q. ilex habitats according to EVM and AFE - a way to distinguish some undertermined pollen grains ?
EVM <- vect(file.path("D:/species","EuroVegMap/evm_species.shp"))
AFE <- vect(file.path("D:/species","AFE/species/all.gpkg")) %>% tidyterra::filter(status == 6)
AFE_qilex <- aggregate(AFE[AFE$taxon_name == "Quercus ilex"])
EVM_qilex <- aggregate(EVM[!is.na(EVM$Quercus_il)]) %>% project(crs(AFE_qilex))
currenthabitat_qilex <- aggregate(buffer(aggregate(union(AFE_qilex, EVM_qilex)), width = 25000)) # 25km buffer

plot_list <- lapply(seq(12000,1000,-1000), function(yr){
  ggplot() +
    # europe background
    geom_raster(data = readRDS(file.path(grid_folder, paste0(yr, "BP.rds"))), aes(x = lon, y =lat), fill = "lightgrey") +
    # afe and evm distribution
    geom_sf(data = currenthabitat_qilex) +
    # deciduous quercus
    geom_point(data = readRDS(file.path(dec_folder, paste0("pres_", yr, "BP.rds"))) %>% dplyr::filter(pres > 0),
               aes(x = lon, y = lat), shape = 24, col = "black", fill = "#90be6d", size = 2.5, alpha = 0.8) +
    # evergreen quercus
    geom_point(data = readRDS(file.path(evg_folder, paste0("pres_", yr, "BP.rds"))) %>% dplyr::filter(pres > 0),
               aes(x = lon, y = lat), shape = 21, col = "black", fill = "#ee9b00", size = 2, alpha = 0.8) +
    # undetermined quercus - but unlikely evergreen
    tidyterra::geom_spatvector_text(data = readRDS(file.path(ind_folder, paste0("pres_", yr, "BP.rds"))) %>% dplyr::filter(pres > 0) %>%
                vect() %>% erase(currenthabitat_qilex),
                label = "?", col = "#4d908e", fontface = "bold", size = 2.5) +
    # undetermined quercus - but possibly evergreen
    tidyterra::geom_spatvector_text(data = readRDS(file.path(ind_folder, paste0("pres_", yr, "BP.rds"))) %>% dplyr::filter(pres > 0) %>%
                vect() %>% intersect(currenthabitat_qilex),
                label = "?", col = "#f9844a", fontface = "bold", size = 2.5) +
    geom_text(aes(x = -5, y = 65), label = paste0(yr,"BP")) +
    theme_void()
})
ggsave(filename= file.path(wd, "output", "quercus_distinction_AFE&EVM.pdf"), 
       plot = cowplot::plot_grid(plotlist = plot_list, nrow = 3), height=10, width=16)


