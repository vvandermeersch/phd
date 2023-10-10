library(ggplot2)
library(terra)
library(tidyterra)
library(cowplot)

models <- data.frame(name = c("7PHENOFIT(fitted)",
                              "5PHENOFIT",
                              "2RandomForest",
                              "3GAM",
                              "1BRT",
                              "4LassoGLM",
                              "6CASTANEA",
                              "8CASTANEA(fitted)"),
                     type = c("4Fittedprocessbased",
                              "3Expertprocessbased",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "1Correlative",
                              "3Expertprocessbased",
                              "4Fittedprocessbased"),
                     type2 = c("4Fittedprocessbased",
                               "3Expertprocessbased",
                               "1Treebased",
                               "2Regressionbased",
                               "1Treebased",
                               "2Regressionbased",
                               "3Expertprocessbased",
                               "4Fittedprocessbased"),
                     simfolder = c("D:/simulations/phenofit/paleo/migration/fitted/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/phenofit/paleo/migration/expert/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/random_forest/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/gam/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/brt/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/csdm/lasso_glm/paleo/migration/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/expert/abies_alba_expandLDD_from12k_scprb_2km20km",
                                   "D:/simulations/castanea/paleo/migration/fitted/abies_alba_expandLDD_from12k_scprb_2km20km_190ppm"))



consensus_fitness <- c()
consensus_migration <- c()

years <- seq(12000,500,-500)

ice_sheet <- rast(lapply(years,function(year){
  print(year)
  yr_ICE6G <- year%/%500*0.5
  extent <- ext(c(-10,35,36,71))
  ice <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), ext(-10,30,34,66))
  return(ice)
}))
names(ice_sheet) <- as.character(years)
ice_sheet_pr <- ice_sheet
ice_sheet[ice_sheet == 0] <- NA

crs(ice_sheet_pr) <- "EPSG:4326"

occ <- rast(lapply(years,function(year){
  
  print(year)
  
  occ <-lapply(1:nrow(models), function(i){
    mod <- models[i,]
    
    output <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    output <- terra::unwrap(output)
    
    # area occupied by the species
    occ <- ifel(output == 2,1,0)
    
    return(occ)})
  
  occ <- mean(rast(occ))
  
  return(occ)}
))
occ <- crop(occ, ext(-10,30,34,66))
names(occ) <- as.character(years)


occ_plots <- ggplot() +
  geom_spatraster(data = occ) +
  facet_wrap(~lyr, ncol = 6) +
  scale_fill_gradient(
    limits = c(0,1),
    low = "#c9e4ca",
    high = "#55828b",
    na.value = "#f7f7f9") +
  new_scale_fill() + 
  geom_spatraster(data = ice_sheet) +
  geom_spatraster_contour(data = ice_sheet_pr, linewidth = 0.2, breaks = c(100)) +
  scale_fill_gradient2(
    low = "#e9e8ef", 
    mid = "#c9ccde", 
    high = "#bfdbf7", 
    na.value = NA,
    limits = c(0,100), 
    breaks = c(0,50,100), 
    midpoint = 50) +
  theme_void() +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(legend.position = "none",
        panel.spacing = unit(0.2, "cm"),
        plot.margin = margin(0,0,0,0, "cm"),
        strip.text.x = element_text(margin = margin(0,0,0.2,0, "cm")),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2)) +
  coord_sf(ylim=c(36, 66), xlim = c(-10, 30), clip = "on")

suit <- rast(lapply(years,function(year){
  
  print(year)
  
  suit <-lapply(1:nrow(models), function(i){
    mod <- models[i,]
    
    output <- readRDS(file.path(mod$simfolder, paste0(year, "BP.rds")))
    output <- terra::unwrap(output)
    
    # area occupied by the species
    suit <- ifel(output > 0,1,0)
    
    return(suit)})
  
  suit <- mean(rast(suit))
  
  return(suit)}
))
suit <- crop(suit, ext(-10,30,34,66))
names(suit) <- as.character(years)

suit_plots <- ggplot() +
  geom_spatraster(data = suit) +
  facet_wrap(~lyr, ncol = 6) +
  scale_fill_gradient(
    limits = c(0,1),
    low = "#c9e4ca",
    high = "#55828b",
    na.value = "#f7f7f9") +
  new_scale_fill() + 
  geom_spatraster(data = ice_sheet) +
  geom_spatraster_contour(data = ice_sheet_pr,  linewidth = 0.2, breaks = c(100)) +
  scale_fill_gradient2(
    low = "#e9e8ef", 
    mid = "#c9ccde", 
    high = "#bfdbf7", 
    na.value = NA,
    limits = c(0,100), 
    breaks = c(0,50,100), 
    midpoint = 50) +
  theme_void() +
  scale_x_continuous(expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(legend.position = "none",
        panel.spacing = unit(0.2, "cm"),
        plot.margin = margin(0,0,0,0, "cm"),
        strip.text.x = element_text(margin = margin(0,0,0.2,0, "cm")),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2)) +
  coord_sf(ylim=c(36, 66), xlim = c(-10, 30), clip = "on")


ggsave(filename= file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/consensus", "aalba_suitable.pdf"), 
       plot= suit_plots, height=10, width=12)

ggsave(filename= file.path("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/simulation/consensus", "aalba_occupied.pdf"), 
       plot= occ_plots, height=10, width=12)



