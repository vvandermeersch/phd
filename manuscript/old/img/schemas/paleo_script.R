
sim_dir <- "D:/simulations/phenofit/paleo/fitted/025deg/fagus_sylvatica"
years <- 12000
refugia <- lapply(years, function(y) readRDS(file.path(sim_dir, paste0(y,"BP.rds")))[c(2,1,3)] %>% rast()) %>% rast()
refugia <- ifel(refugia < 0.785, 0, 1)

rst <- terra::rast(file.path(sim_dir, "steps_step_11750.asc"))
terra::crs(rst) <- "EPSG:3035" 
rst[rst >= 30000] <- 0
rst[rst <=0] <- NA
rcopy <- rst
res(rcopy) <- terra::res(rcopy)*40 # near 0.1deg
rst <- terra::resample(rst, rcopy, method="bilinear")
rm(rcopy)
rst <- project(rst, "EPSG:4326", method = "bilinear")
rst <- resample(rst, refugia, method = "bilinear") 

years <- 250
area <- lapply(years, function(y) readRDS(file.path(sim_dir, paste0(y,"BP.rds")))[c(2,1,3)] %>% rast()) %>% rast()
area <- ifel(area >=0, 0, 0)

rst2 <- sum(rst, area, na.rm = T)


yr_ICE6G <- y%/%500*0.5
ice_sheet <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), ext(-10,30,34,66))
ice_sheet_pr <- ice_sheet
ice_sheet[ice_sheet == 0] <- NA

area <- ifel(is.na(area), 1, 0)

plot <- ggplot() +
  # geom_spatraster(data = area) +
  # scale_fill_gradient2(
  #   low = "#e3ebf5", 
  #   mid = "#e3ebf5", 
  #   high = "#e3ebf5", 
  #   na.value = NA,
  #   limits = c(0,100), 
  #   breaks = c(0,50,100), 
  #   midpoint = 50) +
  # new_scale_fill() +
  geom_spatraster(data = rst) +
  scale_fill_whitebox_c(palette = "viridi", direction = -1) +
  new_scale_fill() +
  geom_spatraster(data = refugia) +
  scale_fill_gradient2(
    low = NA, 
    mid = NA, 
    high = "darkorange", 
    na.value = NA,
    limits = c(0,1), 
    breaks = c(0,1), 
    midpoint = 0.5) +
  geom_spatraster_contour(data = area, linewidth = 0.2, breaks = c(1), color = "grey40") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_void() + theme(legend.position ='none')

ggsave(plot, filename = file.path("C:/Users/vandermeersch/Documents/CEFE/phd/manuscript/img/schemas", "phenofit_fitted_fagus_migration.pdf"),
       width = 8.1, height = 7.64)





