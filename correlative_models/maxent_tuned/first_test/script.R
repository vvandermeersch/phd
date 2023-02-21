
library(blockCV)
library(ENMeval)

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/correlative_models"

# environmental data in raster format
bc_covars <- c("bio6", "bio12")
soil_covars <- c("WHC", "pH")
cc_covars <- c("sum_apsep_GDD5", "w_bal")
covars <- c(bc_covars, soil_covars, cc_covars)
source(file.path(wd, "scripts", "predictors_data.R"))
cov_raster <- brick(stack(lapply(predictors_data[, covars], function(j) rasterFromXYZ(cbind(predictors_data[, c("lon", "lat")], j)))))
crs(cov_raster) <- CRS('+init=EPSG:4326')

# load presence and background points
sp_data_dir <- "D:/species/processed"
sp_name <- "fagus_sylvatica"
nb_pres <- 1000
nb_background <- 50000
source(file.path(wd, "scripts", "calibration_points.R"))
sp_presence <- presabs_points[presabs_points$pres == 1,]
pr_bg <- rbind(sp_presence, background)
pr_bg$lat <- round(pr_bg$lat, 1)
pr_bg$lon <- round(pr_bg$lon, 1)

# presence-background in spdf format
pb_data <- st_as_sf(pr_bg, coords = c("lon", "lat"), crs= crs(cov_raster))

# environmental clustering
eb <- envBlock(rasterLayer = cov_raster,
               speciesData = pb_data,
               species = "pres",
               k = 5,
               standardization = "standard", # rescale variables between 0 and 1
               rasterBlock = FALSE,
               numLimit = 50)
pr_bg$fold_id <- eb[["foldID"]]



# running ENMeval with the fivefold environmental blocks generated with blockCV
blockcv_grp <- list(occs.grp = pr_bg[pr_bg$pres == 1, "fold_id"], bg.grp = pr_bg[pr_bg$pres == 0, "fold_id"])
tune_args <- list(fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), rm = 1:5)
enmeval_blockcv <- ENMevaluate(occs = pr_bg[pr_bg$pres == 1, c("lon", "lat")], 
                      envs = stack(cov_raster), 
                      bg = pr_bg[pr_bg$pres == 0, c("lon", "lat")], 
                      algorithm = "maxent.jar", 
                      tune.args = tune_args, 
                      partitions = "user", user.grp = blockcv_grp)

# visualizing tuning results
evalplot.stats(e = enmeval_blockcv, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", dodge = 0.5)

# model selection
## criterion: lowest average test omission rate (and to break ties, highest average validation AUC)
res <- eval.results(enmeval_blockcv)
opt.seq <- res %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq

