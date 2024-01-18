
#-------------------------------------------------------------------------------------#
# Ordered Beta regression, between Sorensen index and climatic hypervolume similarity #
#-------------------------------------------------------------------------------------#

run_model <- FALSE

# Run/read model
if(run_model){
  ord_fit_mean <- ordbetareg(formula = 
                               bf(mig_sorensen | mi() ~ mi(clim_hpv_sorensen.mean) * type) + 
                               bf(clim_hpv_sorensen.mean | mi(clim_hpv_sorensen.sd) ~ type) +
                               set_mecor(FALSE) +
                               set_rescor(FALSE), 
                             data= model_performance_withmig,
                             backend="cmdstanr",
                             control=list(adapt_delta=0.95),
                             chains = 4,
                             cores = 4,
                             iter = 2000,
                             true_bounds = c(0,1),
                             silent = 1)
  saveRDS(ord_fit_mean, file.path(wd, "manuscript", "processing/output", "ordered_beta_regression_fit_default.rds"))
}else{
  ord_fit_mean <- readRDS(file.path(wd, "manuscript", "processing/output", "ordered_beta_regression_fit_default.rds"))
}

# Get conditional effects
posterior_summary(ord_fit_mean)
epbm <- hypothesis(ord_fit_mean, 
           "bsp_migsorensen_miclim_hpv_sorensen.mean + bsp_migsorensen_miclim_hpv_sorensen.mean:type3Expertprocessbased = 0", 
           class = NULL)
fpbm <- hypothesis(ord_fit_mean, 
           "bsp_migsorensen_miclim_hpv_sorensen.mean + bsp_migsorensen_miclim_hpv_sorensen.mean:type2Fittedprocessbased = 0", 
           class = NULL)

# Predict
new_data <-  data.frame(clim_hpv_sorensen.mean = rep(c(0.07, unique(model_performance_withmig$clim_hpv_sorensen.mean), seq(0.3,0.34,0.01)), 3),
                        clim_hpv_sorensen.sd = rep(0.008, unique(model_performance_withmig$clim_hpv_sorensen.sd, rep(0.01,5)), 3))
new_data$type <- rep(unique(model_performance_withmig$type), each = nrow(new_data)/3)

expected_predictions <- as.data.frame(fitted(ord_fit_mean, newdata = new_data))
expected_predictions$type <- new_data$type
expected_predictions$clim_hpv_sorensen.mean <- new_data$clim_hpv_sorensen.mean

# Summarize model performances (for the seek of clarity in the figure 3)
model_performance_mean <- model_performance_withmig %>%
  group_by(type, clim_hpv_sorensen.mean = round(clim_hpv_sorensen.mean, 2)) %>%
  dplyr::summarise(mean_migsorensen=mean(mig_sorensen), sd_migsorensen=sd(mig_sorensen)) %>%
  as.data.frame()

# Bayesian R2
betareg_r2 <- data.frame(round(bayes_R2(ord_fit_mean, resp = "migsorensen"),2))

