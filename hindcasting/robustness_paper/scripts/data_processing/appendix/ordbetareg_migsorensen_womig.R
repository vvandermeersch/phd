
#-------------------------------------------------------------------------------------#
# Ordered Beta regression, between Sorensen index and climatic hypervolume similarity #
#-------------------------------------------------------------------------------------#

run_model <- FALSE

# Run/read model
if(run_model){
  ord_fit_mean <- ordbetareg(formula = 
                               bf(sorensen | mi() ~ mi(clim_hpv_sorensen.mean) * type) + 
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
                             silent = 0)
  saveRDS(ord_fit_mean, file.path(wd, "data", "ordered_beta_regression_fit_womig.rds"))
}else{
  ord_fit_mean <- readRDS(file.path(wd, "data", "ordered_beta_regression_fit_womig.rds"))
}

# Get conditional effects
# ord_pred <- conditional_effects(ord_fit_mean)[[3]]

# Predict
new_data <-  data.frame(clim_hpv_sorensen.mean = rep(c(0.93, unique(model_performance_withmig$clim_hpv_sorensen.mean), 0.7), 3),
                        clim_hpv_sorensen.sd = rep(0.008, unique(model_performance_withmig$clim_hpv_sorensen.sd, 0.011), 3))
new_data$type <- rep(unique(model_performance_withmig$type), each = nrow(new_data)/3)

expected_predictions <- as.data.frame(fitted(ord_fit_mean, newdata = new_data))
expected_predictions$type <- new_data$type
expected_predictions$clim_hpv_sorensen.mean <- new_data$clim_hpv_sorensen.mean

# Summarize model performances (for the seek of clarity in the figure)
model_performance_mean <- model_performance_withmig %>%
  group_by(type, clim_hpv_sorensen.mean = round(clim_hpv_sorensen.mean, 2)) %>%
  dplyr::summarise(mean_sorensen=mean(sorensen), sd_sorensen=sd(sorensen)) %>%
  as.data.frame()

# Bayesian R2
betareg_r2 <- data.frame(round(bayes_R2(ord_fit_mean, resp = "sorensen"),2))

