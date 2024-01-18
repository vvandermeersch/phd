
# see: https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
# https://discourse.mc-stan.org/t/use-of-95-ci-for-measurement-error-instead-of-se-in-brms/22126/5


# somewhat deprecated:
ord_fit_mean <- ordbetareg(formula= bf(mig_sorensen ~ me(hypervolume_sorensen, sd_hyp_sor)*type)+set_mecor(FALSE), 
                           data=model_performance_withmig[model_performance_withmig$type != "3Expertprocessbasedincomplete",],
                           backend="cmdstanr",
                           control=list(adapt_delta=0.95),
                           chains = 1,
                           cores = 1,
                           iter = 500,
                           true_bounds = c(0,1),
                           silent = 0)

# better?
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
                           silent = 0)


ord_pred <- conditional_effects(ord_fit_mean, "clim_hpv_sorensen.mean:type", resp = "migsorensen")[[1]]
