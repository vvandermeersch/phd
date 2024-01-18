#----------------------------#
# Linear-plateau regressions #
#----------------------------#


# model_performance_lp <- model_performance[model_performance$mod != "CASTANEA" & model_performance$mod != "CASTANEA (fitted)" &
#                                             model_performance$mod != "CASTANEA (fitted) - CO2 fixed" &
#                                             model_performance$mod != "CASTANEA - CO2 fixed",]

model_performance_lp <- model_performance
model_performance_lp <- model_performance_lp %>%
  group_by(type, median) %>%
  dplyr::summarise(median_var=mean(!!sym(var)), sd_var=sd(!!sym(var))) %>%
  as.data.frame()



#---------------#
# 1. Fit models #
#---------------#

p_csdm <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp), 
                data = model_performance_lp[model_performance_lp$type == "cSDM",]) # weights = 1/sd_var^2 ?

p_phenofit <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp),
                    data = model_performance_lp[model_performance_lp$type == "PHENOFIT",])

p_phenofitfitted <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp),
                          data = model_performance_lp[model_performance_lp$type == "PHENOFIT (fitted)",])

p_castanea <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp),
                          data = model_performance_lp[model_performance_lp$type == "CASTANEA",])

# p_phenofit <- nlsLM(formula = median_var ~ a+b*(2-median),
#                     data = model_performance_lp[model_performance_lp$type == "PHENOFIT",],
#                     start = list("a" = 0.3, "b" = 0))

# p_phenofitfitted <- nlsLM(formula = median_var ~ a+b*(2-median),
#                           data = model_performance_lp[model_performance_lp$type == "PHENOFIT (fitted)",],
#                           start = list("a" = 0.4, "b" = 0))

rsquared_phenofit <- round(modelr::rsquare(p_phenofit, model_performance_lp[model_performance_lp$type == "PHENOFIT",]), 2)
rsquared_phenofitfitted <- round(modelr::rsquare(p_phenofitfitted, model_performance_lp[model_performance_lp$type == "PHENOFIT (fitted)",]), 2)
rsquared_csdm <- round(modelr::rsquare(p_csdm, model_performance_lp[model_performance_lp$type == "cSDM",]), 2)
rsquared_castanea <- round(modelr::rsquare(p_castanea, model_performance_lp[model_performance_lp$type == "CASTANEA",]), 2)

#-----------------------------------#
# 2. Calculate confidence intervals #
#-----------------------------------#

newdata <- data.frame(median=seq(min(model_performance_lp$median),1.94, length.out=100))

conf_intervals <- rbind(
  data.frame(newdata,median_var=predict(p_csdm,newdata),
             intconf = predictNLS(p_csdm, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type="cSDM",wts=FALSE),
  data.frame(newdata,median_var=predict(p_phenofit,newdata),
             intconf = predictNLS(p_phenofit, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type="PHENOFIT",wts=FALSE),
  data.frame(newdata,median_var=predict(p_phenofitfitted,newdata),
             intconf = predictNLS(p_phenofitfitted, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type="PHENOFIT (fitted)",wts=FALSE),
  data.frame(newdata,median_var=predict(p_castanea,newdata),
             intconf = predictNLS(p_castanea, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type="CASTANEA",wts=FALSE)
)



#-------------------------------#
# 3. Compute points of interest #
#-------------------------------#

# cSDMs critical point
critpoint_csdm <- coef(p_csdm)[["jp"]]
critpoint_csdm_y <- coef(p_csdm)[["a"]]+critpoint_csdm*coef(p_csdm)[["b"]]
critpoint_csdm_confint <- nlstools::confint2(p_csdm, "jp")

# PHENOFIT critical point
critpoint_phenofit <- coef(p_phenofit)[["jp"]]
critpoint_phenofit_y <- coef(p_phenofit)[["a"]]+critpoint_phenofit*coef(p_phenofit)[["b"]]


# Find intersection of cSDM and PHENOFIT
# A <- matrix(c(coef(p_phenofit)[["b"]], -1,
#               coef(p_csdm)[["b"]], -1), byrow = T, nrow = 2)
# 
# b <- c(-coef(p_phenofit)[["a"]], -coef(p_csdm)[["a"]])
# inter_coords <- solve(A, b)




