#----------------------------#
# Linear-plateau regressions #
#----------------------------#


# model_performance_lp <- model_performance[model_performance$mod != "CASTANEA" & model_performance$mod != "CASTANEA (fitted)" &
#                                             model_performance$mod != "CASTANEA (fitted) - CO2 fixed" &
#                                             model_performance$mod != "CASTANEA - CO2 fixed",]

model_performance_lp <- model_performance
model_performance_lp <- model_performance_lp %>%
  group_by(type2, median) %>%
  dplyr::summarise(median_var=median(!!sym(var)), sd_var=sd(!!sym(var))) %>%
  as.data.frame()



#---------------#
# 1. Fit models #
#---------------#

p_tree <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp), 
                data = model_performance_lp[model_performance_lp$type == "Treebased",]) # weights = 1/sd_var^2 ?

p_reg <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp), 
               data = model_performance_lp[model_performance_lp$type == "Regressionbased",])

p_exppb <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp),
                    data = model_performance_lp[model_performance_lp$type2 == "Expertprocessbased",])

p_fitpb <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp),
                          data = model_performance_lp[model_performance_lp$type == "Fittedprocessbased",])

p_csdm <- nlsLM(formula = median_var ~ SSlinp(2-median, a, b, jp), 
                data = model_performance_lp[model_performance_lp$type == "Regressionbased" | 
                                              model_performance_lp$type == "Treebased",])



rsquared_tree <- round(modelr::rsquare(p_tree, model_performance_lp[model_performance_lp$type == "Treebased",]), 2)
rsquared_reg <- round(modelr::rsquare(p_reg, model_performance_lp[model_performance_lp$type == "Regressionbased",]), 2)
rsquared_exppb <- round(modelr::rsquare(p_exppb, model_performance_lp[model_performance_lp$type == "Expertprocessbased",]), 2)
rsquared_fitpb <- round(modelr::rsquare(p_fitpb, model_performance_lp[model_performance_lp$type == "Fittedprocessbased",]), 2)

#-----------------------------------#
# 2. Calculate confidence intervals #
#-----------------------------------#

newdata <- data.frame(median=seq(min(model_performance_lp$median),1.94, length.out=100))

conf_intervals <- rbind(
  data.frame(newdata,median_var=predict(p_tree,newdata),
             intconf = predictNLS(p_tree, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="Treebased",wts=FALSE),
  data.frame(newdata,median_var=predict(p_reg,newdata),
             intconf = predictNLS(p_reg, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="Regressionbased",wts=FALSE),
  data.frame(newdata,median_var=predict(p_exppb,newdata),
             intconf = predictNLS(p_exppb, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="Expertprocessbased",wts=FALSE),
  data.frame(newdata,median_var=predict(p_fitpb,newdata),
             intconf = predictNLS(p_fitpb, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="Fittedprocessbased",wts=FALSE)
)



#-------------------------------#
# 3. Compute points of interest #
#-------------------------------#

# cSDMs critical point
critpoint_rbcsdm <- coef(p_csdm)[["jp"]]
critpoint_rbcsdm_y <- coef(p_csdm)[["a"]]+critpoint_rbcsdm*coef(p_csdm)[["b"]]
critpoint_rbcsdm_confint <- nlstools::confint2(p_csdm, "jp")

# PHENOFIT critical point
# critpoint_phenofit <- coef(p_phenofit)[["jp"]]
# critpoint_phenofit_y <- coef(p_phenofit)[["a"]]+critpoint_phenofit*coef(p_phenofit)[["b"]]


# Find intersection of cSDM and PHENOFIT
# A <- matrix(c(coef(p_phenofit)[["b"]], -1,
#               coef(p_csdm)[["b"]], -1), byrow = T, nrow = 2)
# 
# b <- c(-coef(p_phenofit)[["a"]], -coef(p_csdm)[["a"]])
# inter_coords <- solve(A, b)




