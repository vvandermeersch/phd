#----------------------------#
# Linear-plateau regressions #
#----------------------------#


#---------------#
# 1. Fit models #
#---------------#

p_tree <- nlsLM(formula = tss ~ SSlinp(2-median, a, b, jp), 
                data = model_performance_withmig[model_performance_withmig$type2 == "1Treebased",]) # weights = 1/sd_var^2 ?

p_reg <- nlsLM(formula = tss ~ SSlinp(2-median, a, b, jp), 
               data = model_performance_withmig[model_performance_withmig$type2 == "2Regressionbased",])

p_exppb <- nlsLM(formula = tss ~ SSlinp(2-median, a, b, jp),
                 data = model_performance_withmig[model_performance_withmig$type2 == "3Expertprocessbased",])

p_fitpb <- nlsLM(formula = tss ~ SSlinp(2-median, a, b, jp),
                 data = model_performance_withmig[model_performance_withmig$type2 == "4Fittedprocessbased",])

p_csdm <- nlsLM(formula = tss ~ SSlinp(2-median, a, b, jp), 
                data = model_performance_withmig[model_performance_withmig$type2 == "2Regressionbased" | 
                                                   model_performance_withmig$type2 == "1Treebased",])

p_pb <- nlsLM(formula = tss ~ SSlinp(2-median, a, b, jp), 
              data = model_performance_withmig[model_performance_withmig$type2 == "3Expertprocessbased" | 
                                                 model_performance_withmig$type2 == "4Fittedprocessbased",])


rsquared_tree <- round(modelr::rsquare(p_tree, model_performance_withmig[model_performance_withmig$type2 == "1Treebased",]), 2)
rsquared_reg <- round(modelr::rsquare(p_reg, model_performance_withmig[model_performance_withmig$type2 == "2Regressionbased",]), 2)
rsquared_exppb <- round(modelr::rsquare(p_exppb, model_performance_withmig[model_performance_withmig$type2 == "3Expertprocessbased",]), 2)
rsquared_fitpb <- round(modelr::rsquare(p_fitpb, model_performance_withmig[model_performance_withmig$type2 == "4Fittedprocessbased",]), 2)

#-----------------------------------#
# 2. Calculate confidence intervals #
#-----------------------------------#

newdata <- data.frame(median=seq(min(model_performance_withmig$median),1.94, length.out=100))

conf_intervals <- rbind(
  data.frame(newdata,tss=predict(p_tree,newdata),
             intconf = predictNLS(p_tree, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="1Treebased",wts=FALSE),
  data.frame(newdata,tss=predict(p_reg,newdata),
             intconf = predictNLS(p_reg, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="2Regressionbased",wts=FALSE),
  data.frame(newdata,tss=predict(p_exppb,newdata),
             intconf = predictNLS(p_exppb, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="3Expertprocessbased",wts=FALSE),
  data.frame(newdata,tss=predict(p_fitpb,newdata),
             intconf = predictNLS(p_fitpb, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="4Fittedprocessbased",wts=FALSE),
  data.frame(newdata,tss=predict(p_csdm,newdata),
             intconf = predictNLS(p_csdm, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type2="Correlative",wts=FALSE)
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




