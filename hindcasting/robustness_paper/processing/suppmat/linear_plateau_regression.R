
#------------------------------------------------------------#
# Linear-plateau regression, between Sorensen index and time #
#------------------------------------------------------------#

library(minpack.lm)
library(nlraa)
library(propagate)

#---------------#
# 1. Fit models #
#---------------#

p_csdm <- nlsLM(formula = mig_sorensen ~ SSlinp(12000-year, a, b, jp), 
                data = model_performance_withmig[model_performance_withmig$type == "1Correlative",]) # weights = 1/sd_var^2 ?

p_exppb <- nlsLM(formula = mig_sorensen ~ SSlinp(12000-year, a, b, jp),
                 data = model_performance_withmig[model_performance_withmig$type == "3Expertprocessbased",])

p_fitpb <- nlsLM(formula = mig_sorensen ~ SSlinp(12000-year, a, b, jp),
                 data = model_performance_withmig[model_performance_withmig$type == "2Fittedprocessbased",])

rsquared_csdm <- round(modelr::rsquare(p_csdm, model_performance_withmig[model_performance_withmig$type == "1Correlative",]), 2)
rsquared_exppb <- round(modelr::rsquare(p_exppb, model_performance_withmig[model_performance_withmig$type == "3Expertprocessbased",]), 2)
rsquared_fitpb <- round(modelr::rsquare(p_fitpb, model_performance_withmig[model_performance_withmig$type == "2Fittedprocessbased",]), 2)

#-----------------------------------#
# 2. Calculate confidence intervals #
#-----------------------------------#

newdata <- data.frame(year=seq(12000,0, length.out=100))
#newerror <- data.frame(year=seq(500,500, length.out=100))

conf_intervals <- rbind(
  data.frame(newdata,mig_sorensen=predict(p_csdm,newdata),
             intconf = predictNLS(p_csdm, newdata=newdata,
                                  interval="confidence", alpha=0.05, nsim=10000)$summary,
             type="1Correlative",wts=FALSE),
  data.frame(newdata,mig_sorensen=predict(p_exppb,newdata),
             intconf = predictNLS(p_exppb, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type="3Expertprocessbased",wts=FALSE),
  data.frame(newdata,mig_sorensen=predict(p_fitpb,newdata),
             intconf = predictNLS(p_fitpb, newdata=newdata, interval="confidence", alpha=0.05, nsim=10000)$summary,
             type="2Fittedprocessbased",wts=FALSE)
)



#-------------------------------#
# 3. Compute points of interest #
#-------------------------------#

# cSDMs critical point
critpoint_csdm <- coef(p_csdm)[["jp"]]
critpoint_csdm_y <- coef(p_csdm)[["a"]]+critpoint_csdm*coef(p_csdm)[["b"]]
critpoint_csdm_confint <- nlstools::confint2(p_csdm, "jp")





