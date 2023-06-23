


library(nlme)

# lF <- formula(auc~K*n0*exp(r*median)/(K+n0*(exp(r*median)-1)) | type)

dat <- model_performance[,c("auc", "median", "type")]

dat <- dat %>% group_by(median, type) %>%
  summarize(meanauc = mean(auc), q25auc = quantile(auc, 0.25), q75auc = quantile(auc, 0.75))


fit2<-smooth.spline(dat$median, dat$meanauc,cv = TRUE, df = 4)

m <- nlsList(lF, data=dat ,start=list(K=0.75,n0=0.5,r=4))
dat$median <- max(dat$median)-dat$median

m <- nls(auc~K*n0*exp(r*median)/(K+n0*(exp(r*median)-1)), data = dat[dat$type == "PHENOFIT",])

plot(dat$median,dat$meanauc)
lines(fit2,lwd=2,col="purple")

lines((dat[dat$type == "PHENOFIT (fitted)",]$median),predict(m),col="red",lty=2,lwd=3)


dat$Pred <-predict(m)
#plot
ggplot()+
  geom_point(data = model_performance[model_performance$type == "cSDM",c("auc", "median", "type")], 
             aes(x=median,y=auc, color=type)) +
  geom_line(data =dat, aes(x=max(model_performance$median)-median,y=Pred,color=type))

dat$type <- as.factor(dat$type)
fo_group <- q25auc~K[type]*n0[type]*exp(r[type]*median)/(K[type]+n0[type]*(exp(r[type]*median)-1))

fit_group <- nls(fo_group, data = dat, 
            start = list(K=c(0.8, 0.7, 0.7),n0=rep(0.5,3),r=rep(5,3)),
            na.action = na.omit,
            control = list(tol = 10))

dat$Pred <-predict(fit_group)
ggplot()+
  geom_point(data = model_performance, aes(x=median,y=auc,color=type)) +
  geom_line(data = dat,aes(x=max(model_performance$median)-median,y=Pred,color=type))


fo_ungroup <- auc~K*n0*exp(r*median)/(K+n0*(exp(r*median)-1))
fit_ungroup <- nls(fo_ungroup, data = dat, 
                 start = list(K=0.75,n0=0.55,r=2),
                 na.action = na.omit)

anova(fit_ungroup, fit_group)



Ks <- c(0.8,0.75,0.75) 
n0 <- c(0.4,0.45,0.45)
r <- c(4,2, 1)
time <- seq(0,2,0.1)
#this function returns population dynamics following
#a logistic curves
logF <- function(time,K,n0,r){
  d <- K * n0 * exp(r*time) / (K + n0 * (exp(r*time) - 1))
  return(d)
}
#simulate some data
dat <- data.frame(Treatment=character(),Time=numeric(),
                  Abundance=numeric())
for(i in 1:3){
  Ab <- logF(time = time,K=Ks[i],n0=n0[i],r=r[i])
  tmp <- data.frame(Treatment=paste0("T",i),Time=time,
                    Abundance=Ab)
  #note that random deviates were added to the simulated 
  #population density values
  dat <-rbind(dat,tmp)
}
ggplot(dat,aes(x=-Time,y=Abundance,color=Treatment))+
  geom_point()



fit <- nls(auc ~ SSlogis(median, Asym = 0.7, xmid = 1, scal = 1), data=dat)
summary(fit)

SSlogis()


c(Asym = 0.7, b = 0.07, K = 0.5)




