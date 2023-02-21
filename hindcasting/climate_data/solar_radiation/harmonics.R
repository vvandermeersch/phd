library(data.table)
glo2000 <- fread("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_glo_2000_dly.fit")

glo <- t(glo2000[54000,3:368])

n <- 31
glo_monthly <- aggregate(glo, list(rep(1:(nrow(glo) %/% n + 1), each = n, len = nrow(glo))), mean)[-1];
glo_monthly <- data.frame(glo = glo_monthly,
                          month = c(17, 47, 75, 105, 135, 162, 198, 228, 258, 288, 318, 344)/30)
names(glo_monthly)[1] <- "glo"
glo_monthly[7, "glo"] <- glo_monthly[7, "glo"] +3



ssp <- spectrum(glo_monthly$glo)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(glo ~ sin(2*pi/per*month)+cos(2*pi/per*month), data = glo_monthly)
summary(reslm)

rg <- diff(range(glo_monthly$glo))
plot(glo_monthly$glo~glo_monthly$month,ylim=c(min(glo_monthly$glo)-0.1*rg,max(glo_monthly$glo)+0.1*rg))
lines(fitted(reslm)~glo_monthly$month,col=4,lty=2)   # dashed blue line is sin fit

# including 2nd harmonic 
reslm2 <- lm(glo ~ sin(2*pi/per*month)+cos(2*pi/per*month)+sin(4*pi/per*month)+cos(4*pi/per*month), data = glo_monthly)
summary(reslm2)
lines(fitted(reslm2)~glo_monthly$month,col=3)

newdata <- data.frame(month = seq(1,365, 1)/30)
newdata$glo <- predict(reslm2, newdata = newdata, type = "response")
lines(newdata$glo ~ newdata$month,col= "orange")

false_clind <- rnorm(365, 0.1, 0.1)
false_clind[false_clind < 0] <- 0
false_clind[false_clind > 0.4] <- 0.4
newdata$clind <- 1-false_clind
newdata$glo_clind <- newdata$glo * newdata$clind
plot(newdata$glo_clind ~ newdata$month, col = 2, ylim=c(min(glo_monthly$glo)-0.1*rg,max(glo_monthly$glo)+0.1*rg))
lines(newdata$glo ~ newdata$month,col= "orange")
lines(newdata$glo_clind ~ newdata$month, col = 2)
