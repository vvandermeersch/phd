#Estimate Sensitivity indices by using generalised additive modelling (GAM) 
#to estimate g(xi)=E(Y|Xi=xi)
#With 36 outputs this takes 11 seconds

#Inputs
modelN='FRSGC'; Npixels=(128*64)/4
#modelN='GISS'; Npixels=(144*90)/6

#Home laptop:
setwd("C:/Users/Ed/Documents/3PostDocLEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Work machine:
#setwd("C:/Users/Public/Documents/Post_doc_LEC/aper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Surface:
#setwd("C:/Users/Ed/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")

#install.packages("mgcv")
library(mgcv)

#read in training and validation inputs:
inputs_ALL_norm <-read.csv('Inputs_altern_trainingruns_norm_withHeader.csv',header=TRUE)
inputs_norm <- as.matrix(inputs_ALL_norm[1:80,], rownames.force = NA)
X_ALL <- inputs_norm
inputs_VAL_norm <-read.csv('Inputs_altern_valruns_norm_withHeader.csv',header=TRUE)
inputs_norm_val <- as.matrix(inputs_VAL_norm[1:24,], rownames.force = NA)
X_ALL_val <- inputs_norm_val

#read in training outputs
outputs_IN <-read.csv(paste('Outputs_5degby5deg_tCH_',modelN,'.csv',sep=""),header=FALSE)
outputs <- outputs_IN[1:80,]
outputs_val <- outputs_IN[81:104,]

SI <- matrix(-9999,nrow=8,ncol=Npixels)

################################################
# Use regression to estimate Var_{X3}[E(Y|X3)] #
################################################
ptm <- proc.time()
for (j in 1:1){ #Npixels){
  print(c(j))
  Y<-outputs[,j]
  varY=var(Y)
  v<-rep(-9999,8)
  for (i in 1:8){
    #Use generalised additive modelling (GAM) to estimate g(xi)=E(Y|Xi=xi)
    gam.model1 <- gam(Y ~ s(X[,i]))
    v[i]<-var(gam.model1$fitted)
  }
  SI[,j]=(v/varY)*100
}
proc.time() - ptm 
write.csv(data.frame(SI), file = paste("SIs_GAM_NoEmul_",modelN,".csv",sep=""), row.names=FALSE)


################################################
# Estimate SIs using method from GAM paper     #
################################################
SI2 <- matrix(-9999,nrow=8,ncol=Npixels)
ptm <- proc.time()
for (j in 1:1){ #Npixels){
  print(c(j))
  Y<-outputs[,j]
  varY=var(Y)
  dataDF <- data.frame(y=Y,X)
  gam.model2 <- gam(Y ~ s(X1)+s(X2)+s(X3)+s(X4)+s(X5)+s(X6)+s(X7)+s(X8),data=dataDF)
  #gam.model2 <- gam(Y ~ ti(X1)+ti(X2)+ti(X3)+ti(X4)+ti(X5)+ti(X6)+ti(X7)+ti(X8)+ti(X1,x2)+ti(X1,x3)+ti(X1,x4)+ti(X1,x5)+ti(X1,x6)+ti(X1,x7)+ti(X1,x8)+ti(X2,x3)+ti(X2,x4)+ti(X2,x5)+ti(X2,x6)+ti(X2,x7)+ti(X2,x8),data=dataDF)
  #gam.model2 <- gam(Y ~ ti(X1)+ti(X2)+ti(X3)+ti(X4)+ti(X5)+ti(X6)+ti(X7)+ti(X8)+ti(X1,X2)+ti(X1,X3)+ti(X1,X4)+ti(X1,X5)+ti(X1,X6)+ti(X1,X7)+ti(X1,X8),data=dataDF)
  betaHat <- gam.model2$coefficients
  Z <- predict(gam.model2,type="lpmatrix")
  V2<-rep(-9999,8)
  for (i in 1:8){
    if (i==1){X1Marginal <- grep("(X1)",names(betaHat),fixed=TRUE)}
    if (i==2){X1Marginal <- grep("(X2)",names(betaHat),fixed=TRUE)}
    if (i==3){X1Marginal <- grep("(X3)",names(betaHat),fixed=TRUE)}
    if (i==4){X1Marginal <- grep("(X4)",names(betaHat),fixed=TRUE)}
    if (i==5){X1Marginal <- grep("(X5)",names(betaHat),fixed=TRUE)}
    if (i==6){X1Marginal <- grep("(X6)",names(betaHat),fixed=TRUE)}
    if (i==7){X1Marginal <- grep("(X7)",names(betaHat),fixed=TRUE)}
    if (i==8){X1Marginal <- grep("(X8)",names(betaHat),fixed=TRUE)}
    h1Hat <- Z[,X1Marginal]%*%betaHat[X1Marginal]
    V2[i] <- var(h1Hat)
  }
  SI2[,j]=(V2/varY)*100
}
proc.time() - ptm 
write.csv(data.frame(SI2), file = paste("SIs_GAM_NoEmul_",modelN,"_GAMpaper.csv",sep=""), row.names=FALSE)
