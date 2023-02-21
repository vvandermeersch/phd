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
X <- inputs_norm

#read in training outputs
outputs_IN <-read.csv(paste('Outputs_5degby5deg_tCH_',modelN,'.csv',sep=""),header=FALSE)
outputs <- outputs_IN[1:80,]

SI <- matrix(-9999,nrow=8,ncol=Npixels)

#EXTRA STUFF:
Mean_AbsResid <- matrix(-9999,nrow=8,ncol=Npixels)
Var_AbsResid <- matrix(-9999,nrow=8,ncol=Npixels)
for (j in 1:1){ #Npixels){
  print(c(j))
  Y<-outputs[,j]
  for (i in 1:8){
    #Use generalised additive modelling (GAM) to estimate g(xi)=E(Y|Xi=xi)
    gam.model1 <- gam(Y ~ s(X[,i]))
    #Mean_AbsResid[i,j] <- mean(abs(Y-gam.model$fitted))
    #Var_AbsResid[i,j] <- var(abs(Y-gam.model$fitted))
  }
}
#Took about 3 mins to run this R script.

#write.csv(data.frame(Mean_AbsResid), file = paste("Table2_GAM_MeanAbsResids_",modelN,".csv",sep=""), row.names=FALSE)
#write.csv(data.frame(Var_AbsResid), file = paste("Table2_GAM_VarAbsResids_",modelN,".csv",sep=""), row.names=FALSE)

for (j in 1:1){ #Npixels){
  print(c(j))
  Y<-outputs[,j]
  x1=X[,1]; x2=X[,2]; x3=X[,3]; x4=X[,4]; x5=X[,5]; x6=X[,6]; x7=X[,7]; x8=X[,8]; 
  gam.model1 <- gam(Y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8))
  EY1=gam.model1$fitted
  abs(EY1-Y)
  gam.model2 <- gam(Y ~ ti(X1)+ti(X2)+ti(X3)+ti(X4)+ti(X5)+ti(X6)+ti(X7)+ti(X8)+ti(X1,x2)+ti(X1,x3)+ti(X1,x4)+ti(X1,x5)+ti(X1,x6)+ti(X1,x7)+ti(X1,x8)+ti(X2,x3)+ti(X2,x4)+ti(X2,x5)+ti(X2,x6)+ti(X2,x7)+ti(X2,x8))
  EY2=gam.model2$fitted
  abs(EY1-Y)
  abs(EY2-Y)
  mean(abs(EY1-Y))
  mean(abs(EY2-Y))
}

