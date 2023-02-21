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

################################################
# Perform Validation                           #
################################################
preds <- matrix(-9999,nrow=24,ncol=Npixels)
ptm <- proc.time()
for (j in 1:Npixels){
  print(c(j))
  Y<-outputs[,j]
  dat=data.frame(cbind(Y,X_ALL))
  b <- gam(Y ~ s(X1)+s(X2)+s(X3)+s(X4)+s(X5)+s(X6)+s(X7)+s(X8),data=dat)
  Xv <- X_ALL_val
  nd2 <- data.frame(Xv)
  preds[,j] <- predict(b,nd2)
  #preds <- predict(b,nd2,se=TRUE)
}
proc.time() - ptm 
write.csv(data.frame(preds), file = paste("ValidationPreds_GAM_NoEmul_",modelN,".csv",sep=""), row.names=FALSE)




