#In this R script, we run the Hankin and DICE-Kriging emulator at the validation inputs for each of hte 36 spatially
#varying outputs.

rm(list = ls())

#Home laptop:
#setwd("C:/Users/Ed/Documents/3PostDocLEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Work machine:
#setwd("C:/Users/Public/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Surface:
setwd("C:/Users/Ed/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")


#Inputs
modelN='FRSGC'; Npixels=(128*64)/4; Npc=5
#modelN='GISS'; Npixels=(144*90)/6; Npc=40
#Npc is the no. of principal components to use. The value chosen was based on output from running script up to line 50. 

#install.packages("emulator")
#install.packages("lhs")
#install.packages("mvtnorm")
#install.packages("sensitivity")
#install.packages("DiceKriging")
#install.packages("DiceOptim")
library(lhs)
library(emulator)
library(mvtnorm)
library(sensitivity)
library(DiceKriging)
library(DiceOptim)

#read in training and validation inputs:
inputs_norm_IN <-read.csv('Inputs_altern_trainingruns_norm.csv',header=FALSE)
inputs_norm <- as.matrix(inputs_norm_IN[1:80,], rownames.force = NA)
X <- inputs_norm
inputs_val_norm_IN <-read.csv('Inputs_altern_valruns_norm.csv',header=FALSE)
inputs_val_norm <- as.matrix(inputs_val_norm_IN[1:24,], rownames.force = NA)
Xnew <- inputs_val_norm

#read in training outputs
outputs_IN <-read.csv(paste('Outputs_5degby5deg_tCH_',modelN,'.csv',sep=""),header=FALSE)
Y <- outputs_IN[1:80,]

#Create an empty matrix to store the emulator outputs:
outputs_emul_val <- matrix(-9999,nrow=48,ncol=Npixels)

S <- var(Y)
S.eig <- eigen(S)
PC.ALL <- (S.eig$values/sum(S.eig$values))*100  
sum(PC.ALL[1:Npc])   #1st five PCs contain 99.9% of the variance, so use PC1-PC5.

ynew_PCs=matrix(-9999,nrow=48,ncol=Npixels)
for (i in 1:Npc){
  PC <- matrix(rep(S.eig$vectors[,i],80),nrow=80,ncol=Npixels,byrow=TRUE)
  #PCi <- matrix(S.eig$vectors[,i])
  y_PCi <- as.matrix(rowSums(PC*Y),rownames.force=NA)
  #y_PCi <- Y%*%PCi
  scales.optim <- optimal.scales(val=X, scales.start=rep(1,8),d=y_PCi,give=FALSE)
  ynew_PCs[1:24,i] <- interpolant.quick(Xnew, y_PCi, X, scales=scales.optim, g=TRUE)$mstar.star
  m <- km(~ ., design = X, response = y_PCi, covtype = "matern5_2")
  ynew_PCs[25:48,i] <- predict.km(m, Xnew, "UK", se.compute = FALSE, checkNames = FALSE)$mean
}
for (i in (Npc+1):Npixels){
  #if (S.eig$values[i]>0.01){temp1=matrix(rnorm(48,0,S.eig$values[i]),nrow=48,ncol=1)}
  #else {temp1 <- matrix(0,nrow=48,ncol=1)}
  temp1 <- matrix(0,nrow=48,ncol=1)
  ynew_PCs[,i]=temp1
}

PCs.inv <- solve(S.eig$vectors)
outputs_emul_val1 <- ynew_PCs[1:24,]%*%PCs.inv
outputs_emul_val2 <- ynew_PCs[25:48,]%*%PCs.inv
outputs_emul_val <- rbind(outputs_emul_val1,outputs_emul_val2)

#Write emulator outputs to csv file:
write.csv(data.frame(tCH=outputs_emul_val), file = paste("Outputs_5degby5deg_tCH_",modelN,"_PCAPreds.csv",sep=""), row.names=FALSE)
