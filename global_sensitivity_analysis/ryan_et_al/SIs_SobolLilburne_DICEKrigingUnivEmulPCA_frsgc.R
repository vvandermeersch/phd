#Computing sensitivity indices using an average of 8 different variations of the Sobol method
#described in Lilburne & Tarantola (2009)

#Home laptop:
#setwd("C:/Users/Ed/Documents/3PostDocLEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Work machine:
setwd("C:/Users/Public/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Surface:
#setwd("C:/Users/Ed/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")

#Inputs
mn=1  #Which model to use
Npc=5  #No. of principal components to use.  Npc=5 was based on output from running script up to line 50.  
N=10000  

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

if(mn==1){modelN='FRSGC'; Npixels=(128*64)/4}
if(mn==2){modelN='GISS'; Npixels=(144*90)/6}

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

S <- var(Y)
S.eig <- eigen(S)
eig.value.cumul = cumsum((S.eig$values/sum(S.eig$values))*100)
yALL.dim.index = c(1:dim(yALL)[2])
Npca=min(yALL.dim.index[eig.value.cumul>99])
#1st five PCs contain 99.9% of the variance, so use PC1-PC5.
min.z=rep(-9999,Npca); max.z=rep(-9999,Npca);


###############################################################
# Use Sobol method (described in Lilburne & Tarantola (2009): #
###############################################################

ptm <- proc.time()

AB <- randomLHS(N,16)
A <- AB[,1:8]
B <- AB[,9:16]

yA_PCA=matrix(-9999,N,Npixels)
yB_PCA=matrix(-9999,N,Npixels)

for (j in 1:Npc){
  PC <- matrix(rep(S.eig$vectors[,j],80),nrow=80,ncol=Npixels,byrow=TRUE)
  y_PCj <- as.matrix(rowSums(PC*Y),rownames.force=NA)
  assign(paste("m",j, sep=""),km(~ ., design = X, response = y_PCj, covtype = "matern5_2"))
  yA_PCA[,j] <- predict.km(eval(as.symbol(paste("m",j,sep=""))), A, "UK", se.compute = FALSE, checkNames = FALSE)$mean
  yB_PCA[,j] <- predict.km(eval(as.symbol(paste("m",j,sep=""))), B, "UK", se.compute = FALSE, checkNames = FALSE)$mean
}
for (j in (Npc+1):Npixels){
  #if (S.eig$values[i]>0.01){yA_PCA[,i]=matrix(rnorm(48,0,S.eig$values[i]),nrow=48,ncol=1)}
  #else {yA_PCA[,i]=matrix(0,nrow=48,ncol=1)}
  yA_PCA[,j]=matrix(0,nrow=N,ncol=1)
  yB_PCA[,j]=matrix(0,nrow=N,ncol=1)
}

PCs.inv <- solve(S.eig$vectors)
yA <- yA_PCA%*%PCs.inv
yB <- yB_PCA%*%PCs.inv


yCi <- matrix(-9999,nrow=N,ncol=Npixels*8)
yDi <- matrix(-9999,nrow=N,ncol=Npixels*8)
for (i in 1:8){
  print(c(i))  
  Ci=B
  Ci[,i]=A[,i]
  Di=A
  Di[,i]=B[,i]
  yCj_PCA <- matrix(-9999,nrow=N,ncol=Npixels)
  yDj_PCA <- matrix(-9999,nrow=N,ncol=Npixels)
  for (j in 1:Npc){
    yCj_PCA[,j] <- predict.km(eval(as.symbol(paste("m",j,sep=""))), Ci, "UK", se.compute = FALSE, checkNames = FALSE)$mean
    yDj_PCA[,j] <- predict.km(eval(as.symbol(paste("m",j,sep=""))), Di, "UK", se.compute = FALSE, checkNames = FALSE)$mean
  }
  for (j in (Npc+1):Npixels){
    yCj_PCA[,j] <- matrix(0,nrow=N,ncol=1)
    yDj_PCA[,j] <- matrix(0,nrow=N,ncol=1)
  }
  i2 <- i*Npixels
  i1 <- i2 - (Npixels-1)
  yCi[,i1:i2] <- yCj_PCA%*%PCs.inv
  yDi[,i1:i2] <- yDj_PCA%*%PCs.inv
}

SI <- matrix(-9999,nrow=8,ncol=Npixels)
for (i in 1:Npixels){
  yA_sum <- mean(yA[,i])
  yB_sum <- mean(yB[,i])
  yAyA_sum <- mean(yA[,i]^2)
  yByB_sum <- mean(yB[,i]^2) 
  SI_temp <- rep(-9999,8)
  for (j in 1:8){
    z <- ((j-1)*Npixels) + i
    yCi_sum <- mean(yCi[,z])
    yDi_sum <- mean(yDi[,z])
    yCiyCi_sum <- mean(yCi[,z]^2)
    yDiyDi_sum <- mean(yDi[,z]^2)
    yAyCi_sum <- mean(yA[,i]*yCi[,z])
    yAyDi_sum <- mean(yA[,i]*yDi[,z])
    yByCi_sum <- mean(yB[,i]*yCi[,z])
    yByDi_sum <- mean(yB[,i]*yDi[,z]) 
    SI_temp[1] <- (yAyCi_sum - (yA_sum*yB_sum))/(yAyA_sum - (yA_sum*yB_sum))
    SI_temp[2] <- (yByDi_sum - (yA_sum*yB_sum))/(yByB_sum- (yA_sum*yB_sum))
    SI_temp[3] <- (yAyCi_sum - (yA_sum*yB_sum))/(yByB_sum - (yA_sum*yB_sum))
    SI_temp[4] <- (yAyCi_sum - (yCi_sum*yDi_sum))/(yCiyCi_sum - (yCi_sum*yDi_sum))
    SI_temp[5] <- (yAyCi_sum - (yCi_sum*yDi_sum))/(yDiyDi_sum - (yCi_sum*yDi_sum))
    SI_temp[6] <- (yByDi_sum - (yA_sum*yB_sum))/(yAyA_sum - (yA_sum*yB_sum))
    SI_temp[7] <- (yByDi_sum - (yCi_sum*yDi_sum))/(yCiyCi_sum - (yCi_sum*yDi_sum))
    SI_temp[8] <- (yByDi_sum - (yCi_sum*yDi_sum))/(yDiyDi_sum - (yCi_sum*yDi_sum))
    SI[j,i] <- mean(SI_temp)
  }
}
SI

# Stop the clock
proc.time() - ptm

write.csv(data.frame(SI*100), file = paste("SIs_SobolLilburne_DICEKrigingUnivEmulPCA_",modelN,"_N",N,".csv",sep=""), row.names=FALSE)
