#Computing sensitivity indices using fast99 R package and the DICE-Kriging univariate emulator. 
#Using 36 outputs takes 60 seconds.

#model=['frsgc']; mn=1; Nlong=128; Nlat=64; Nlevel=28; %size(airmass) = [128 64 37 12] = [long lat alt month]
#model=['giss']; mn=2;  Nlong=144; Nlat=90; Nlevel=21; %size(airmass) = [144 90 40 12] = [long lat alt month]

rm(list = grep("^k", ls(), value = TRUE, invert = TRUE))

#Inputs
N=10000  
modelN='FRSGC'; Npixels=(128*64)/4
#modelN='GISS'; Npixels=(144*90)/6

#Home laptop:
#setwd("C:/Users/Ed/Documents/3PostDocLEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Surface:
#setwd("C:/Users/Ed/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Dell:
setwd("C:/Users/Public/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")

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
inputs_ALL_norm <-read.csv('Inputs_altern_trainingruns_norm.csv',header=FALSE)
inputs_norm <- as.matrix(inputs_ALL_norm[1:80,], rownames.force = NA)
X <- inputs_norm

#read in training outputs
outputs_IN <-read.csv(paste('Outputs_5degby5deg_tCH_',modelN,'.csv',sep=""),header=FALSE)
outputs <- outputs_IN[1:80,]

###############################################################
# Use Sobol method (described in Lilburne & Tarantola (2009): #
###############################################################

SI <- matrix(-9999,nrow=8,ncol=Npixels)
ptm <- proc.time()
for (j in 1:Npixels){
  y <- as.matrix(outputs[,j],rownames.force=NA)
  m <- km(~ ., design = X, response = y, covtype = "matern5_2")
  AB <- randomLHS(N,16)
  A <- AB[,1:8]
  B <- AB[,9:16]
  yA <- predict.km(m, A, "UK", se.compute = FALSE, checkNames = FALSE)$mean
  yB <- predict.km(m, B, "UK", se.compute = FALSE, checkNames = FALSE)$mean
  yA_sum <- mean(yA)
  yB_sum <- mean(yB)
  yAyA_sum <- mean(yA^2)
  yByB_sum <- mean(yB^2)  
  for (i in 1:8){
    print(c(j,i))
    Ci=B
    Ci[,i]=A[,i]
    Di=A
    Di[,i]=B[,i]
    yCi <- predict.km(m, Ci, "UK", se.compute = FALSE, checkNames = FALSE)$mean
    yDi <- predict.km(m, Di, "UK", se.compute = FALSE, checkNames = FALSE)$mean
    yCi_sum <- mean(yCi)
    yDi_sum <- mean(yDi)
    yCiyCi_sum <- mean(yCi^2)
    yDiyDi_sum <- mean(yDi^2)
    yAyCi_sum <- mean(yA*yCi)
    yAyDi_sum <- mean(yA*yDi)
    yByCi_sum <- mean(yB*yCi)
    yByDi_sum <- mean(yB*yDi) 
    SI_temp <- rep(-9999,8)
    SI_temp[1] <- (yAyCi_sum - (yA_sum*yB_sum))/(yAyA_sum - (yA_sum*yB_sum))
    SI_temp[2] <- (yByDi_sum - (yA_sum*yB_sum))/(yByB_sum - (yA_sum*yB_sum))
    SI_temp[3] <- (yAyCi_sum - (yA_sum*yB_sum))/(yByB_sum - (yA_sum*yB_sum))
    SI_temp[4] <- (yAyCi_sum - (yCi_sum*yDi_sum))/(yCiyCi_sum - (yCi_sum*yDi_sum))
    SI_temp[5] <- (yAyCi_sum - (yCi_sum*yDi_sum))/(yDiyDi_sum - (yCi_sum*yDi_sum))
    SI_temp[6] <- (yByDi_sum - (yA_sum*yB_sum))/(yAyA_sum - (yA_sum*yB_sum))
    SI_temp[7] <- (yByDi_sum - (yCi_sum*yDi_sum))/(yCiyCi_sum - (yCi_sum*yDi_sum))
    SI_temp[8] <- (yByDi_sum - (yCi_sum*yDi_sum))/(yDiyDi_sum - (yCi_sum*yDi_sum))
    SI[i,j] <- mean(SI_temp)
  }
}

# Stop the clock
proc.time() - ptm

write.csv(data.frame(SI*100), file = paste("SIs_SobolLilburne_DICEKrigingUnivEmul_",modelN,"_N",N,".csv",sep=""), row.names=FALSE)
