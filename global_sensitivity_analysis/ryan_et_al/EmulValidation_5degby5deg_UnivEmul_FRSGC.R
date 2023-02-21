#In this R script, we run the Hankin and DICE-Kriging emulator at the validation inputs for each of hte 36 spatially
#varying outputs.

#Home laptop:
#setwd("C:/Users/Ed/Documents/3PostDocLEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Work machine:
setwd("C:/Users/Public/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Surface:
#setwd("C:/Users/Ed/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")

modelN='FRSGC'; Npixels=(128*64)/4
#modelN='GISS'; Npixels=(144*90)/6

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
y_ALL <- outputs_IN[1:80,]

#Create an empty matrix to store the emulator outputs:
outputs_emul_val <- matrix(-9999,nrow=48,ncol=Npixels)

for (j in 1:Npixels){
  y <- as.matrix(y_ALL[1:80,j], rownames.force = NA)
  for (k in 1:2){ #k=1 is the GP emulator, k=2 is the DICE-Kriging emulator.
    print(c(j,k))
    R2=k*24
    R1=R2-23
    if (k==1){  
      scales.optim <- optimal.scales(val=X, scales.start=rep(1,8),d=y,give=FALSE)
      outputs_emul_val[R1:R2,j] <- interpolant.quick(Xnew, y, X, scales=scales.optim, g=TRUE)$mstar.star
    }
    else if (k==2){  
      m <- km(~ ., design = X, response = y, covtype = "matern5_2")
      outputs_emul_val[R1:R2,j] <- predict.km(m, Xnew, "UK", se.compute = FALSE, checkNames = FALSE)$mean
    }
  }
}

#Write emulator outputs to csv file:
write.csv(data.frame(tCH=outputs_emul_val), file = paste("Outputs_5degby5deg_tCH_",modelN,"_UnivEmulPreds.csv",sep=""), row.names=FALSE)
