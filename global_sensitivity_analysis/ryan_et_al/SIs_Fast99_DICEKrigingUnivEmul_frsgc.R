#Computing sensitivity indices using fast99 R package and the DICE-Kriging univariate emulator. 
#Using 36 outputs takes 60 seconds.

#model=['frsgc']; mn=1; Nlong=128; Nlat=64; Nlevel=28; %size(airmass) = [128 64 37 12] = [long lat alt month]
#model=['giss']; mn=2;  Nlong=144; Nlat=90; Nlevel=21; %size(airmass) = [144 90 40 12] = [long lat alt month]

rm(list = grep("^k", ls(), value = TRUE, invert = TRUE))

#Inputs
N=5000  
modelN='FRSGC'; Npixels=(128*64)/4
#modelN='GISS'; Npixels=(144*90)/6

#Home laptop:
#setwd("C:/Users/Ed/Documents/3PostDocLEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Work machine:
setwd("C:/Users/Public/Documents/Post_doc_LEC/aper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Surface:
#setwd("C:/Users/Ed/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")

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

SI <- matrix(-9999,nrow=8,ncol=Npixels)
ptm <- proc.time()
for (j in 1:Npixels){
  y <- as.matrix(outputs[,j],rownames.force=NA)
  m <- km(~ ., design = X, response = y, covtype = "matern5_2")
  kriging.mean <- function(Xnew, m){
    predict.km(m, Xnew, "UK", se.compute = FALSE, checkNames = FALSE)$mean
  }
  temp <- fast99(model = kriging.mean, factors = 8, n = N, q = "qunif", q.arg = list(min = 0, max = 1), m = m)
  SI[,j] <- as.matrix(temp$D1/temp$V)
}
proc.time() - ptm
#Took about 4 hours to run this R script.

write.csv(data.frame(SI*100), file = paste("SIs_Fast99_DICEKrigingUnivEmul_",modelN,".csv",sep=""), row.names=FALSE)
