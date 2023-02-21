#Computing sensitivity indices using fast99 R package and the DICE-Kriging univariate emulator. 
#Using 36 outputs takes 60 seconds.

#model=['frsgc']; mn=1; Nlong=128; Nlat=64; Nlevel=28; %size(airmass) = [128 64 37 12] = [long lat alt month]
#model=['giss']; mn=2;  Nlong=144; Nlat=90; Nlevel=21; %size(airmass) = [144 90 40 12] = [long lat alt month]

#Inputs
modelN='FRSGC'; Npixels=(128*64)/4
#modelN='GISS'; Npixels=(144*90)/6

#Home laptop:
setwd("C:/Users/Ed/Documents/3PostDocLEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Surface:
#setwd("C:/Users/Ed/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")
#Dell:
#setwd("C:/Users/Public/Documents/Post_doc_LEC/Paper1_FeasabilityStudyExtension/Analysis_tCH_5degby5deg_FRSGC")

#install.packages("pls")
#install.packages("moments")
library(pls)
library(moments)

#read in training and validation inputs:
inputs_ALL <-read.csv('Inputs_altern_trainingruns.csv',header=FALSE)
inputs_ALL_stand <- apply(inputs_ALL[1:104,],2,function(x){(x-mean(x))/sqrt(var(x))})
inputs_stand <- as.matrix(inputs_ALL_stand[1:80,], rownames.force = NA)
X <- inputs_stand
inputs_VAL_norm <-read.csv('Inputs_altern_valruns_norm_withHeader.csv',header=TRUE)
inputs_norm_val <- as.matrix(inputs_VAL_norm[1:24,], rownames.force = NA)
X_val <- inputs_norm_val

skewness.values <- apply(X,2,skewness)
skewness.values
kurt.values <- apply(X,2,kurtosis)
kurt.values

#read in training outputs
outputs_ALL <-read.csv(paste('Outputs_5degby5deg_tCH_',modelN,'.csv',sep=""),header=FALSE)
outputs_ALL_stand <- apply(outputs_ALL,2,function(x){(x-mean(x))/sqrt(var(x))})
Y <- as.matrix(outputs_ALL_stand[1:80,], rownames.force = NA)
Y_val <- outputs_ALL [81:104,]

pls.mod <- plsr(Y ~ X)
summary(pls.mod)
explvar(pls.mod)
pls.mod.coef <- pls.mod$coefficients
SI.temp <- matrix(pls.mod.coef[,,8],nrow=8,ncol=Npixels)
SI <- (abs(SI.temp)/colSums(abs(SI.temp)))*100
write.csv(data.frame(SI), file = paste("SIs_PLS_NoEmul_",modelN,".csv",sep=""), row.names=FALSE)

