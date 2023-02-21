###############################################
# Comparing and selecting predictors for cSDM #
###############################################

wd <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/predictors"

library(dplyr)
library(corrplot)

# Compute predictors
compute <- FALSE # avoid long computation time
if(compute){
  source(file.path(wd, "compute_predictors.R"))
  predictors <- c(bc_predictors, soil_predictors, cc_predictors)
  predictors_fullnames <- c(bc_fullnames, soil_fullnames, cc_fullnames)
  predictors_data  <- cbind(biovars_30y, data_soil[,-c(1,2)], ccvars_30y[,-c(1,2)]) %>%
    dplyr::select(all_of(predictors))
}else{
  load(file.path(wd, "predictors_data.Rdata"))
  bc_predictors <- c("bio1", "bio6", "bio5", "bio4", "bio12", "bio14", "bio13", "bio15")
  bc_fullnames <- c("Annual mean temperature", "Max. temp. warmest month", "Min. temp. coldest month", "Temperature seasonality", 
                    "Annual precipitation", "Prec. of driest month", "Prec. of wettest month", "Precipitation seasonality")
  soil_predictors <- c("WHC", "bld", "pH", "nitrogen")
  soil_fullnames <- c("Water holding cap.", "Bulk density", "pH", "Nitrogen")
  cc_predictors <- c("sum_GDD", "nd_10deg", "lastd_frost",  "w_bal")
  cc_fullnames <- c("Sum of growing degree days (Mar-Oct)","Number of days with T<10C (Oct-Feb)", "Last frost day (Jan-Jul)","Water balance (Jun-Jul)")
  predictors <- c(bc_predictors, soil_predictors, cc_predictors)
  predictors_fullnames <- c(bc_fullnames, soil_fullnames, cc_fullnames)
}



#-----------------------#
# Predictor correlogram #
#-----------------------#




# all predictors
mat_corr <- cor(predictors_data)
colnames(mat_corr) <- predictors_fullnames

corrplot(mat_corr, method="color",   
         type="upper",  
         addCoef.col = "black", # ajout du coefficient de correlation
         tl.col="black", tl.srt=45, #rotation des etiquettes de textes
         diag = FALSE)

# custom predictors
custom_predictors <- c("WHC","sum_GDD", "nd_10deg", "lastd_frost",  "w_bal")
mat_corr <- cor(predictors_data %>% dplyr::select(all_of(custom_predictors)))
colnames(mat_corr) <- predictors_fullnames[which(predictors %in% custom_predictors)]
rownames(mat_corr) <- predictors_fullnames[which(predictors %in% custom_predictors)]
corrplot(mat_corr, method="color",   
         type="upper",  
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         diag = F)

# default predictors
default_predictors <- c("bio6", "bio5", "bio4", "bio14", "bio13", "bio15", "pH")
mat_corr <- cor(predictors_data %>% dplyr::select(all_of(default_predictors)))
colnames(mat_corr) <- predictors_fullnames[which(predictors %in% default_predictors)]
rownames(mat_corr) <- predictors_fullnames[which(predictors %in% default_predictors)]
corrplot(mat_corr, method="color",   
         type="upper",  
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         diag = F)
