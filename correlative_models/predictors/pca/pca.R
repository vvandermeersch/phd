#
# Compute PCA axes on default predictors
# 

library(dplyr)
library(ggfortify)
library(ggcorrplot)
library(ggthemes)

wd <- "C:/Users/vandermeersch/Documents/CEFE/thesis/correlative_models/predictors"


# Load predictors
load(file.path(wd, "predictors_data.Rdata"))
bc_predictors <- c("bio1", "bio5", "bio6", "bio4", "bio12", "bio14", "bio13", "bio15")
bc_fullnames <- c("Annual mean temp.", "Max. temp. warmest month", "Min. temp. coldest month", "Temp. seasonality", 
                  "Annual prec.", "Prec. of driest month", "Prec. of wettest month", "Prec. seasonality")
soil_predictors <- c("WHC", "bld", "pH", "nitrogen", "carbon")
soil_fullnames <- c("Water holding cap.", "Bulk density", "pH", "Nitrogen", "Carbon")
cc_predictors <- c("sum_GDD", "sum_leaf_GDD", "sum_fruit_GDD", "nd_sup10deg",
                   "nd_10deg", "nd_5deg", 
                   "nd_neg5deg", "nd_neg0deg", 
                   "lastd_frost",  "w_bal")
cc_fullnames <- c("GDD (Mar-Oct)", "GDD (Mar-May)", "GDD (Jun-Sep)", "Num. of days with Tmean>10C (Mar-Oct)",
                  "Num. of days with Tmean<10C (Nov-Feb)", "Num. of days with Tmean<5C (Nov-Feb)", 
                  "Num. of days with Tmin<-5C (April-May)", "Num. of days with Tmin<0C (April-May)",
                  "Last frost day (Jan-Jul)","Water balance (Jun-Jul)")
predictors <- c(bc_predictors, soil_predictors, cc_predictors)
predictors_fullnames <- c(bc_fullnames, soil_fullnames, cc_fullnames)
predictors_names <- data.frame(predictors, predictors_fullnames)


# Select predictors
bc_covars <- c("bio1", "bio5", "bio6", "bio4", "bio12","bio13", "bio14", "bio15")
soil_covars <- c("pH", "bld", "nitrogen", "carbon")
predictors <- c(bc_covars, soil_covars)
default_predictors <- predictors_data %>%
  dplyr::select(all_of(predictors))

###==================================
# First test: PCA on climate + soil #
###==================================

# PCA using prcomp function (normalize as PCA depends on distance measure)
pred_pca <- prcomp(default_predictors, scale = TRUE,
                 center = TRUE, retx = T)
summary(pred_pca)

# Plotting 
autoplot(pred_pca, scale = 0, loadings = TRUE, loadings.label = TRUE)


# Plot cumulative variance explained for each principal component
pred_pca.var <- pred_pca$sdev ^ 2
propve <- pred_pca.var / sum(pred_pca.var)
plot(cumsum(propve),
     xlab = "Principal component",
     ylab = "Cumulative proportion of variance explained",
     ylim = c(0, 1), type = "b")

cumvar_plot <- ggplot(data = data.frame(cumvar = cumsum(propve)), aes(y=cumvar, x=(1:length(cumvar)))) +
  geom_line(color = "darkgrey") +
  geom_point() +
  geom_hline(yintercept = 0.9, linetype = "dashed") +
  theme_hc() +
  labs(y = "Cumulative proportion of variance explained", x = "Principal component") +
  scale_x_discrete(limits=as.character(1:12))


# Find principal components (which cover 90 % variance of dimension)
naxis <- which(cumsum(propve) >= 0.9)[1]

# Correlation tests between PCA axes and original predictors
mat_corr <- cor(x = default_predictors, y = pred_pca$x[,1:naxis]) 
colnames(mat_corr) <- paste("Axis", 1:naxis)
rownames(mat_corr) <- t(left_join(data.frame(predictors), predictors_names) %>% dplyr::select(predictors_fullnames))

df_corr <- reshape2::melt(mat_corr)
df_corr$Var2 <- as.factor(df_corr$Var2)

pca_cor_plot <- ggplot(df_corr, aes(Var1, factor(Var2,levels = c("Axis 5","Axis 4","Axis 3","Axis 2", "Axis 1")), fill=value)) +
  geom_tile(height=0.8, width=0.8) +
  scale_fill_gradient2(low="#DB5A42", mid="white", high="#4F9D69") +
  theme_minimal() +
  coord_equal() +
  labs(x="",y="",fill=NULL) +
  theme(axis.text.x=element_text(size=11, angle=45, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=11, margin=margin(0,-3,0,0)),
        panel.grid.major=element_blank()) +
  guides(fill = guide_colourbar(barheight = unit(2 , "in" ),
                                barwidth = 0.5,
                                ticks.colour = NA,
                                frame.colour = "grey",
                                frame.linewidth = 1))

plot_grid(pca_cor_plot, cumvar_plot, ncol = 2, rel_widths = c(0.7, 0.3))


###===========================
# Second test: separate PCAs #
###===========================

climate_predictors <- predictors_data %>%
  dplyr::select(all_of(bc_predictors))

# PCA
clim_pca <- prcomp(climate_predictors, scale = TRUE,
                   center = TRUE, retx = T)
summary(clim_pca)

# Plot cumulative variance explained for each principal component
clim_pca.var <- clim_pca$sdev ^ 2
propve <- clim_pca.var / sum(clim_pca.var)

cumvar_plot <- ggplot(data = data.frame(cumvar = cumsum(propve)), aes(y=cumvar, x=(1:length(cumvar)))) +
  geom_line(color = "darkgrey") +
  geom_point() +
  geom_hline(yintercept = 0.9, linetype = "dashed") +
  theme_hc() +
  labs(y = "Cumulative proportion of variance explained", x = "Principal component") +
  scale_x_discrete(limits=as.character(1:8))


# Find principal components (which cover 90 % variance of dimension)
naxis <- which(cumsum(propve) >= 0.9)[1]

# Correlation tests between PCA axes and original predictors
mat_corr <- cor(x = climate_predictors, y = clim_pca$x[,1:naxis]) 
colnames(mat_corr) <- paste("Axis", 1:naxis)
rownames(mat_corr) <- t(left_join(data.frame(predictors = names(climate_predictors)), predictors_names) %>% dplyr::select(predictors_fullnames))

df_corr <- reshape2::melt(mat_corr)
df_corr$Var2 <- as.factor(df_corr$Var2)

pca_cor_plot <- ggplot(df_corr, aes(Var1, factor(Var2,levels = c("Axis 5","Axis 4","Axis 3","Axis 2", "Axis 1")), fill=value)) +
  geom_tile(height=0.8, width=0.8) +
  scale_fill_gradient2(low="#DB5A42", mid="white", high="#4F9D69") +
  theme_minimal() +
  coord_equal() +
  labs(x="",y="",fill=NULL) +
  theme(axis.text.x=element_text(size=11, angle=45, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=11, margin=margin(0,-3,0,0)),
        panel.grid.major=element_blank()) +
  guides(fill = guide_colourbar(barheight = unit(2 , "in" ),
                                barwidth = 0.5,
                                ticks.colour = NA,
                                frame.colour = "grey",
                                frame.linewidth = 1))

plot_grid(pca_cor_plot, cumvar_plot, ncol = 2, rel_widths = c(0.7, 0.3))

