library(nlme)

# Function to estimate local temporal rate of change

# this local rate is then needed to calculate displacement vectores
# it is quantified as the slope coefficient of a GLS regression with a first order autoregressive term

# see Ordonnez et al. (2016) and Burke et al. (2019)

local_rate_of_change <- function(focal_stack){
  
  # create the output raster
  rout <- subset(focal_stack, 1)
  rout[] <- NA
  
  # transform into data.frame
  focal_df <- as.data.frame(focal_stack, xy = T)
  
  # loop on focal cells
  for(i in 1:ncell(focal_stack)){
    
    focal_cell_df <- data.frame(scale(t(focal_df[i,3:ncol(focal_df)])))
    
    if(!any(is.na(focal_cell_df) == TRUE)){
      
      names(focal_cell_df) <- "var"
      focal_cell_df$t <- 1:500
      
      #GLS regression
      m <- gls(var ~ t,
               data=focal_cell_df,
               correlation=corARMA(p=1, q=0, form=~t))
      local_slope <- as.numeric(m$coefficients["t"])
      
      rout[i] <- local_slope
      
    }
    
  }
  
  gc()
  return(rout)
  
}
