setwd("C:/Users/vandermeersch/Documents/CEFE/phd/sampling/EBC_Chauvier")

require(ggplot2)
require(raster)
require(cluster)

source("wsl.ebc.R")
source("wsl.obs.filter.R")

load("exrst.RData")
load("xy_ppm.RData")
names(xy.ppm)[1] = "sp.id"

wsl.ebc(obs = xy.ppm,
        ras = rst[[1:5]],
        pportional = TRUE,
        plog = TRUE,
        nclust = 50,
        sp.specific = FALSE,
        sp.cor = NULL,
        filter = FALSE,
        keep.bias = TRUE,
        path = getwd())


# files = list.files(getwd())
# target.files = files[grep("_obs_corrected_",files)]
# correct.obs = lapply(target.files, function(x) obs=read.table(paste0(getwd(),"/",x)))
# correct.obs = do.call("rbind",correct.obs)

correct.obs=read.table(paste0(getwd(),"/","2022-01-11_obs_corrected_sp1.txt"), header=T)

correct.obs.v50 = correct.obs[!duplicated(correct.obs), ] # 343 obs

nrow(xy.ppm[xy.ppm$sp.id=="sp1",]) # 469 initial obs

p <- ggplot() + 
  geom_point(data = correct.obs.v50, aes(x, y), color = 'blue', alpha=0.5, size =1) +
  geom_point(data = correct.obs.v100, aes(x, y), color = 'green', alpha=0.5, size =1)+
  theme(legend.position = 'NONE') +
  theme_void()
