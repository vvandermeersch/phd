
start_time <- Sys.time()
toarad <- rast(file.path(out_folder, "processed_toarad_daily_15minRes_Europe_24000_0kyr",
                    paste0("processed_toarad_daily_15minRes_Europe_", time_slice, ".nc")))
toa <- subset(toarad, 1:365) # WARNING

dayl <- rast(file.path(out_folder, "processed_dayl_daily_15minRes_Europe_24000_0kyr",
                         paste0("processed_dayl_daily_15minRes_Europe_", time_slice, ".nc")))
dayl <- subset(dayl, 1:365) # WARNING

delta <- rast(file.path(out_folder, "processed_delta_daily_15minRes_Europe_24000_0kyr",
                       paste0("processed_delta_daily_15minRes_Europe_", time_slice, ".nc")))
delta <- subset(dayl, 1:365) # WARNING

gwgen_data_yr <- left_join(gwgen_data[gwgen_data$year == yr,c("id", "doy", "tmin", "tmax", "mean_cloud", "prcp", "wind")], latlon_data)
tmin <- rast(lapply(unique(gwgen_data_yr$doy), function(d)rast(gwgen_data_yr[gwgen_data_yr$doy == d,c("lon", "lat", "tmin")])))
tmax <- rast(lapply(unique(gwgen_data_yr$doy), function(d)rast(gwgen_data_yr[gwgen_data_yr$doy == d,c("lon", "lat", "tmax")])))
cloud <- rast(lapply(unique(gwgen_data_yr$doy), function(d)rast(gwgen_data_yr[gwgen_data_yr$doy == d,c("lon", "lat", "mean_cloud")])))
pre <- rast(lapply(unique(gwgen_data_yr$doy), function(d)rast(gwgen_data_yr[gwgen_data_yr$doy == d,c("lon", "lat", "prcp")])))
wind <- rast(lapply(unique(gwgen_data_yr$doy), function(d)rast(gwgen_data_yr[gwgen_data_yr$doy == d,c("lon", "lat", "wind")])))
lat <- rast(gwgen_data_yr[gwgen_data_yr$doy == 1,c("lon", "lat", "lat")])

alt <- rast(file.path(out_folder, "altitude_ICE6GC_15minRes_Europe_24000_0kyr",
                      paste0("altitude_ICE6GC_Europe_", time_slice, ".nc")))
alt <- resample(alt, tmin) # just to have exactly the same extent

albedo <- rast(file.path(out_folder, "albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_24000_0kyr",
                      paste0("albedos_old_sims_1yrAvg_monthly_15minRes_noBias_Europe_", time_slice, ".nc")))
albeldo <- resample(albedo, tmin) # just to have exactly the same extent
alb <- subset(albeldo, 1:12) # WARNING
if(max(unique(gwgen_data_yr$doy)) == 366){
  ndays <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  alb <- rast(lapply(1:12, function(m) return(rep(subset(alb, m), ndays[m]))))
}else{
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  alb <- rast(lapply(1:12, function(m) return(rep(subset(alb, m), ndays[m]))))
}



# compute the relative atmospheric pressure
z0 <- 1/8000
ratm <- exp(-alt * z0)

# compute the "precipitation equitability index", used thereafter to compute global radiation
cat("Computing precipitation equitability index...\n")
input_data_yr <- input_data[input_data$year == yr,]
input_data_yr$temp <- (input_data_yr$min.temperature + input_data_yr$max.temperature)/2
temp_mn <- rast(lapply(1:12, function(m) rast(input_data_yr[input_data_yr$month == m, c("lon", "lat", "temp")])))
pre_mn <- rast(lapply(1:12, function(m) rast(input_data_yr[input_data_yr$month == m, c("lon", "lat", "precipitation")])))
coldest_index <- app(temp_mn, which.min)
warmest_index <- app(temp_mn, which.max)
t_cm <- extract_values_from_index(temp_mn, coldest_index)
t_wm <- extract_values_from_index(temp_mn, warmest_index)
p_cm <- extract_values_from_index(pre_mn, coldest_index)
p_wm <- extract_values_from_index(pre_mn, warmest_index)
peqin <- ifel(p_wm + p_cm > 0, max(2 * (p_wm - p_cm) / (p_wm + p_cm), 0), 0)



# calculate daily airmass
# parameters
m0 <- 1
m80 <- 5.6
m90 <- 39.7
pir <- pi/180
mindayl <- 2 * 10e-30
cos80 <- cos(80 * pir)
c00 <- c(0.008307, 0, 0)
c00[2] <- (m0 - m80) * (c00[1] + 1) * (c00[1] + cos80) / (cos80 - 1)
c00[3] <- m0 - c00[2] / (c00[1] + 1)
c80 <- c(0.037160, 0, 0)
c80[2] <- (m90 - m80) * c80[1] * (c80[1] + cos80) / cos80
c80[3] <- m90 - c80[2] / c80[1]
w  = 15

rid <- dayl 
rid[!is.na(rid)] <- 1
mbar <- mc <- ml <- mo <- rid


t1 <- ifel(dayl > mindayl, 0.5*dayl, dayl)
Zn <- ifel(abs(lat + delta) >= 90, acos(sin(pir * lat) * sin(pir * delta) - cos(pir * lat) * cos(pir * delta)) / pir, 90)
Z0 <- ifel(abs(lat - delta) >= 90, 90, lat - delta)
b <- cos(pir * lat) * cos(pir * delta)
t80 <- 1 / w * acos((cos80 - sin(pir * lat)  * sin(pir * delta)) / (cos(pir * lat) * cos(pir * delta))) / pir

mbar <- ifel(dayl == 0, m90, 
             ifel(t1 == 0, m90, 
                  ifel(abs(Zn) <= 80, (1/t1) * Fint(t1,a = sin(pir * lat) * sin(pir * delta) + c00[1], b, c00),
                       ifel(abs(Z0) >= 80,  (1/t1) * Fint(t1,a = c80[1] + sin(pir * lat) * sin(pir * delta), b,c80),
                            (1/t1) * (Fint(t80,a = c00[1] + sin(pir * lat) * sin(pir * delta),b,c00) + 
                                      Fint(t1,a = c80[1] + sin(pir * lat) * sin(pir * delta),b,c80) - 
                                        Fint(t80,a = c80[1] + sin(pir * lat) * sin(pir * delta),b,c80))))))

Z <- Z0
cosZ <- cos(Z * pir)
mo <- ifel(dayl == 0, m90, 
           ifel(Z <= 80, c00[2] / (c00[1] + cosZ) + c00[3],
                c80[2] / (c80[1] + cosZ) + c80[3]))

Z <- (Z0 + Zn) / 2
cosz = (cos(Z0 * pir) + cos(Zn * pir)) / 2
mc <- ifel(dayl == 0, m90, 
           ifel(Z <= 80, c00[2] / (c00[1] + cosZ) + c00[3],
                c80[2] / (c80[1] + cosZ) + c80[3]))

Z <- (Z0 + 3 * Zn) / 4
cosz <- (cos(Z0 * pir) + 3 * cos(Zn * pir)) / 4
ml <- ifel(dayl == 0, m90, 
           ifel(Z <= 80, c00[2] / (c00[1] + cosZ) + c00[3],
                c80[2] / (c80[1] + cosZ) + c80[3]))


# correct calculated air mass for elevation
mbar <- ratm * mbar
mo <- ratm * mo
mc <- ratm * mc
ml <- ratm * ml




# compute long-wave radiation (needed to compute PET in Kaplan method),
# saturation vapour pressure, slope of vapour pressure curve, latent heat and psychrometric constant
# parameters
sb <- 5.6704e-8
e <- 0.98 
al <- 0.045 
a <- 10.77
b <- 2.34
c <- 18.44
cs <- 1.5 
Tfreeze <- 273.15 

# set values
temp <- (tmin + tmax)/2 # approximation

# clear-sky factor
sunf <- 1 - cloud

Tk <- temp + Tfreeze

gamma <- 65.05 + temp * 0.064 # psychrometric constant

lvap <- 0.001 * 1.91846e6 * (Tk / (Tk - 33.91))**2  # latent heat

# these coeff are for celcius temperature
bl <- c(4.44017302e-1, 2.86064092e-2, 7.94683137e-4, 1.21211669e-5, 1.03354611e-7,
        4.04125005e-10, -7.88037859e-13, -1.14596802e-14, 3.81294516e-17)
bi <- c(5.03277922e-1, 3.77289173e-2, 1.26801703e-3, 2.49468427e-5, 3.13703411e-7,
        2.57180651e-9, 1.32268878e-11, 3.94116744e-14, 4.98070196e-17)

# slope of vapour pressure curve (FUNCTION)
ss <- 100*ifel(Tk <= Tfreeze, sum(bi[1]*(Tk - Tfreeze)**0,  tapp(rast(lapply(2:9, function(i) bi[i]*(Tk - Tfreeze)**i)), 1:365, sum)),
             sum(bl[1]*(Tk - Tfreeze)**0, tapp(rast(lapply(2:9, function(i) bl[i]*(Tk - Tfreeze)**i)), 1:365, sum)))

f <- 0.2 + 0.8 * sunf  

Ts <- Tk # approximation: mean daily surface temperature = air temperature

Ql_up <- e * sb * Ts**4

# these coeff are for celcius temperature
al <- c(6.11213476, 4.44007856e-1, 1.43064234e-2, 2.64461437e-4, 3.05903558e-6,
        1.96237241e-8, 8.92344772e-11, -3.73208410e-13, 2.09339997e-16)
ai <- c(6.11123516, 5.03109514e-1, 1.88369801e-2, 4.20547422e-4,
        6.14396778e-6, 6.02780717e-8, 3.87940929e-10, 1.49436277e-12, 2.62655803e-15)

# saturated vapor pressure (FUNCTION)
es <- ifel(Tk <= Tfreeze, sum(ai[1]*(Tk - Tfreeze)**0,  tapp(rast(lapply(2:9, function(i) ai[i]*(Tk - Tfreeze)**i)), 1:365, sum)),
               sum(al[1]*(Tk - Tfreeze)**0, tapp(rast(lapply(2:9, function(i) al[i]*(Tk - Tfreeze)**i)), 1:365, sum)))  

TdewK <- 34.07 + 4157 / log(2.1718e8 / es)  

D <- TdewK - Tk

Ql_dn <- sb * (Tk + a*cloud**2 + b*cloud + c + 0.84 * (D + 4.01))**4 

al <- 0.045 
Ql <- Ql_up - (1 - al) * Ql_dn   

lw_rad <- 0.001 * 3600 * dayl * Ql  
tdew <- TdewK - Tfreeze




# global radiation
k <- 1
albedoPM <- 0.23*rid # hypothetical reference crop with an albedo of 0.23
pet <- pet0 <- 0*rid

# wind speed
z_meas <- 10
u2 <- wind * 4.87/log(67.8 * z_meas - 5.42)

# saturation vapour pressure
es_Tmax <- 0.6108 * exp(17.27 * tmax/(tmax + 237.3))
es_Tmin <- 0.6108 * exp(17.27 * tmin/(tmin + 237.3))
es <- (es_Tmax + es_Tmin)/2

# Tdew <- 34.07 + 4157 / log(2.1718e8 / es) - 273.15
ea <- 0.6108 * exp((17.27 * tdew)/(tdew+237.3))

# slope of vapour pressure curve (delta)
# ss <- 4098 * (0.6108 * exp((17.27 * gwgen_data[i,"temp"])/(gwgen_data[i,"temp"] + 237.3)))/((gwgen_data[i,"temp"] + 237.3)^2)
ss <- ss/1000 # convert to kPa/C

# psychrometric constant
#lambda <- 2.45
#gamma <- 0.00163 * P/lambda
gamma <- gamma/1000 #convert to kPa/C

# effect of cloudiness (relative shortwave radiation)
# rs0 <- (0.75 + (2 * 10^-5) * elev) * toa_data_temp[i,"toa"]/1000 # as in FAO
# rs_rs0 <- 1-0.29*(gwgen_data_temp[i,"mean_cloud"] + (gwgen_data_temp[i,"mean_cloud"])^2) # Antoine et al, 1996
rs_rs0 <- 1 - cloud

# compute global radiation (FUNCTION)
# parameters
kp <- 0.5
kag <- 3.3
kan <- 2.32
kn <- 0.686
# tropics indicator (tropical = 1, else 0)
x <- rid*ifel(t_cm < 10, 0,
              ifel(t_cm>20, 1, sin(pi / 2 * (t_cm / 10 - 1))))

# direct insolation atmospheric turbidity factor
tau <- exp(-0.115 * ratm * ((2.15 - 0.713 * x + exp(-6.74 / (pre + 1))) * exp(0.0971 * pet) - 0.650 * (1 - x) * peqin))

# atmospheric transmittance function
fm <- 0.01452 * (mbar + ml) * exp(1.403 * tau) - 0.1528 * mo + mc + 0.48700 * (mc - ml) + 0.2323 

# direct downwelling shortwave radiation
direct <- sunf * tau**kp * toa * tau**fm

# diffuse insolation atmospheric turbidity factor
zeta0 <- 0.503 * exp(-1.20 * ratm * exp(-0.633 / (pre + 1) - 0.226 * pet))

# diffuse downwelling shortwave radiation
diffuse <- zeta0 * kag**alb * kan**(1 - sunf) * (1 - kn * (1 - sunf)) * (tau**kp * toa - direct)

# global radiation
glo <- (diffuse+direct)


r_s <- glo/1000 # convert to MJ/m2

# estimated net outgoing longwave radiation
r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
  ((tmax + 273.2)^4 + (tmin + 273.2)^4)/2 * 
  (1.35 * rs_rs0 - 0.35) 

# net radiation
r_ng <- (1 - alb) * r_s  - r_nl

# compute PET (Penman-Monteith equation) 
pet <- (0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (temp + 273.15))/
             (ss + gamma * (1 + 0.34 * u2))
pet[pet<0] <- 0

# J. Kaplan: "weak dependence between global radiation and PET, need to equilibrate them"
k <- 0
while(global(any(abs(pet - pet0) > 0.01), max, na.rm = TRUE)==1 & k < 100){
  
  pet0 <- pet
  
  # compute global radiation (FUNCTION)
  # parameters
  kp <- 0.5
  kag <- 3.3
  kan <- 2.32
  kn <- 0.686
  # tropics indicator (tropical = 1, else 0)
  x <- rid*ifel(t_cm < 10, 0,
                ifel(t_cm>20, 1, sin(pi / 2 * (t_cm / 10 - 1))))
  
  # direct insolation atmospheric turbidity factor
  tau <- exp(-0.115 * ratm * ((2.15 - 0.713 * x + exp(-6.74 / (pre + 1))) * exp(0.0971 * pet) - 0.650 * (1 - x) * peqin))
  
  # atmospheric transmittance function
  fm <- 0.01452 * (mbar + ml) * exp(1.403 * tau) - 0.1528 * mo + mc + 0.48700 * (mc - ml) + 0.2323 
  
  # direct downwelling shortwave radiation
  direct <- sunf * tau**kp * toa * tau**fm
  
  # diffuse insolation atmospheric turbidity factor
  zeta0 <- 0.503 * exp(-1.20 * ratm * exp(-0.633 / (pre + 1) - 0.226 * pet))
  
  # diffuse downwelling shortwave radiation
  diffuse <- zeta0 * kag**alb * kan**(1 - sunf) * (1 - kn * (1 - sunf)) * (tau**kp * toa - direct)
  
  # global radiation
  glo <- (diffuse+direct)
  
  
  r_s <- glo/1000 # convert to MJ/m2
  
  # estimated net outgoing longwave radiation
  r_nl <- 4.903e-09 * (0.34 - 0.14 * sqrt(ea)) * 
    ((tmax + 273.2)^4 + (tmin + 273.2)^4)/2 * 
    (1.35 * rs_rs0 - 0.35) 
  
  # net radiation
  r_ng <- (1 - alb) * r_s  - r_nl
  
  # compute PET (Penman-Monteith equation) 
  pet <- (0.408 * ss * r_ng + gamma * 900 * u2 * (es - ea) / (temp + 273.15))/
    (ss + gamma * (1 + 0.34 * u2))
  pet[pet<0] <- 0
  
  k <- k+1
  
}
end_time <- Sys.time()

glo <- as.data.frame(r_s, xy = TRUE)
glo[is.na(glo)] <- 0
pet <- as.data.frame(pet, xy = T)
pet[is.na(pet)] <- 0

extract_values_from_index <- function(rvalues, rindex){
  r <- rindex
  r[] <- NA
  for(i in 1:dim(rvalues)[3]){
    r_i <- rindex
    r_i[r_i != i] <- NA
    rvalues_i <- mask(subset(rvalues, i), r_i)
    rvalues_i[is.na(rvalues_i)] <- NA
    r <- sum(r, rvalues_i, na.rm = T)
  }
  return(r)
}



Fint <- function(t1,a,b,c){
  
  pir <- pi / 180
  w <- 15
  rw <- pir * w
  wpi <- 180 / (pi * w)
  wt1 <- rw * t1
  
  Fair <- ifel(a > b, wpi * c[2] / sqrt(a**2 - b**2) * acos((b + a * cos(wt1)) / (a + b * cos(wt1))) + c[3] * t1,
               ifel(a < b, F <- wpi * c[2] / sqrt(b**2 - a**2) * log((sqrt((b + a) * (1 + cos(wt1))) + sqrt((b - a) * (1 - cos(wt1)))) / (sqrt((b + a) * (1 + cos(wt1))) - sqrt((b - a) * (1 - cos(wt1))))) + c[3] * t1, 
                    wpi * c[2] / a * tan(wt1 / 2) + c[3] * t1))
  
  return(Fair)
  
}



