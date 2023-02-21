# exponential kernel #
Ks = function(parms)
{
  asd = parms[1]
  ald = parms[2]
  pldd = parms[3]
  ks = function(x)
    (1-pldd)*(1/(asd))* exp(-x/asd)+pldd*(1/(ald))* exp(-x/ald)
    
  return(ks)
}

intKs = function (parms, minDist) 
  integrate(Ks(parms),  lower = minDist, upper = Inf)$value

# default (F. sylvatica)
a1 = 25/2
a2 = 200/2
pldd = 0.01

LDD_lwrCL_25<-floor(294.99/200)   #divide by 25 to match grid resolution (25m) and round down

dist <- seq(0, 100, 25)
prob <- 0

dist.prob<-cbind(dist,prob)


for(k in 1:length(dist.prob[,1])){
  dist.prob[k,2]<-int2(c(a1,a2,pldd), as.numeric(dist.prob[k,1])) 
}

View(dist.prob)





# 2Dt kernel #
Ks = function(parms)
{
  asd = parms[1]
  ald = parms[2]
  b = parms[3]
  pldd = parms[4]
  ks = function(x)
    (1-pldd)*2*asd*x/b*(1 + x^2/b)^(-asd-1)+pldd*2*ald*x/b*(1 + x^2/b)^(-ald-1)
  
  return(ks)
}

intKs = function (parms, minDist) 
  integrate(Ks(parms),  lower = minDist, upper = Inf)$value

# LPJ_GM 
b = 2
a1 = 2/pi * gamma(b-1)/gamma(b-(3/2)) * 25
a2 = 2/pi * gamma(b-1)/gamma(b-(3/2)) * 200
pldd = 0.01


for(k in 1:length(dist.prob[,1])){
  dist.prob[k,2]<-int2(c(a1,a2,b,pldd), as.numeric(dist.prob[k,1])) 
}

View(dist.prob)
