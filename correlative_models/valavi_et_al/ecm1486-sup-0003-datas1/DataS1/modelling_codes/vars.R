#####################################
# environmental covariates
awt_vars <- c("bc04",  "bc05",  "bc06",  "bc12",  "bc15",  "slope", "topo", "tri")
can_vars <- c("alt", "asp2", "ontprec", "ontslp", "onttemp", "ontveg", "watdist")
nsw_vars <- c("cti", "disturb", "mi", "rainann", "raindq", "rugged", "soildepth", "soilfert", "solrad", "tempann", "topo", "vegsys")
nz_vars <- c("age", "deficit", "hillshade", "mas", "mat", "r2pet", "slope", "sseas", "toxicats", "tseas", "vpd")
sa_vars <- c("sabio12", "sabio15", "sabio17", "sabio18", "sabio2", "sabio4", "sabio5", "sabio6")
swi_vars <- c("bcc", "calc", "ccc", "ddeg", "nutri", "pday", "precyy", "sfroyy", "slope", "sradyy", "swb", "topo")

vars <- list(awt=awt_vars, can=can_vars, nsw=nsw_vars, nz=nz_vars, sa=sa_vars, swi=swi_vars)

#############################################
# builing GAM formulas
categoricalvars <- c("ontveg", "vegsys", "toxicats", "age", "calc")

awt_fm <- paste("occ ~", paste(paste0("s(", vars[["awt"]], ")"), collapse = " + "))
can_fm <- paste("occ ~", paste(paste0("s(", vars[["can"]][-6], ")"), collapse = " + "))
can_fm <- paste(can_fm, " + ", vars[["can"]][6])
nsw_fm <- "occ ~ s(cti) + s(mi) + s(rainann) + s(raindq) + s(rugged) + s(soildepth) + s(solrad) + s(tempann) + s(topo) + soilfert + disturb + vegsys"
nz_fm <- "occ ~ s(deficit) + s(hillshade) + s(mas) + s(mat) + s(r2pet) + s(slope) + s(sseas) + s(tseas) + s(vpd) + age"
sa_fm <- paste("occ ~", paste(paste0("s(", vars[["sa"]], ")"), collapse = " + "))
swi_fm <- paste("occ ~", paste(paste0("s(", vars[["swi"]][-2], ")"), collapse = " + "))
swi_fm <- paste(swi_fm, " + ", vars[["swi"]][2])

myform <- list(AWT=awt_fm, CAN=can_fm, NSW=nsw_fm, NZ=nz_fm, SA=sa_fm, SWI=swi_fm)

#############################################
# building model scopes for GLM mdeol selection
awt_scope <- list("bc04" = ~1 + bc04 + poly(bc04, 2),
                  "bc05" = ~1 + bc05 + poly(bc05, 2),
                  "bc06" = ~1 + bc06 + poly(bc06, 2),
                  "bc12" = ~1 + bc12 + poly(bc12, 2), 
                  "bc15" = ~1 + bc15 + poly(bc15, 2), 
                  "slope" = ~1 + slope + poly(slope, 2),
                  "topo" = ~1 + topo + poly(topo, 2),
                  "tri" = ~1 + tri + poly(tri, 2))

can_scope <- list("alt" = ~1 + alt + poly(alt, 2),
                  "asp2" = ~1 + asp2 + poly(asp2, 2),
                  "ontprec" = ~1 + ontprec + poly(ontprec, 2),
                  "ontslp" = ~1 + ontslp + poly(ontslp, 2), 
                  "onttemp" = ~1 + onttemp + poly(onttemp, 2),
                  "watdist" = ~1 + watdist + poly(watdist, 2),
                  "ontveg" = ~1 + ontveg)

nsw_scope <- list("cti" = ~1 + cti + poly(cti, 2),
                  "disturb" = ~1 + disturb + poly(disturb, 2),
                  "mi" = ~1 + mi + poly(mi, 2),
                  "rainann" = ~1 + rainann + poly(rainann, 2), 
                  "raindq" = ~1 + raindq + poly(raindq, 2), 
                  "rugged" = ~1 + rugged + poly(rugged, 2),
                  "soildepth" = ~1 + soildepth + poly(soildepth, 2),
                  "soilfert" = ~1 + soilfert + poly(soilfert, 2),
                  "solrad" = ~1 + solrad + poly(solrad, 2),
                  "tempann" = ~1 + tempann + poly(tempann, 2),
                  "topo" = ~1 + topo + poly(topo, 2),
                  "vegsys" = ~1 + vegsys)

nz_scope <- list("age" = ~1 + age,
                 "deficit" = ~1 + deficit + poly(deficit, 2),
                 "hillshade" = ~1 + hillshade + poly(hillshade, 2),
                 "mas" = ~1 + mas + poly(mas, 2), 
                 "mat" = ~1 + mat + poly(mat, 2), 
                 "r2pet" = ~1 + r2pet + poly(r2pet, 2),
                 "slope" = ~1 + slope + poly(slope, 2),
                 "sseas" = ~1 + sseas + poly(sseas, 2),
                 "tseas" = ~1 + tseas + poly(tseas, 2),
                 "vpd" = ~1 + vpd + poly(vpd, 2),
                 "toxicats" = ~1 + toxicats)

sa_scope <- list("sabio12" = ~1 + sabio12 + poly(sabio12, 2),
                 "sabio15" = ~1 + sabio15 + poly(sabio15, 2),
                 "sabio17" = ~1 + sabio17 + poly(sabio17, 2), 
                 "sabio18" = ~1 + sabio18 + poly(sabio18, 2), 
                 "sabio2" = ~1 + sabio2 + poly(sabio2, 2),
                 "sabio4" = ~1 + sabio4 + poly(sabio4, 2),
                 "sabio5" = ~1 + sabio5 + poly(sabio5, 2),
                 "sabio6" = ~1 + sabio6 + poly(sabio6, 2))

swi_scope <- list("bcc" = ~1 + bcc + poly(bcc, 2),
                  "ccc" = ~1 + ccc + poly(ccc, 2),
                  "ddeg" = ~1 + ddeg + poly(ddeg, 2), 
                  "nutri" = ~1 + nutri + poly(nutri, 2), 
                  "pday" = ~1 + pday + poly(pday, 2),
                  "precyy" = ~1 + precyy + poly(precyy, 2),
                  "sfroyy" = ~1 + sfroyy + poly(sfroyy, 2),
                  "slope" = ~1 + slope + poly(slope, 2),
                  "sradyy" = ~1 + sradyy + poly(sradyy, 2),
                  "swb" = ~1 + swb + poly(swb, 2),
                  "topo" = ~1 + topo + poly(topo, 2),
                  "calc" = ~1 + calc)

myscope <- list(AWT=awt_scope, CAN=can_scope, NSW=nsw_scope, NZ=nz_scope, SA=sa_scope, SWI=swi_scope)

#####################################