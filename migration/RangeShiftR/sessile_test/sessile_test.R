###############
# RANGESHIFTR #
###############

library(RangeShiftR)

# Working directory 
dirpath = "C:/Users/vandermeersch/Documents/CEFE/phd/migration/RangeShiftR/sessile_test"

dir.create(file.path(dirpath,"Inputs"), showWarnings = TRUE)
dir.create(file.path(dirpath,"Outputs"), showWarnings = TRUE)
dir.create(file.path(dirpath,"Output_Maps"), showWarnings = TRUE)


# Initialisation

## Landscape parameters
suit <- ImportedLandscape(LandscapeFile = "hab_suit_raster_ds.asc",
                          Resolution = 1000, #meters
                          HabPercent = TRUE,
                          K_or_DensDep = 100, #number of indivuals per ha
                          SpDistFile = "sp_dist.asc",
                          SpDistResolution = 1000 #meters
)


## Demography parameters

# parameters for Fagus (Zani et al.)
GERMp <- 0.71 # seed germination rate
mu <- 0.8 # loss of seed, not specific
FECmax <- 29 # maximum fecundity : nb of seeds/100 years ?! not sure to understand

# other parameters
age_mat <- 20 # age to be mature
age_s1 <- 40 # young adult => adult
R1 <- 0.5 # ratio for seed production, young adult (~ LAI/LAImax)
R2 <- 0.9 # ratio for seed prod, adult

# transition matrix
trans_mat <- matrix(c(0.0,   0.0,              0.0,   R1*FECmax,  R2*FECmax,
                      1.0,   (1-GERMp)*(1-mu), 0.0,   0.0,        0.0,
                      0.0,   0.01*GERMp,       0.0,   0.0,        0.0,
                      0.0,   0.0,              1.0,   0.0,        0.0,
                      0.0,   0.0,              0.0,   1.0,        0.0),
                    ncol = 5, byrow = TRUE)

st_struc <- StageStructure(
  Stages = 5, # seeds, soil seed bank, saplings, mature young adult, mature adult
  MinAge = c(0, 0, 1, age_mat, age_s1),
  MaxAge = 200,
  TransMatrix = trans_mat,
  RepSeasons = 1,  # only one seed productio event per year
  RepInterval = 2, # masting effect ? (number of seasons missed)
  PRep = 1,
  PostDestructn = FALSE, # individuals die if landscape not suitable
  SurvSched = 1 # each season: seed production, dispersal, then development and ageing
)



demo <- Demography(StageStruct = st_struc)




## Dispersal parameters

# stage   short_dist   long_dist   prob_of_short_dist
dist_mat <- matrix(c(0,  25,   200,   0.99, 
                     1,  25,   200,   0.99, # not used
                     2,  25,   200,   0.99, # not used
                     3,  25,   200,   0.99, # not used
                     4,  25,   200,   0.99), # not used
                   ncol = 4, byrow = TRUE)

disp_kernel <- DispersalKernel(StageDep=TRUE, # stage-dependent dispersal kernel: only seed are allowed to disperse
                               DoubleKernel=TRUE,
                               Distances = dist_mat
                               )

# stage   probability of emigration
emig_mat <- matrix(c(0, 1.0, # only seed can disperse
                     1, 0.0,
                     2, 0.0,
                     3, 0.0,
                     4, 0.0),
                   ncol = 2, byrow = TRUE)


disp <-  Dispersal(Emigration = Emigration(EmigProb = emig_mat, StageDep = TRUE, UseFullKern = TRUE), 
                   Transfer = disp_kernel, 
                   Settlement = Settlement(Settle = 0) )

# N.B. : "kernel applied to the entire population without re-sampling (the 'use full kernel'
# option): individuals which draw a short movement distance do not leave the natal cell/patch
# and implicitly become sedentary, and therefore the kernel itself defines the proportion of
# individuals which emigrate. When this option is selected, the emigration probability for those
# stages/sexes which disperse should be set to 1.0; otherwise, only a proportion of such
# individuals would use the kernel to determine whether or not they emigrate" (from user manual)




## Initial parameters
init <- Initialise(InitType = 1, # = initialisation from a loaded species distribution map
                   SpType = 0,   # = all suitable cells within all distribution presence cells
                   InitDens = 0, # = init at carrying capacity
                   PropStages = c(0, 0, 0, 1, 0), # only young mature
                   InitAge = 2 # initial age distribution with stage (default: quasi-equilibrium distribution)
                   ) 

## Simulation parameters

sim_0 <- Simulation(Simulation = 0, 
                    Replicates = 1, 
                    Years = 10,
                    
                    Absorbing = TRUE, # seed which hits a non-valid cell/landscape boundary during the dispersal act is eliminated
                    
                    OutIntPop = 10,
                    OutIntRange = 10)

s <- RSsim(land = suit, demog = demo, dispersal = disp, simul = sim_0, init = init)

# Run the simulation !
RunRS(s, paste0(dirpath, "/"))





pop_df <- readPop(s, paste0(dirpath, "/"))
ext <- as.vector(extent(hab_suit_raster_ds))
res <- 1000
pop_wide_rep0 <- reshape(subset(pop_df,Rep==0)[,c('Year','x','y','NInd')], timevar='Year', v.names=c('NInd'), idvar=c('x','y'), direction='wide')
stack_years_rep0 <- rasterFromXYZ(pop_wide_rep0)
