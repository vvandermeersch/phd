options(stringsAsFactors = FALSE)

# loading the library
suppressPackageStartupMessages(library(disdat))
suppressPackageStartupMessages(library(biomod2))
suppressPackageStartupMessages(library(forcats)) # for handling factor variables

# set output directory for prediction csv files:
outdir <- "models_output/biomod"
if(!file.exists(outdir)){
  dir.create(file.path(outdir))
  print("The directory is created")
}

# provide names for regions to be modeled - here we model all 6:
regions <- c("AWT", "CAN", "NSW", "NZ", "SA", "SWI")
# specify names of all categorical variables across all regions:
categoricalvars <- c("ontveg", "vegsys", "toxicats", "age", "calc")
# the list of uncorrelated variables
covars <- list(
  AWT = c("bc04",  "bc05",  "bc06",  "bc12",  "bc15",  "slope", "topo", "tri"),
  CAN = c("alt", "asp2", "ontprec", "ontslp", "onttemp", "ontveg", "watdist"), 
  NSW = c("cti", "disturb", "mi", "rainann", "raindq", "rugged", "soildepth", "soilfert", "solrad", "tempann", "topo", "vegsys"), 
  NZ = c("age", "deficit", "hillshade", "mas", "mat", "r2pet", "slope", "sseas", "toxicats", "tseas", "vpd"), 
  SA = c("sabio12", "sabio15", "sabio17", "sabio18", "sabio2", "sabio4", "sabio5", "sabio6"), 
  SWI = c("bcc", "calc", "ccc", "ddeg", "nutri", "pday", "precyy", "sfroyy", "slope", "sradyy", "swb", "topo")
)

n <- 0
for(r in regions){
  # reading presence-only and background species data for this region, one file per region:
  presences <- disPo(r)
  background <- read.csv(paste0("background_50k/", r, ".csv"))
  
  # extract names for all species
  species <- unique(presences$spid)
  
  # now for each species, prepare data for modeling and evaluation 
  for(s in species){
    n <- n + 1
    
    # subset presence records of species for this species
    sp_presence <- presences[presences$spid == s, ]
    # add background data
    pr_bg <- rbind(sp_presence, background)
    
    # if species file exist go to the next species
    if(file.exists(paste0(outdir, "/", s, "_biomod.csv"))) next
    if(sum(pr_bg$occ) < 5) next
    
    # find the evaluation file – for some regions this means identifying the taxonomic group
    if(r %in% c("AWT", "NSW")) {
      grp <- sp_presence[, "group"][1]
      evaluation <- disEnv(r, grp)
    } else {
      evaluation <- disEnv(r)
    }
    
    # convert categorical vars to factor in both training and testing data. We use the package forcats to ensure that the levels of the factor in the evaluation data match those in the training data, regardless of whether all levels are present in the evaluation data. 
    for(i in 1:ncol(pr_bg)){
      if(colnames(pr_bg)[i] %in% categoricalvars){
        fac_col <- colnames(pr_bg)[i]
        pr_bg[ ,fac_col] <- as.factor(pr_bg[ ,fac_col])
        evaluation[ ,fac_col] <- as.factor(evaluation[ ,fac_col])
        evaluation[ ,fac_col] <- forcats::fct_expand(evaluation[,fac_col], levels(pr_bg[,fac_col]))
        evaluation[ ,fac_col] <- forcats::fct_relevel(evaluation[,fac_col], levels(pr_bg[,fac_col]))
      }
    }
    
    # extract the relevant columns for modelling
    model_data <- pr_bg[, c("occ", covars[[r]])]
    
    # normalising covariates
    for(v in covars[[r]]){
      if(!v %in% categoricalvars){
        meaanv <- mean(model_data[, v])
        sdv <- sd(model_data[, v])
        model_data[, v] <- (model_data[, v] - meaanv) / sdv
        evaluation[, v] <- (evaluation[, v] - meaanv) / sdv
      }
    }
    
    
    myseed <- sum(model_data$occ) + 300 + n
    set.seed(myseed)
    
    # start modeling! We use the "try" notation so if a species fails to fit, the loop will continue.
    # preparing data
    myBiomodData <- myBiomodOption <- myBiomodModelOut <- myEnProjDF <- NULL
    myRespName <- "occ"
    myResp <- as.numeric(model_data[,myRespName])
    myResp[which(myResp == 0)] <- NA
    myExpl <- data.frame(model_data[,covars[[r]]])
    myRespXY <- pr_bg[, c("x", "y")]

    ptm <- proc.time()
    # create biomod data format
    myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                         expl.var = myExpl,
                                         resp.name = myRespName,
                                         resp.xy = myRespXY,
                                         PA.nb.absences = 50000,
                                         PA.strategy = 'random',
                                         na.rm = TRUE)
    # set options for biomod using my formula for mgcv
    myBiomodOption <- BIOMOD_ModelingOptions(
      GAM = list( algo = 'GAM_mgcv',
                  type = 's_smoother',
                  k = -1,
                  interaction.level = 0,
                  myFormula = as.formula(myform[[r]]),
                  family = binomial(link = 'logit'),
                  method = 'GCV.Cp',
                  optimizer = c('outer','newton'),
                  select = FALSE,
                  knots = NULL,
                  paraPen = NULL,
                  control = list(nthreads = 1, irls.reg = 0, epsilon = 1e-07, maxit = 200, trace = FALSE,
                                 mgcv.tol = 1e-07, mgcv.half = 15, rank.tol = 1.49011611938477e-08,
                                 nlm = list(ndigit=7, gradtol=1e-06, stepmax=2, steptol=1e-04, iterlim=200, check.analyticals=0),
                                 optim = list(factr=1e+07),
                                 newton = list(conv.tol=1e-06, maxNstep=5, maxSstep=2, maxHalf=30, use.svd=0), outerPIsteps = 0,
                                 idLinksBases = TRUE, scalePenalty = TRUE, keepData = FALSE, scale.est = "fletcher",
                                 edge.correct = FALSE)),
      MAXENT.Phillips = list( path_to_maxent.jar = "NCEAS_rerun_code", # change it to maxent directory
                              memory_allocated = 512,
                              background_data_dir = 'default',
                              maximumbackground = 50000,
                              maximumiterations = 200,
                              visible = FALSE,
                              linear = TRUE,
                              quadratic = TRUE,
                              product = TRUE,
                              threshold = TRUE,
                              hinge = TRUE,
                              lq2lqptthreshold = 80,
                              l2lqthreshold = 10,
                              hingethreshold = 15,
                              beta_threshold = -1,
                              beta_categorical = -1,
                              beta_lqp = -1,
                              beta_hinge = -1,
                              betamultiplier = 1,
                              defaultprevalence = 0.5)
      
    )
    # model fitting
    ptm <- proc.time()
    set.seed(myseed)
    myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,
                                        models = c('GLM','GBM','GAM','CTA','ANN','FDA','MARS','RF','MAXENT.Phillips'),
                                        models.options = myBiomodOption,
                                        NbRunEval = 1, 
                                        DataSplit = 100, 
                                        models.eval.meth = c("ROC"), 
                                        SaveObj = TRUE,
                                        rescal.all.models = FALSE,
                                        do.full.models = TRUE,
                                        modeling.id = paste(myRespName,"NCEAS_Modeling", sep = ""))
    # ensemble modelling using mean probability
    myBiomodEM <- BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut,
                                          chosen.models = 'all',
                                          em.by = 'all',
                                          eval.metric = c("ROC"),
                                          eval.metric.quality.threshold = NULL, # since some species auc are naturally low
                                          prob.mean = TRUE,
                                          prob.cv = FALSE,
                                          prob.ci = FALSE,
                                          prob.median = FALSE,
                                          committee.averaging = FALSE,
                                          prob.mean.weight = FALSE)
    t <- proc.time() - ptm
    # project single models
    myBiomodProj <- BIOMOD_Projection(modeling.output = myBiomodModelOut,
                                      new.env = as.data.frame(evaluation[, covars[[r]]]),
                                      proj.name = "nceas_modelling",
                                      selected.models = "all",
                                      binary.meth = "ROC",
                                      compress = TRUE,
                                      clamping.mask = TRUE)
    # project ensemble of all models
    myBiomodEnProj <- BIOMOD_EnsembleForecasting(projection.output = myBiomodProj,
                                                 EM.output = myBiomodEM,
                                                 selected.models = "all")
    # extracting the values for ensemble prediction
    myEnProjDF <- as.data.frame(get_predictions(myBiomodEnProj))

    # make and save the prediction
    out_file <- evaluation[, 1:4]
    out_file$spid <- sp_presence$spid[1]
    out_file$region <- r
    out_file$model <- "biomod"
    out_file$prediction <- myEnProjDF[,1]
    out_file$time <- t[3]
    
    write.csv(out_file, sprintf("%s/%s_biomod.csv", outdir, s), row.names = FALSE)
    print(n)
  }
}

sessionInfo()

