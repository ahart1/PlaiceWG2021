# This script sets up an initial framework for plaice MSE simulations

##### Define MSE variables #####
nyear <- 10 # 30 year projection
nsim <- 1 # Number of simulations to run
freqEM <- 3

# OM settings
env_driver_OM <- "BT" # Options: "None", "BT", "AMO"
env_driver_EM <- "BT"
stock_dynamic <- "q" # Options: "q", "recruit"  SPECIFY IF Q OR R IMPACTED BY ENV, PROVIDE DIRECTLY TO WHAM SETUP
forcing <- "increase" # Options: "increase", "decrease" SPECIFY DIRECTION OF ENVIRONMENTAL FORCING
env_noise <- "low" # Options: "low", "high" SPECIFY AMOUNT OF NOISE IN COVARIATE - MAY BE LOW OR HIGH
env_lag <- 0
outdir <- here::here("PlaiceMSE", "Results")

library(wham)
library(tidyverse)

##### Documentation #####
#' @param env_driver_OM A string indicating whether operating model (OM) has environmental driver ("BT" or "AMO") or not ("None"), default = "None"
#' \itemize{
#'   \item{"None" - No environmental impact specified}
#'   \item{"BT" - Bottom temperature anomaly impacts specified stock dynamic}
#'   \item{"AMO" - Atlantic-Multidecadal Oscillation impacts specified stock dynamic}
#' }
#' @param env_driver_EM A string indicating what environmental driver is assumed to effect stock dynamics for one of the two candidate estimating models, must match env_driver_OM unless model misspecification desired, default = "BT"
#' \itemize{
#'   \item{"BT" - Bottom temperature anomaly impacts specified stock dynamic}
#'   \item{"AMO" - Atlantic-Multidecadal Oscillation impacts specified stock dynamic} 
#' }
#' @param env_noise A string indicating whether environmental data has "high" or "low" noise, only required if env_driver_OM != "None", default = "low"
#' @param env_lag A number indicating the number of years to lag the environmental effect in the OM and EM, default = 0.
#' @param stock_dynamic A string indicating whether the environmental driver impacts catchability ("q") or recruitment ("recruit"), only required if env_driver_OM != "None", default = "q"
#' \itemize{
#'   \item{"q" - Environmental covariate impacts catchability, by default impacts all 4 indices (Bigelow and Albatross spring and fall)}
#'   \item{"recruit" - Environmental covariate impacts recruitment}
#' }
#' @param forcing A string indicating whether the environmental impact has and increasing ("increase") or decreasing ("decrease") effect, default = "increase". At present assume same forcing direction for EM & OM (but can change env variable between the two for partial misspecification)
#' @param nyear A number indicating the lenght of the projection, default = 30.
#' @param nsim A number indicating the number of MSE simulations to carry out, default = 3.
#' @param outdir A string specifying the output directory for simulation results files, default = current directory.
#' @param freqEM A number indicating the frequency with which stock assessments are performed

plaiceMSE <- function(env_driver_OM = "None",
                      env_driver_EM = "BT",
                      env_noise = "low",
                      env_lag = 0,
                      stock_dynamic = "q",
                      forcing = "increase",
                      nyear = 30,
                      nsim = 3,
                      outdir = getwd(),
                      freqEM = 3){
  
  ########## Setup ##########
  ##### Set up storage #####
  # Set directory for storing outputs, default is current working directory
  setwd(outdir)
  # Timestamp for result storage
  timeStamp <- Sys.time()
  # File name for result storage folder
  filelocation <- paste(outdir, paste("MSE_Results_envOM", env_driver_OM, "envEM", env_driver_EM, timeStamp, sep="_"), sep="/")
  # Create directory to store output
  dir.create(filelocation, showWarnings=TRUE) # makes output directory folder
  
  ##### Save function inputs in text file #####
  write("# MSE simulation settings", file=paste0(filelocation,"/simInputs.txt"), append = FALSE)
  write("# OM environmental driver setting", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(env_driver_OM, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write("# EM environmental driver setting", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(env_driver_EM, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write("# Environmental noise level", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(env_noise, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write("# Environmental lag", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(env_lag, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write("# Stock dynamic linked to environment", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(stock_dynamic, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write("# Direction of environmental forcing", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(forcing, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write("# Number of years projected in simulation", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(nyear, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write("# Number of simulations", file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  write(nsim, file = paste0(filelocation,"/simInputs.txt"), append = TRUE)
  
  ##### Read in historic environmental driver for the MSE simulations #####
  if(env_driver_OM == "BT" | env_driver_EM == "BT"){ #!!! Need to figure out if I need to read in this data based on the OM setting s(i.e. directional change in this data, if so probably want OM setup first so this can be done within OM loop)
    # Load historic bottom temperature anomaly data 
    BT <- read.csv(here::here("data", "MSE_data", "GLORYS_se.csv")) # Same data explored in research track but naming standardized
    BT <- BT %>% filter(Year>1979)
    # Specify what observations to use (all available span 1980-2019) 
    use_obs = matrix(c(rep(TRUE, nrow(BT))), ncol = 1, nrow=nrow(BT)) # use all obs except the first 3 (no data 1979, 1980, and 1981 not full year of data in 1981) # !!! this comes from input argument settings
    
  } else if (env_driver_OM == "AMO" | env_driver_EM == "AMO"){
    # Load and filter AMO data to match year range of other model data
    AMO <- read.csv(paste(here::here(), "data", "MSE_data", "AMO_se.csv", sep="/")) # Same data explored in research track but naming standardized
    # Add placeholder for missing years of data
    AMO <- rbind(c(1981, -999, -999, -999, -999), AMO) # 1981 years in the wrong order will cause R to bomb
    AMO <- rbind(c(1980, -999, -999, -999, -999), AMO) # 1980
    AMO <- rbind(c(1979, -999, -999, -999, -999), AMO) # 1979
    # Specify what observations to use (all available span 1982-2019 for this dataset)
    use_obs = matrix(c(FALSE, FALSE, FALSE, rep(TRUE, nrow(AMO)-3)), ncol = 1, nrow=nrow(AMO)) # use all obs except the first 3 (no data 1979, 1980, and 1981 not full year of data in 1981) # !!! this comes from input argument settings
  }
  
  ##### Set up shared EM/setupOM features (misspecification in env only) #####
  ### Load data
  asap3 <- read_asap3_dat(paste(here::here(), "data", "PlaiceWHAM-2019_revised_NEFSC-LW-WAA_splitNEFSC-BigUnit.DAT", sep="/"))
  
  ### Prepare model input data
  NAA_re = list(sigma = "rec+1") # Full state-space model
  NAA_re$cor = "iid" # iid random effects
  NAA_re$recruit_model = 2 # recruitment is random about the mean
  NAA_re$recruit_pars = exp(10) #initial guess for mean recruitment
  # Setup initial selectivity model and parameters
  use_n_indices = 4
  modelsetup <- c(rep("logistic", asap3$dat$n_fleet_sel_blocks), rep("age-specific", use_n_indices))
  # Setup fixed parameters
  fix_fleet_sel <- lapply(1:asap3$dat$n_fleets, function(x) NA) 
  fix_index_sel <- lapply(1:use_n_indices, function(x) NA) # Set up index object
  fix_index_sel[[1]] <- c(6) # Fix age 6  for for index 1 (NEFSC spring Albatross) # Based on preliminary run
  fix_index_sel[[2]] <- c(5) # Fix age 5 for for index 2 (NEFSC spring Bigelow)
  fix_index_sel[[3]] <- c(4) # Fix age 4 for for index 3 (NEFSC fall Albatross)
  fix_index_sel[[4]] <- c(3) # Fix age 3  for for index 4 (NEFSC spring Bigelow)
  init_fleet_sel <- list(c(2,0.4)) # logistic parameters, based on model type
  init_index_sel <- lapply(1:use_n_indices, function(x) c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
  for(i in 1:use_n_indices) init_index_sel[[i]][fix_index_sel[[i]]] <- 1 # replace initial values for 1 for the ages where selectivity fixed
  # Setup random effect by selectivity block (here: fleet, index1, index2, index3, index4)
  randeffect <- c(rep("iid", asap3$dat$n_fleet_sel_blocks), rep("none", 4)) # Don't include selectivity random effects for any surveys 
  # Setup selectivity list
  sel_list <- list(model = modelsetup, # list selectivity model for each fleet and index
                   re = randeffect,
                   initial_pars = c(init_fleet_sel, init_index_sel),
                   fix_pars = c(fix_fleet_sel, fix_index_sel))
  
  ##### WHAM setupOM based on MSE function arguments & fit to real data #####
  # The setupOM generated in this section is used to simulate historic datasets for the OM in each MSE simulation (so data internally consistent over timeseries)
  # Set up OM when no environmental driver, fit to env driver assumed by 
  if(env_driver_OM == "None"){ # OMs without environmental driver - set up like EM, essentially a self-test
    # Read in mean trends for projection of selected env_driver_OM (subsequent if statements set up OM with selected trend)
    projFile <- list.files(here::here("data", "MSE_data"), pattern = paste0("proj", env_driver_EM))
    projData <- read.csv(here::here("data", "MSE_data", projFile))
    
    # Filter projData to setup years
    setupData <- projData %>% 
      filter(Year < asap3$dat$year1 + asap3$dat$n_years) %>% 
      filter(Year > asap3$dat$year1-1)
    
    # Setup ecov_OM so available to EM, OM not fit to this data
    ecov_OM <- list(
      label = env_driver_OM,
      mean = as.matrix(setupData[,which(grepl(forcing, names(setupData), fixed = TRUE))]),
      logsigma = as.matrix(log(setupData[,which(grepl(forcing, names(setupData), fixed = TRUE))])),
      year = as.numeric(setupData$Year),
      use_obs = use_obs,
      lag = 0,
      process_model = "rw",
      where = "none",
      how = 0)
    
    # Regenerate input as for inputEM_withoutEnv but DO NOT FIT ENV DATA
    inputOM <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "OM", age_comp = "logistic-normal-miss0") 
    setupOM <- fit_wham(input = inputOM, do.retro=FALSE, do.osa=FALSE, MakeADFun.silent=TRUE)
    
  } else if(stock_dynamic == "q"){ # Catchability OMs
    
    # Read in mean trends for projection of selected env_driver_OM (subsequent if statements set up OM with selected trend)
    projFile <- list.files(here::here("data", "MSE_data"), pattern = paste0("proj", env_driver_OM))
    projData <- read.csv(here::here("data", "MSE_data", projFile))
    
    # Filter projData to setup years
    setupData <- projData %>% 
      filter(Year < asap3$dat$year1 + asap3$dat$n_years) %>% 
      filter(Year > asap3$dat$year1-1)
    
          # Populate ecov object for OM with historic environmental data
          ecov_OM <- list(
            label = env_driver_OM, 
            mean = as.matrix(setupData[,which(grepl(forcing, names(setupData), fixed = TRUE))]), 
            logsigma = as.matrix(log(setupData[,which(grepl(forcing, names(setupData), fixed = TRUE))])), #  need to reduce standard error - in projData file (ask Lisa and Tim what SE is relatively noisy/not noisy data)
            year = as.numeric(setupData$Year), 
            use_obs = use_obs,
            lag = env_lag, 
            process_model = "rw",
            where = stock_dynamic, 
            how = 1,
            indices = list(c(1,2,3,4)))
          
          # Format input #!!! look at Jamie's work regarding shift in depth/BT, maybe can have consistent rate to match obs and add noise
          inputOM <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "OM", age_comp = "logistic-normal-miss0", ecov = ecov_OM) 
          # Fit OM
          setupOM <- fit_wham(input = inputOM, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent=TRUE)
          
      
      # Recruitment OMs 
    } else if(stock_dynamic == "recruit"){
      
      # Read in mean trends for projection of selected env_driver_OM (subsequent if statements set up OM with selected trend)
      projFile <- list.files(here::here("data", "MSE_data"), pattern = paste0("proj", env_driver_OM))
      projData <- read.csv(here::here("data", "MSE_data", projFile))
      
      # Filter projData to setup years
      setupData <- projData %>% 
        filter(Year < asap3$dat$year1 + asap3$dat$n_years) %>% 
        filter(Year > asap3$dat$year1-1)
      
          # Populate ecov object for OM with historic environmental data
          ecov_OM <- list(
            label = env_driver_OM, 
            mean = as.matrix(setupData[,which(grepl(forcing, names(setupData), fixed = TRUE))]), 
            logsigma = as.matrix(log(setupData[,which(grepl(forcing, names(setupData), fixed = TRUE))])), #  need to reduce standard error - in projData file (ask Lisa and Tim what SE is relatively noisy/not noisy data)
            year = as.numeric(setupData$Year), 
            use_obs = use_obs,
            lag = env_lag, 
            process_model = "rw",
            where = stock_dynamic, 
            how = 1) # controlling = density-independent mortality
          
          # Format input 
          inputOM <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "OM", age_comp = "logistic-normal-miss0", ecov = ecov_OM) 
          # Fit OM
          setupOM <- fit_wham(input = inputOM, do.retro = FALSE, do.osa = FALSE, MakeADFun.silent=TRUE)
          
        
    } # End if statements for stock_dynamic options
  
  ##### Simulate historic data based on selected OM ##### 
  # Set (and save) random seed for each simulation so replicible, save and read in so used for each model
  sim.seeds <- sample(1:1000000, nsim, replace = FALSE)
  saveRDS(sim.seeds, file.path(paste(filelocation, "sim_seeds.rds", sep="/")))
  seeds <- readRDS(paste(filelocation, "sim_seeds.rds", sep="/"))

  # Simulate OM historic data for each simulation
  OMsimdata <- NULL
  for(isim in 1:nsim){ # Loop over simulations 
    # Set simulation seed
    set.seed(seeds[isim])
    # Simulate OM data
    OMsimdata[[isim]] <- setupOM$simulate(par=setupOM$env$last.par.best, complete=TRUE)
  } 
  
  # Save OM data for all simulations
  saveRDS(OMsimdata, file = paste(filelocation, "OMdata.rds", sep="/"))
  
  
  ########### Don't think this is necessary ###############
  # ##### Generate setupEM inputs and fit to real data (overwritten in simulations) #####
  # 
  # # Read in mean trends for projection of selected env_driver_EM 
  # projFile <- list.files(here::here("data", "MSE_data"), pattern = paste0("proj", env_driver_EM))
  # projData <- read.csv(here::here("data", "MSE_data", projFile))
  # 
  # # Filter projData to setup years
  # setupData <- projData %>% filter(Year < 2020)
  # 
  # ### Setup environmental covariate with no effect 
  # ecov_noEffect <- ecov_OM # Initialize using OM settings # NO ecov_OM if not env link???!!!!!!!
  # # Update pieces that differ from the OM
  # ecov_noEffect$label <- env_driver_EM 
  # ecov_noEffect$logsigma <- log(exp(ecov_OM$logsigma) + exp(ecov_OM$logsigma)/2) # Increase SE half as much as OM (not actually used when no covariate)
  # ecov_noEffect$lag <- 0 # No lag
  # ecov_noEffect$where <- "none"
  # ecov_noEffect$how <- 0 
  # 
  # ### setupEM no environmental covariate
  # # Same input as when env_driver_OM == "None", but fit to env covariate with no effect
  # inputEM_withoutEnv <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "EM_withoutEnv", age_comp = "logistic-normal-miss0", ecov=ecov_noEffect) # logistic normal age comp, 0s treated as missing
  # setupEM_withoutEnv <- fit_wham(inputEM_withoutEnv, do.retro = TRUE, do.osa = FALSE, MakeADFun.silent = TRUE)
  # 
  # ### setupEM environmental covariate with effect
  # ecov_withEffect <- ecov_OM # Initialize using OM settings # Does not work if OM has no ecov
  # # Update pieces that differ from the OM
  # ecov_withEffect$label <- env_driver_EM
  # ecov_withEffect$logsigma <- log(exp(ecov_OM$logsigma) + exp(ecov_OM$logsigma)/2) # Increase SE half as much as OM
  # 
  # inputEM_withEnv <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "EM_withEnv", age_comp = "logistic-normal-miss0", ecov=ecov_withEffect) # logistic normal age comp, 0s treated as missing
  # setupEM_withEnv <- fit_wham(inputEM_withEnv, do.retro = TRUE, do.osa = FALSE, MakeADFun.silent = TRUE)
  # 
  ### setupEM environmental covariate without effect but WITH random effects for specified stock_dynamic
  if(stock_dynamic == "q"){ # Add catchability random effect for all indices
    # Setup catchability random effect
    catchability <- list(re=rep("iid",4))
    # Fit model
    # inputEM_noEnv_randQ <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "EM_noEnv_randQ", age_comp = "logistic-normal-miss0", ecov=ecov_noEffect, catchability = catchability) # logistic normal age comp, 0s treated as missing, based on WG run 29F4
    # EM_withoutEnv_rand <- fit_wham(inputEM_noEnv_randQ, do.retro=TRUE, do.osa=FALSE, MakeADFun.silent = TRUE)
  } else if(stock_dynamic == "R"){ # Add recruitment random effect for all indices - this might not make sense to do if using accepted assessment for plaice !!!
    #!!! fill this in
  }
  
  
  ##### Loop over simulations #####
  for(isim in 1:nsim){
    
    ##### Set up simulation's OM #####
    # Pull original setupOM input
    inputSimOM <- setupOM$input
    # Pull OM historic (simulated) data for this simulation
    simdata <- OMsimdata[[isim]]
    # Use to overwrite the following values in the setupOM input (which was fit to real data) so simOM self-consistent
    obs_names <-  c("agg_catch","agg_indices", "catch_paa", "index_paa", "Ecov_obs", "obs", "obsvec") # Other input$data objects match original OM
    inputSimOM$data[obs_names] <- simdata[obs_names] # overwrite storage for this simulation input
    inputSimOM$par <- setupOM$parList
    # Fit OM to simulated data (rather than raw data as in setupOM)
    simOM <- fit_wham(inputSimOM, do.sdrep=F, do.osa=F, do.retro=T, do.proj=F, MakeADFun.silent=TRUE)
    
    ##### Set up storage #####
    advice <- rep(NA, nyear)
    
    ##### MISSING ADVICE FOR FIRST YEAR OF MSE #####
    advice[1] <- 10000 # placeholder!!!!!!
    
    ##### Loop over projection years in simulation #####
    # Specify the number of historic (base) years to which projection is appended
    hist_year <- length(simOM$years) # !!! update with some specific value of length baseyears
    
    for(iproj in seq(1,nyear, by=freqEM)){
      ##### OM #####
      ### Project OM forward one year under the F from management advice
      # Provide management info to projection of nyear years (only the projected years 1:iproj are used to increase the observed data)
      projInput <- prepare_projection(simOM, proj.opts = list(n.yrs = nyear,
                                                              use.last.F = FALSE,
                                                              use.avg.F = FALSE,
                                                              use.FXSPR = FALSE,
                                                              use.FMSY = FALSE,
                                                              # proj.F = 0.2,
                                                              avg.yrs = tail(simOM$years, 5), # Use avg over last 5 yrs for ref pts
                                                              proj.catch = advice, 
                                                              cont.ecov = TRUE,
                                                              use.last.ecov = FALSE,
                                                              # proj.ecov = testEcov, # !!! Figure out how to use this instead so pulling from vector/table of env timeseries instead of using avg
                                                              avg.ecov.yrs = tail(simOM$years, 5)))
      # Set up model for projection with appended advice input
      projOM <- fit_wham(projInput, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE, do.proj = FALSE, MakeADFun.silent = TRUE, do.fit = FALSE)
      # Do projection
      set.seed(sim.seeds[isim]) # Use same seed for the entire simulation so projection generates same historic time series
      resultOM <- projOM$simulate(complete=TRUE)
      
      # I don't think I need this!!!!
      # ### Update the simOM with the projected data and reset the OM for use in the next timestep
      # # Update with projected data
      # projInput$par$log_NAA = resultOM$log_NAA
      # # Reset the OM
      # simOM <- fit_wham(projInput, n.newton=n.newton, do.sdrep=F, do.retro=F, do.osa=F, do.check=F, do.proj=F,
      #                MakeADFun.silent = TRUE, save.sdrep=FALSE, do.fit = F)
      
      
      ##### Observation model #####
      # !!!!!! still need to add as data passed from resultOM to EM
      
      
      ##### EM  ##### 
      ### Update general EM data (only ecov & random effect data vary between EMs in this study - this info initialized below)
      # Initialize new input data
      inputEM_setup <- NULL 
      # Append index & catch data from OM #!!!! Add obs error here
      inputEM_setup$years <- asap3$dat$year1:max(asap3$dat$year1 + asap3$dat$n_years - 1 + iproj) # Add proj to historic time series whose length matches asap3 data (-1 included so first year not double counted)
      inputEM_setup$agg_indices <- rbind(resultOM$agg_indices, resultOM$agg_indices_proj[1:iproj,,drop=F]) # Append projected index data through year iproj
      inputEM_setup$agg_catch <- rbind(resultOM$agg_catch, resultOM$agg_catch_proj[1:iproj,,drop=F]) # Append projected catch data through year iproj
      inputEM_setup$index_paa <- abind::abind(resultOM$index_paa, resultOM$index_paa_proj[,1:iproj,,drop=F], along=2) # Append index PAA through year iproj
      inputEM_setup$catch_paa <- abind::abind(resultOM$catch_paa, resultOM$catch_paa_proj[,1:iproj,,drop=F], along=2) # Append catch PAA through year iproj
      inputEM_setup$n_years_catch <- length(inputEM_setup$agg_catch)
      inputEM_setup$n_years_indices <- length(inputEM_setup$years)
      inputEM_setup$n_years_model <- length(inputEM_setup$years)
      # Use catch paa from all years
      inputEM_setup$use_catch_paa <- matrix(1,inputEM_setup$n_years_model, ncol=1) 
      # Use index data from all years (but terminal years differ since Bigelow indices have data)
      if(iproj==1){ # In first year of MSE projection only add 1 year of index data for use
        resultOM$use_index_paa <- rbind(resultOM$use_index_paa, resultOM$use_index_paa[nrow(resultOM$use_index_paa),]) # Use new index_paa data as in last year of OM
        inputEM_setup$catch_Neff <- rbind(resultOM$catch_Neff, tail(resultOM$catch_Neff, n=1)) # Assume catch_Neff in projection same as in last year
        inputEM_setup$index_Neff <- rbind(resultOM$index_Neff, resultOM$index_Neff[nrow(resultOM$index_Neff),]) # Assume index_Neff in projection same as in last year
      } else{ # in subsequent projection years add data in steps of assessment frequency
        for(i in 1:freqEM){
          resultOM$use_index_paa <- rbind(resultOM$use_index_paa, resultOM$use_index_paa[nrow(resultOM$use_index_paa),]) # Use new index_paa data as in last year of OM
          # Update Neff
          resultOM$catch_Neff <- rbind(resultOM$catch_Neff, tail(resultOM$catch_Neff, n=1)) # Assume catch_Neff in projection same as in last year
          resultOM$index_Neff <- rbind(resultOM$index_Neff, resultOM$index_Neff[nrow(resultOM$index_Neff),]) # Assume index_Neff in projection same as in last year
        }
        inputEM_setup$catch_Neff <- resultOM$catch_Neff
        inputEM_setup$index_Neff <- resultOM$index_Neff
      }
      inputEM_setup$use_index_paa <- resultOM$use_index_paa
      
      
      # Use WAA from projection !!! currently without error
      inputEM_setup$waa <- resultOM$waa[,1:inputEM_setup$n_years_model,,drop=F] # abind::abind(resultOM$waa, resultOM$waa[,length(inputEM_setup$years),,drop=F], along=2)
      
      # Pass the remaining EM setup info pulled from the OM and passed to basic_info (i.e. generic EM settings that are not specific to one of the 4 EM options) - assume no misspecification in selectivity blocks/#fleets/#indices
      inputEM_setup$ages <- 1:resultOM$n_ages
      inputEM_setup$n_fleets <- resultOM$n_fleets
      inputEM_setup$n_years_selblocks <- rep(length(inputEM_setup$years), 5) # Assume 1 fleet + 4 indices !!!!
      inputEM_setup$selblock_pointer_fleets <- rbind(resultOM$selblock_pointer_fleets, tail(resultOM$selblock_pointer_fleets,n=1))
      inputEM_setup$n_indices <- resultOM$n_indices
      inputEM_setup$units_indices <- resultOM$units_indices
      inputEM_setup$units_index_paa <- resultOM$units_index_paa
      inputEM_setup$selblock_pointer_indices <- rbind(resultOM$selblock_pointer_indices, resultOM$selblock_pointer_indices[nrow(resultOM$selblock_pointer_indices),])
      # for(i in 1:freqEM){
      #   resultOM$mature <- rbind(resultOM$mature, resultOM$mature[nrow(resultOM$mature),]) 
      # }
      inputEM_setup$maturity <- matrix(rep(resultOM$mature[1,], inputEM_setup$n_years_model), ncol = length(inputEM_setup$ages), byrow=TRUE) # Maturity schedule remains consistent
      inputEM_setup$fracyr_SSB <- rep(0.25,inputEM_setup$n_years_model)
      inputEM_setup$fracyr_indices <- rbind(resultOM$fracyr_indices, resultOM$fracyr_indices[nrow(resultOM$fracyr_indices),])
      inputEM_setup$Fbar_ages <- as.integer(resultOM$Fbar_ages)
      # !!! double check that it makes sense to have no obs error for q
      # inputEM_setup$q <- asap3$dat$q_ini # Use same initial q as in plaice data runs
      inputEM_setup$percentSPR <- resultOM$percentSPR
      inputEM_setup$percentFXSPR <- resultOM$percentFXSPR
      inputEM_setup$percentFMSY <- resultOM$percentFMSY
      
      
      ### Do assessment without Env covariate (status quo) !!! need to fit to data but without effect!!! 
      ## Update ecov object with additional years of data
      # Initialize object with OM settings
      ecov_withoutEffect <- ecov_OM 
      # Pull extended ecov time series
      envDat <- projData %>% 
        filter(Year < asap3$dat$year1 + asap3$dat$n_years + iproj) %>% # Don't extend past iproj year
        filter(Year > asap3$dat$year1-1) # Don't go back in time farther than first asap3 year
      
      # Overwrite with EM specific settings & append new years of data
      ecov_withoutEffect$label <- env_driver_EM
      ecov_withoutEffect$mean <- as.matrix(envDat[,which(grepl(forcing, names(envDat), fixed = TRUE))])
      ecov_withoutEffect$logsigma <- as.matrix(log(envDat[,which(grepl(forcing, names(envDat), fixed = TRUE))]))
      ecov_withoutEffect$year <- as.numeric(envDat$Year)
      ecov_withoutEffect$use_obs <- as.matrix(rep(TRUE, length(ecov_withoutEffect$year)), ncol=1)
      ecov_withoutEffect$lag <- 0
      ecov_withoutEffect$where <- "none"
      ecov_withoutEffect$how <- 0 # process & indices arguments are consistent with the OM settings
      # Updated input
      inputEM_withoutEnv <- prepare_wham_input(basic_info = inputEM_setup, NAA_re = NAA_re, selectivity = sel_list, model_name = "EM_withoutEnv", age_comp = "logistic-normal-miss0", ecov=ecov_withoutEffect)
      ## Fit EM to simulated data from OM with sdreport - originally without
      fit_withoutEnv <- fit_wham(inputEM_withoutEnv, do.sdrep=TRUE, do.osa=F, do.retro=T, do.proj=F, MakeADFun.silent=TRUE) 
      # Add OSA residual calculation for fit to aggregate indices and environmental data
        # Rename fitted model so it can be manipulated in OSA residual calcs without losing info
        fitted_withoutEnv <- fit_withoutEnv
        # Remove paa data from model input object so OSA only calculated for aggregate index and Ecov (as in first few lines of make_osa_residuals())
        fitted_withoutEnv$input$data$obs <- fitted_withoutEnv$input$data$obs %>% filter(type != "indexpaa") %>% filter(type != "catchpaa")
        # Calculate OSA residual
        modOSA_withoutEnv <- make_osa_residuals(fitted_withoutEnv)
        
      ### Do assessment with Env covariate (status quo)
      ## Update ecov object with additional years of data
      # Initialize object with OM settings
      ecov_withEffect <- ecov_OM 
      # Pull extended ecov time series
      envDat <- projData %>% 
        filter(Year < asap3$dat$year1 + asap3$dat$n_years + iproj) %>% # Don't extend past iproj year
        filter(Year > asap3$dat$year1-1) # Don't go back in time farther than first asap3 year
      # Overwrite with EM specific settings & append new years of data
      ecov_withEffect$label <- env_driver_EM
      ecov_withEffect$mean <- as.matrix(envDat[,which(grepl(forcing, names(envDat), fixed = TRUE))])
      ecov_withEffect$logsigma <- as.matrix(log(envDat[,which(grepl(forcing, names(envDat), fixed = TRUE))]))
      ecov_withEffect$year <- as.numeric(envDat$Year)
      ecov_withEffect$use_obs <- as.matrix(rep(TRUE, length(ecov_withEffect$year)), ncol=1)
      ecov_withEffect$lag <- env_lag
      ecov_withEffect$where <- stock_dynamic
      ecov_withoutEffect$how <- 1 # process & indices arguments are consistent with the OM settings
      # Updated input
      inputEM_withEnv <- prepare_wham_input(basic_info = inputEM_setup, NAA_re = NAA_re, selectivity = sel_list, model_name = "EM_withEnv", age_comp = "logistic-normal-miss0", ecov=ecov_withEffect)
      ## Fit EM to simulated data from OM with sdreport - originally without
      fit_withEnv <- fit_wham(inputEM_withEnv, do.sdrep=TRUE, do.osa=F, do.retro=T, do.proj=F, MakeADFun.silent=TRUE) 
      # Add OSA residual calculation for fit to aggregate indices and environmental data
        # Rename fitted model so it can be manipulated in OSA residual calcs without losing info
        fitted_withEnv <- fit_withEnv
        # Remove paa data from model input object so OSA only calculated for aggregate index and Ecov (as in first few lines of make_osa_residuals())
        fitted_withEnv$input$data$obs <- fitted_withEnv$input$data$obs %>% filter(type != "indexpaa") %>% filter(type != "catchpaa")
        # Calculate OSA residual
        modOSA_withEnv <- make_osa_residuals(fitted_withEnv)
      
      ### Do assessment without Env covariate but with random effect (fit to env data but no effect)
      ## Update ecov object with additional years of data
      # Initialize object with OM settings
      ecov_withoutEffect_rand <- ecov_OM 
      # Pull extended ecov time series
      envDat <- projData %>% 
        filter(Year < asap3$dat$year1 + asap3$dat$n_years + iproj) %>% # Don't extend past iproj year
        filter(Year > asap3$dat$year1-1) # Don't go back in time farther than first asap3 year
      # Overwrite with EM specific settings & append new years of data
      ecov_withoutEffect_rand$label <- env_driver_EM
      ecov_withoutEffect_rand$mean <- as.matrix(envDat[,which(grepl(forcing, names(envDat), fixed = TRUE))])
      ecov_withoutEffect_rand$logsigma <- as.matrix(log(envDat[,which(grepl(forcing, names(envDat), fixed = TRUE))]))
      ecov_withoutEffect_rand$year <- as.numeric(envDat$Year)
      ecov_withoutEffect_rand$use_obs <- as.matrix(rep(TRUE, length(ecov_withoutEffect$year)), ncol=1)
      ecov_withoutEffect_rand$lag <- 0
      ecov_withoutEffect_rand$where <- "none"
      ecov_withoutEffect_rand$how <- 0 # process & indices arguments are consistent with the OM settings
      # Updated input
      inputEM_withoutEnv_rand <- prepare_wham_input(basic_info = inputEM_setup, NAA_re = NAA_re, selectivity = sel_list, model_name = "EM_withoutEnv_rand", age_comp = "logistic-normal-miss0", ecov=ecov_withoutEffect_rand, catchability = catchability) 
      ## Fit EM to simulated data from OM with sdreport
      fit_withoutEnv_rand <- fit_wham(inputEM_withoutEnv_rand, do.sdrep=TRUE, do.osa=F, do.retro=T, do.proj=F, MakeADFun.silent=TRUE)
      # Add OSA residual calculation for fit to aggregate indices and environmental data
        # Rename fitted model so it can be manipulated in OSA residual calcs without losing info
        fitted_withoutEnv_rand <- fit_withoutEnv_rand
        # Remove paa data from model input object so OSA only calculated for aggregate index and Ecov (as in first few lines of make_osa_residuals())
        fitted_withoutEnv_rand$input$data$obs <- fitted_withoutEnv_rand$input$data$obs %>% filter(type != "indexpaa") %>% filter(type != "catchpaa")
        # Calculate OSA residual
        modOSA_withoutEnv_rand <- make_osa_residuals(fitted_withoutEnv_rand)
      
      ### Compare diagnostics for the three runs
      # Set up storage for diagnostics 
      compareEM <- matrix(rep(NA,6*3), ncol=3)
      colnames(compareEM) <- c("withEnv", "withoutEnv", "withoutEnv_rand")
      rownames(compareEM) <- c("parsimony", "AIC", "deltaAIC", "mohns_rhoSSB", "mohns_rhoFbar", "mohns_rhoR")
      
      # Parsimony (score only to pick when there are multiple equivalent models and a tie between them)
      compareEM["parsimony", ] <- c(2,0,1) # higher score = less parsimonious model
      # AIC
      # mohn's rho
      # (not in table yet) want OSA residuals for only aggregate fits - could use to assess normality (AIC and normality needs to be better for at least 1-2 indices)
      
      # Calculate AIC - adapted from compare_wham_models() lines 112-115 
        # with environmental covariate
        k_withEnv <- length(fit_withEnv$opt$par)
        fit_withEnv$AIC <- 2*(fit_withEnv$opt$obj + k_withEnv)[1]
        compareEM["AIC", "withEnv"] <- fit_withEnv$AIC
        # without environmental covariate
        k_withoutEnv <- length(fit_withoutEnv$opt$par)
        fit_withoutEnv$AIC <- 2*(fit_withoutEnv$opt$obj + k_withoutEnv)[1]
        compareEM["AIC", "withoutEnv"] <- fit_withoutEnv$AIC
        # without environmental covariate BUT with random effect
        k_withoutEnv_rand <- length(fit_withoutEnv_rand$opt$par)
        fit_withoutEnv_rand$AIC <- 2*(fit_withoutEnv_rand$opt$obj + k_withoutEnv_rand)[1]
        compareEM["AIC", "withoutEnv_rand"] <- fit_withoutEnv_rand$AIC
        
      # Calculate delta AIC (absolute difference)
        minAIC <- min(compareEM["AIC",])
        compareEM["deltaAIC",] <- abs(compareEM["AIC",]) - abs(minAIC)
        
      # Calculate mohn's rho values
        # with environmental covariate
        fit_withEnv$mohns_rho = mohns_rho(fit_withEnv)
        compareEM[2:4,"withEnv"] <- fit_withEnv$mohns_rho[1:3]
        # without environmental covariate
        fit_withoutEnv$mohns_rho = mohns_rho(fit_withoutEnv)
        compareEM[2:4,"withoutEnv"] <- fit_withoutEnv$mohns_rho[1:3]
        # without environmental covariate BUT with random effect
        fit_withoutEnv_rand$mohns_rho = mohns_rho(fit_withoutEnv_rand)
        compareEM[2:4,"withoutEnv_rand"] <- fit_withoutEnv_rand$mohns_rho[1:3]
      
      ## Pick between EMs, select the model with the larger number of smaller values
      # Check if delta AIC is < 2 (and not 0), if so pick more parsimonious model
      checkAIC <- compareEM["deltaAIC",] <= 2 & compareEM["deltaAIC",] != 0
      if("TRUE" %in% checkAIC){ # At least one model is within 2 of the smallest AIC
        equivalentMod <- compareEM[, which(compareEM["deltaAIC",] <= 2)] # Look at all equivalent models & pick based on best overall performance
        
        # Take absolute value of mohns_rho values
        equivalentMod["mohns_rhoSSB",] <- abs(equivalentMod["mohns_rhoSSB",])
        equivalentMod["mohns_rhoFbar",] <- abs(equivalentMod["mohns_rhoFbar",])
        equivalentMod["mohns_rhoR",] <- abs(equivalentMod["mohns_rhoR",])
        
        # ID what equivalent model has best(smallest) statistics
        best <- NULL
        best$AIC <- NA
        best$deltaAIC <- names(which(equivalentMod["deltaAIC",]==0))
        best$mohns_rhoSSB <- names(which(equivalentMod["mohns_rhoSSB",] == min(equivalentMod["mohns_rhoSSB",])))
        best$mohns_rhoFbar <- names(which(equivalentMod["mohns_rhoFbar",] == min(equivalentMod["mohns_rhoFbar",])))
        best$mohns_rhoR <- names(which(equivalentMod["mohns_rhoR",] == min(equivalentMod["mohns_rhoR",])))
        
        equivalentMod <- cbind(equivalentMod, best)
        
        # Pick model with the highest number of "best" (smallest) statistics
        pickName <- equivalentMod %>% 
          as.data.frame %>% 
          filter(best != "NA") %>% 
          count(best) %>% 
          filter(n == max(n)) %>% select(best) %>% unlist()
        
        # If 2+ models tied for best, reset pickName to most parsimonious of the tied models
        if(length(pickName) > 1){ 
          pickName <- which(compareEM["parsimony",pickName] == min(compareEM["parsimony",pickName])) %>% names()
        } 
        # Now pick corresponding EM
        if(pickName == "withEnv"){
          pickEM <- fit_withEnv
        } else if(pickName == "withoutEnv"){
          pickEM <- fit_withoutEnv
        } else if(pickName == "withoutEnv_rand"){
          pickEM <- fit_withoutEnv_rand
        }
        
        # End if statement for selecting best from equivalent models
      } else{ # Pick best model based on smallest AIC score
        pickName <- names(which(compareEM["deltaAIC",] == 0))
        
        # Now pick corresponding EM
        if(pickName == "withEnv"){
          pickEM <- fit_withEnv
        } else if(pickName == "withoutEnv"){
          pickEM <- fit_withoutEnv
        } else if(pickName == "withoutEnv_rand"){
          pickEM <- fit_withoutEnv_rand
        }
      }  
      
      
      ##### Mngmt #####
      # Project the picked EM for 5 years under average F from the last 5 years
        # !!!! should we project under F40 instead????
      EM_proj <- project_wham(pickEM,
                              proj.opts = list(n.yrs=freqEM, # Project number of years until next EM performed
                                               use.last.F = FALSE, # Don't use terminal year F for projections, instead use F averaged over avg.yrs
                                               use.avg.F = TRUE, 
                                               avg.yrs=tail(pickEM$years, n=5)), # Avg SSB/R and YPR inputs over most recent 5 years
                                               # Default continues env process (random walk/AR1) but could use env in last year for projection or average over avg.yrs
                              MakeADFun.silent = TRUE)
      # Generate advice using average projected catch over 5 years of fishing at F40 (assume no implementation error)
      advice[iproj] <- EM_proj$rep$pred_catch[length(EM_proj$years) + 1:freqEM]
      
      
    } # End loop over years
    
  } # End loop over simulations
  
} # End function definition


########## Test projection??? need to check this piece
om_input <- prepare_projection(models$Run29F4, proj.opts=list(n.yrs=39, use.last.F=FALSE, use.avg.F=FALSE,
                                                     use.FXSPR=FALSE, proj.F=NULL, proj.catch=0.5, avg.yrs=NULL,
                                                     cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL))


temp <- fit_wham(om_input, n.newton=n.newton, do.sdrep=F, do.retro=F, do.osa=F, do.check=F, do.proj=F,
                 MakeADFun.silent = TRUE, save.sdrep=FALSE, do.fit = F)

# ? For Lisa/Tim
  # Change perception of env data noise via SE - what is a good low (not noisy) vs. high SE?
    # Also possible to add noise to observations themselves and leave SE within the range of current observations (draw from a log-normal distribution so always positive?)
  # Does Jamie have regression of BT anomaly on Depth? - could use to generate directional projection but would need to check that plaice can continue to shift at this rate (i.e. there are deep enough sections of GOM to support)

# Use deterministic trend for each simulation? Or set up trend and sample from normal distribution where trend data = mean, and std deviation is either high or low (correspond with high/low std error)

# I haven't built in timescale for forcing - this will impact how env info is generated
# Forcing direction
  # worth exploring only increasing effect?
  # for catchability increasing effecth could be continued mvmt to deeper water, colder temp, what happens when max depth reached?
  # for catchability decreasing effect could be move to deeper/colder water until max depth/min historic temp  reached then water temp increase?

# ANSWERS BELOW

# DO THIS
# Add noise around mean directional trend - add stochastic error
# SE = avg over last 10 years of actual data or whole time series
# Do 20-30 years
# look at relationship between temperature and q in OM for initial sims
# CMIP 5 projections -rcp 8.7 - anomaly projection, just use this
# stochastic normal error 0.1/0.2 for std dev, mean = 0
  # characterize variability in actual projection to get high and low variability to get std dev - get in ballpark of observed values from historical time period
# do first runs without stochasisity, see if rcp 8.7 pushes trend enough to 

# Add stochastic noise to a constant time series
  # look at papers for time-varying catchability 


# Other reporting for plaice? 
# focus on candidate models or hit highlights - major decision point, follow writeup outline, focus on candidates
