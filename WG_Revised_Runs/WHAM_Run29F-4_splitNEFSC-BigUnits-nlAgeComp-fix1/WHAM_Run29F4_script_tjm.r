### Load R packages
library(tidyverse)
library(wham)
library(readxl)
library(DataExplorer)

### Load data
asap3 <- read_asap3_dat(paste(here::here(), "data", "PlaiceWHAM-2019_revised_NEFSC-LW-WAA_splitNEFSC-BigUnit.DAT", sep="/"))

### Preliminary run with freely estimated selectivity
##### Set up preliminary input with no selectivity parameters fixed
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

# fix_index_sel[[1]] <- c(4) # Fix age 4  for for index 1 (NEFSC spring Albatross)
# fix_index_sel[[2]] <- 5 # Fix age 4 & 5 for for index 2 (NEFSC spring Bigelow)
# fix_index_sel[[3]] <- c(11) # Fix age 4, 5, and 11 for for index 3 (NEFSC fall Albatross)
# fix_index_sel[[4]] <- 4 # Fix age 4  for for index 4 (NEFSC spring Bigelow)

init_fleet_sel <- list(c(2,0.4)) # logistic parameters, based on model type
init_index_sel <- lapply(1:use_n_indices, function(x) c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
# for(i in 1:4) init_index_sel[[i]][fix_index_sel[[i]]] <- 1 # replace initial values for 1 for the ages where selectivity fixed


# Setup random effect by selectivity block (here: fleet, index1, index2, index3, index4)
randeffect <- c(rep("iid", asap3$dat$n_fleet_sel_blocks), rep("none", 4)) # Don't include selectivity random effects for any surveys 

# Setup selectivity list
sel_list <- list(model = modelsetup, # list selectivity model for each fleet and index
                 re = randeffect,
                 initial_pars = c(init_fleet_sel, init_index_sel),
                 fix_pars = c(fix_fleet_sel, fix_index_sel))

#fix q at some safely large value
catchability <- list(
  initial_q = rep(2,use_n_indices))

input <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "WHAM_Run29F4", age_comp = "logistic-normal-miss0") # logistic normal age comp, 0s treated as missing

#fix the q values
input$map$logit_q = factor(rep(NA,use_n_indices)) #don't estimate q

##### Run model with freely estimated selectivity
WHAM_Run29F4 <- fit_wham(input, MakeADFun.silent = TRUE, do.osa = FALSE, do.retro=FALSE) 
check_convergence(WHAM_Run29F4)
print(paste("Number of parameters", length(WHAM_Run29F4$par), sep=" "))

# ID ages that should be fixed at full selectivity
#divide by max selectivity
t(sapply(WHAM_Run29F4$rep$selAA[2:5], function(x) x[1,]/max(x[1,])))
#WHAM_Run29F4$rep$selAA[2][[1]][1,] # Albatross spring, max sel = age 6
#WHAM_Run29F4$rep$selAA[3][[1]][1,] # Bigelow spring, max sel = age 5
#WHAM_Run29F4$rep$selAA[4][[1]][1,] # Albatross fall, max sel = age 4
#WHAM_Run29F4$rep$selAA[5][[1]][1,] # Bigelow fall, max sel = age 3

### Prepare model input for full run
#Fix only 1 age selectivity at 1 for each index based on highest estimated selectivity in preliminary run.
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

# fix_index_sel[[1]] <- c(4) # Fix age 4  for for index 1 (NEFSC spring Albatross) # From Tim's exploration
# fix_index_sel[[2]] <- c(5) # Fix age  5 for for index 2 (NEFSC spring Bigelow)
# fix_index_sel[[3]] <- c(11) # Fix age 11 for for index 3 (NEFSC fall Albatross)
# fix_index_sel[[4]] <- c(4) # Fix age 4  for for index 4 (NEFSC spring Bigelow)

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

input <- prepare_wham_input(asap3, NAA_re = NAA_re, selectivity = sel_list, model_name = "WHAM_Run29F4", age_comp = "logistic-normal-miss0") # logistic normal age comp, 0s treated as missing


### Save input
# Save model data input
#saveRDS(input, file=paste(here::here(), "WG_Revised_Runs", "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1", "WHAM_Run29F4_input.rds", sep="/"))

### Fit model, check convergence, and run diagnostics
#checking fit before OSA, retro
WHAM_Run29F4_basic <- fit_wham(input, MakeADFun.silent = TRUE, do.osa = FALSE, do.retro=FALSE) 
check_convergence(WHAM_Run29F4_basic)

# Run with OSA residuals
#input$par = WHAM_Run29F4_basic$parList #start at the optimized values (from run without OSA, retro) to save time
#WHAM_Run29F4 <- fit_wham(input, MakeADFun.silent = TRUE, do.osa = TRUE) 
#check_convergence(WHAM_Run29F4)
print(paste("Number of parameters", length(WHAM_Run29F4$par), sep=" "))

#plot_wham_output(mod=WHAM_Run29F4, out.type='html')
# plot_wham_output(mod=WHAM_Run29F4, out.type='png', dir.main = paste(here::here(), "WHAM_Run29F4", sep="/"))

### Save output
# Save fitted model
saveRDS(WHAM_Run29F4_basic, file=paste(here::here(), "WG_Revised_Runs", "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1", "WHAM_Run29F4_model_noosa_noretro_tjm.rds", sep="/"))


# ### Rerun model using saved input data
# # Load data from saved input RData and rerun model
# 
# inputRerun <- readRDS(paste(here::here(), "WG_Revised_Runs",
#                      "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1/WHAM_Run29F4_input.rds", sep="/"))
# 
# # Rerun data
# ReRun29F4 <- fit_wham(input = inputRerun, MakeADFun.silent = TRUE)


# ### Plot Bigelow:Albatross catchability for spring and fall indices
# Lines 110-123 borrowed from plot_q() function used to generate default q plots in WHAM
# ```{r}
# mod <- WHAM_Run29F4
# 
#   if("sdrep" %in% names(mod)){
#     if("q_re" %in% mod$input$random){
#       se = as.list(mod$sdrep, "Std. Error", report=TRUE)$logit_q_mat
#     }else{
#       se = t(matrix(as.list(mod$sdrep, "Std. Error")$logit_q, nrow = NCOL(mod$rep$logit_q_mat), 
#       ncol = NROW(mod$rep$logit_q_mat)))
#     }
#     logit_q_lo = mod$rep$logit_q_mat - qnorm(0.975)*se
#     logit_q_hi = mod$rep$logit_q_mat + qnorm(0.975)*se
#     ### Retransform out of logit space
#     q = t(mod$input$data$q_lower + (mod$input$data$q_upper - mod$input$data$q_lower)/(1+exp(-t(mod$rep$logit_q_mat))))
#     q_lo = t(mod$input$data$q_lower + (mod$input$data$q_upper - mod$input$data$q_lower)/(1+exp(-t(logit_q_lo))))
#     q_hi = t(mod$input$data$q_lower + (mod$input$data$q_upper - mod$input$data$q_lower)/(1+exp(-t(logit_q_hi))))
#   }
# 
# ### Constant q over time series so pick first line and plot 2 ways:
# q <- q[1,]
# q_lo <- q_lo[1,]
# q_hi <- q_hi[1,]
# 
# q_dat <- data.frame(q = q, q_lo = q_lo, q_hi = q_hi, index = c("Alb spring", "Big spring", "Alb fall", "Big fall"))
# 
# # Plot q value with confidence bounds
# ggplot(q_dat) + 
#   geom_bar(aes(x=index, y=q), stat="identity") + 
#   scale_x_discrete(limits = c("Alb spring", "Big spring", "Alb fall", "Big fall")) + 
#   geom_errorbar(aes(index, ymin = q_lo, ymax = q_hi), width = 0.4, colour = "orange", size = 1.3) +
#   ylim(0,0.00029)
# ggsave(filename = paste(here::here(), "WG_Revised_Runs/WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1/plots_png/q_barplot.png", sep="/"))
# 
# # Plot ratio of bigelow to albatross q values
# springRatio <- q_dat[which(q_dat$index == "Big spring"), "q"]/ q_dat[which(q_dat$index == "Alb spring"), "q"]
# fallRatio <- q_dat[which(q_dat$index == "Big fall"), "q"]/ q_dat[which(q_dat$index == "Alb fall"), "q"]
# 
# qRatio <- data.frame(qRatio = c(springRatio, fallRatio), Season = c("Spring", "Fall"))
# 
# ggplot() +
#   geom_bar(data = qRatio, aes(x=Season, y = qRatio), stat = "identity") +
#   ylim(0,1.5)
# ggsave(filename = paste(here::here(), "WG_Revised_Runs/WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1/plots_png/qRatio_barplot.png", sep="/"))
# ```
# 
# 
# ### Plot selectivity with CI 
# Borrowed code from plot.fleet.sel.blocks() for plot and par_tables_fn() for CI data
# -currently pull CI mannually from WHAM Output Tables but would be good to automate so we can loop over indices
# ```{r}
# dat <- WHAM_Run29F4$env$data
# ages = 1:dat$n_ages
# 
# # Plot index selectivity (fleet doesn't have age-specific CI since logistic selectivity)
# sb_p = dat$selblock_pointer_indices #selblock pointer by year and index
#   # sb_p = dat$selblock_pointer_fleets #selblock pointer by year and fleet
# 
# # Index 1
# i = 1
# blocks = unique(sb_p[,i])
# sel = do.call(rbind, lapply(WHAM_Run29F4$rep$selAA, function(x) apply(x,2,mean)))[blocks,,drop=FALSE]
# 
# data.frame(ages = ages, 
#            sel = c(sel), 
#            lowerCI = c(0.007, 0.127, 0.249, 0.074, 0.000, 1, 0.142, 0.262, 0.287, 0.287, 0.331),
#            upperCI = c(0.022,	0.383,	0.737,	0.998, 1.000, 1,	0.992,	0.854,	0.858,	0.891,	0.779)) %>%
#   ggplot() +
#   geom_line(aes(x=ages, y=sel)) + 
#   geom_ribbon(aes(x=ages,ymin=lowerCI, ymax=upperCI), alpha=0.2) + 
#   ggtitle("Index 1 Selectivity")
# ggsave(filename = paste(here::here(), "WG_Revised_Runs", "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1", "plots_png", "results", "Selectivity_index1_CI.png", sep="/"))
# 
# # Index 2
# i = 2
# blocks = unique(sb_p[,i])
# sel = do.call(rbind, lapply(WHAM_Run29F4$rep$selAA, function(x) apply(x,2,mean)))[blocks,,drop=FALSE]
# 
# data.frame(ages = ages, 
#            sel = c(sel), 
#            lowerCI = c(0.052, 0.217, 0.436, 0.057, 1, 0.488, 0.448, 0.421, 0.397, 0.373, 0.378),
#            upperCI = c(0.110, 0.441,	0.861,	1.000, 1, 0.937,	0.861,	0.829,	0.791,	0.753,	0.652)) %>%
#   ggplot() +
#   geom_line(aes(x=ages, y=sel)) + 
#   geom_ribbon(aes(x=ages,ymin=lowerCI, ymax=upperCI), alpha=0.2) + 
#   ggtitle("Index 2 Selectivity")
# ggsave(filename = paste(here::here(), "WG_Revised_Runs", "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1", "plots_png", "results", "Selectivity_index2_CI.png", sep="/"))
# 
# # Index 3
# i = 3
# blocks = unique(sb_p[,i])
# sel = do.call(rbind, lapply(WHAM_Run29F4$rep$selAA, function(x) apply(x,2,mean)))[blocks,,drop=FALSE]
# 
# data.frame(ages = ages, 
#            sel = c(sel), 
#            lowerCI = c(0.123, 0.352, 0.510, 1, 0.537, 0.481, 0.315, 0.292, 0.282, 0.356, 0.000),
#            upperCI = c(0.217,	0.598,	0.970, 1,	0.950, 0.821,	0.546,	0.516,	0.510,	0.686,	1.000)) %>%
#   ggplot() +
#   geom_line(aes(x=ages, y=sel)) + 
#   geom_ribbon(aes(x=ages,ymin=lowerCI, ymax=upperCI), alpha=0.2) + 
#   ggtitle("Index 3 Selectivity")
# ggsave(filename = paste(here::here(), "WG_Revised_Runs", "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1", "plots_png", "results", "Selectivity_index3_CI.png", sep="/"))
# 
# # Index 4
# i = 4
# blocks = unique(sb_p[,i])
# sel = do.call(rbind, lapply(WHAM_Run29F4$rep$selAA, function(x) apply(x,2,mean)))[blocks,,drop=FALSE]
# 
# data.frame(ages = ages, 
#            sel = c(sel), 
#            lowerCI = c(0.148, 0.194, 1, 0.027, 0.119, 0.200, 0.192, 0.180, 0.174, 0.170, 0.211),
#            upperCI = c(0.629,	0.873, 1,	0.999,	0.991, 0.918,	0.865,	0.847,	0.827,	0.970,	0.658)) %>%
#   ggplot() +
#   geom_line(aes(x=ages, y=sel)) + 
#   geom_ribbon(aes(x=ages,ymin=lowerCI, ymax=upperCI), alpha=0.2) + 
#   ggtitle("Index 4 Selectivity")
# ggsave(filename = paste(here::here(), "WG_Revised_Runs", "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1", "plots_png", "results", "Selectivity_index4_CI.png", sep="/"))
# ```
# 
# ## Comment
# The model converged when only a single age was fixed at full selectivity, and these ages overlapped with those fixed in runs 29F and 29F-2. Run 29F-4 had high uncertainty (CI range from at/near 0 to at/near 1) for Albatross spring age 5, Bigelow spring age 4, Albatross fall 11+ and Bigelow fall age 4. 
# 
# | Run   | Albatross spring | Bigelow spring | Albatross fall | Bigelow fall |
# | 29F   | 4,5,6            | 4,5            | 4,11           | 3,4,5        |
# | 29F-2 |   5,6            |   5            | 4,11           | 3,4          |
# | 29F-4 |     6            |   5            | 4              | 3            |
# 
# Catchability estimates are similar for run 29F-2 and 29F-4 but the CI for the Bigelow fall catchability estimate were much larger.
# 
# Run 29F-4 OSA residuals for fit to aggregate fleet and catch data were similarly or slightly less normally distributed than in run 29F-2 which could be attributed to the Albatross fall selectivity random effect that was not applied in run 29F-4. OSA residuals for fit to age comp data had similar patterns and distributions in run 29F-2 and 29F-4. 
