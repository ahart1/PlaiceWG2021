devtools::install_github("timjmiller/wham", dependencies=TRUE, ref="devel")
library(wham)
library(here)

mod = readRDS(paste(here::here(), "WG_Revised_Runs", "WHAM_Run29F-4_splitNEFSC-BigUnits-nlAgeComp-fix1", "WHAM_Run29F4_model.rds", sep="/"))
mod$env$data$XSPR_R_avg_yrs = (1:mod$env$data$n_years_model)[mod$years >2010] -1
mod$env$data$XSPR_R_opt = 2

mod_proj = wham:::prepare_projection(mod, proj.opts = list(use.FXSPR=TRUE))
#specify to use catch in first year of projections
mod_proj$data$proj_F_opt[1] = 5
mod_proj$data$proj_Fcatch[1] = 1000

mod_proj = fit_wham(mod_proj, do.retro=FALSE,do.osa=FALSE)
