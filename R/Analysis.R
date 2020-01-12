#######################
####   Analysis    ####
#######################

#----    Load    -----

source("R/Settings.R")  # loads packages
source("R/Functions.R") # defines functions
source("R/Plan.R")      # creates the drake plan
source("R/Auxiliary_functions.R") # Auxiliary functions


#----    Check   -----

config <- drake_config(plan)
vis_drake_graph(config, font_size = 16, targets_only = T)
#drake_ggraph(config, targets_only = T)

#----    Make    ----

# clean(destroy = TRUE)
make(plan)


loadd(data)
loadd(fit_rma_mv)
loadd(mod_rma_mv_pub)
loadd(mod_rma_mv_grade)
loadd(mod_rma_mv_weeks)
loadd(mod_rma_mv_intensity)
loadd(mod_rma_mv_device)
loadd(mod_rma_mv_mot)

loadd(egger_regression_N)
loadd(egger_regression_vi_dppc2)

sqrt(data$vi_dppc2)
AIC(mod_rma_mv_mot,fit_rma_mv)
#-------






