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

#----    Make    ----

# clean(destroy = TRUE)
make(plan)

readd(data)
loadd(data)

readd(table_freq_grade)
readd(table_freq_pub)
readd(table_freq_weeks)
readd(plot_effects_participants)
readd(table_freq_mot)

readd(fit_rma_mv)
loadd(fit_rma_mv)

fitloadd(plot_forest)
readd(plot_forest)

summary(fit_rma_mv)
fit_rma_mv$coef_test
fit_rma_mv$I_squared

drake_config(plan)$seed



loadd(sens_loo_summary)
loadd(sens_summary)
readd(plot_sens_summary)

matrixcalc::is.positive.definite(v)

#-------






