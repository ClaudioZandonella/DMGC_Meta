#######################
####   Analysis    ####
#######################

#----    Load    -----

source("R/Settings.R")  # loads packages
source("R/Functions.R") # defines functions
source("R/Plan.R")      # creates the drake plan


#----    Check   -----

config <- drake_config(plan)
vis_drake_graph(config, font_size = 16)

#----    Make    ----

# clean(destroy = TRUE)
make(plan)

readd(data)
loadd(data)

readd(table_n_effects_studies)

drake_config(plan)$seed
