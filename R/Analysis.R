#######################
####   Analysis    ####
#######################

#----    Load    -----

source("R/Settings.R")  # loads packages
source("R/Functions.R") # defines functions
source("R/Auxiliary_functions.R") # Auxiliary functions
source("R/Plan.R")      # load analysis plan

#----    Check   -----

# Configure the analysis plan
config <- drake::drake_config(plan)

# Plot the analysis plan
vis_drake_graph(config, font_size = 16, targets_only = T)

#----    Make    ----

# Delate the analysis results
# drake::clean(destroy = TRUE)

# Run the analysis
make(plan)

# Plot the analysis plan
vis_drake_graph(config, font_size = 16, targets_only = T)

#----    Load elements    ----
# With drake you load the results with the function:
# - loadd() to load the object in the environment
# - readd() print the object

# Data preparation
loadd(data_raw)
loadd(data_munged)
loadd(data_effect)
loadd(data)

# Descriptive statistics
readd(table_n_effects_studies)
readd(plot_publication_year)
readd(table_freq_pub)
readd(table_freq_grade)
readd(table_freq_math_area)
readd(table_freq_device)
readd(table_freq_weeks)
readd(table_freq_dependence)
readd(plot_participants_studies)
readd(plot_effects_participants)
readd(table_freq_mot)

# Multilevel meta-analysis
loadd(fit_rma_mv)
summary(fit_rma_mv)   # summary
fit_rma_mv$coef_test  # Coefficient test with correction
fit_rma_mv$I_squared  # I squared
readd(plot_forest)

# Romubeta analysis
loadd(fit_robumeta)
print(fit_robumeta) # summary

# Meta-analysis data_aggregated
loadd(data_aggregated)
loadd(fit_rma)
summary(fit_rma)      # summary

# Sensitivity correlations
loadd(sens_summary)
readd(table_sens_summary)
loadd(plot_sens_summary)
plot(plot_sens_summary)

# Sensitivity leave-one-out
loadd(sens_loo_summary)
loadd(table_loo_summary)
readd(plot_sens_loo)
loadd(sens_cook_summary)
readd(plot_cook)

# Publication-bias
funnel(fit_rma_mv)
loadd(trim_fill_aggregated)
summary(trim_fill_aggregated)
funnel(trim_fill_aggregated)
loadd(egger_regression_N)
summary(egger_regression_N)
loadd(egger_regression_vi_dppc2)
summary(egger_regression_vi_dppc2)
loadd(rank_test)
print(rank_test)

# Moderator-Analysis
loadd(mod_rma_mv_pub)
summary(mod_rma_mv_pub)
loadd(mod_rma_mv_grade)
summary(mod_rma_mv_grade)
loadd(mod_rma_mv_weeks)
summary(mod_rma_mv_weeks)
loadd(mod_rma_mv_intensity)
summary(mod_rma_mv_intensity)
loadd(mod_rma_mv_math_area)
summary(mod_rma_mv_math_area)
loadd(mod_rma_mv_device)
summary(mod_rma_mv_device)
loadd(mod_rma_mv_mot)
summary(mod_rma_mv_mot)

loadd(test_mod_rma_mv_pub)
print(test_mod_rma_mv_pub)
loadd(test_mod_rma_mv_grade)
print(test_mod_rma_mv_grade)
loadd(test_mod_rma_mv_weeks)
print(test_mod_rma_mv_weeks)
loadd(test_mod_rma_mv_intensity)
print(test_mod_rma_mv_intensity)
loadd(test_mod_rma_mv_math_area)
print(test_mod_rma_mv_math_area)
loadd(test_mod_rma_mv_device)
print(test_mod_rma_mv_device)
loadd(test_mod_rma_mv_mot)
print(test_mod_rma_mv_mot)

loadd(table_moderator_analysis)
print(table_moderator_analysis)

#-------


#############################
###    Extra analysis    ####
#############################

loadd(data)

#----    multilevel with 2 random effects    ----

data%>%
  filter(r_size=="r_mediumh")%>%
  rma.mv(yi_dppc2,vi_dppc2, random =  list(~ 1|study,
                                           ~ 1|id_effect), 
      method = "REML", data = ., slab=author_y)


#----    moderator analysis math_area dropping geometry    ----

rma_multilevel(data[data$math_area!="geometry",],r_pre_post="r_mediumh",r_outocomes=.5,
               moderator = ~math_area)
