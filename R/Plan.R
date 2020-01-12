####################################
####    Plan of the Analysis    ####
####################################

#----    Plan    ----
plan <- drake_plan(
  #----    Data Preparation   -----
  
  # Load the dataset
  data_raw = load_dataset(),
  
  # Munge the dataset
  data_munged = munge_data(data_raw),
  
  # Compute Effect size
  data_effect = compute_dppc2(data_munged),
  
  # Remove 2° effect Hung et al.
  data = rm_hung(data_effect),
  
  #----    Descriptive Statistics    ----
  
  # Table n effects for studies
  table_n_effects_studies = n_effects_studies(data),
  
  # Plot n studies according to publication year
  plot_publication_year = publication_year(data),
  
  # Frequecies studies according to publication, school-grade
  table_freq = target(freq_table(data, var_name = ddd),
                      transform = map(ddd=c(pub,grade))),
  
  # Frequencies studies according  to weeks
  table_freq_weeks = freq_table_weeks(data),
  
  # Plot participants by studies
  plot_participants_studies = participants_studies(data),
  
  # Plot effect according to participants
  plot_effects_participants = effects_participants(data),
  
  # Table different motivation outcomes measured
  table_freq_mot = freq_table_mot(data),
  
  #----    Meta-analysis    ----
  
  # model fit
  fit_rma_mv = rma_multilevel(data,r_pre_post="r_mediumh",r_outocomes=.5),
  
  # forest plot
  
  plot_forest = forest_plot(fit_rma_mv),
  
  #----    Sensitivity correlations    ----
  
  # Compute all possible combinations
  sens_fit_rma_mv = target(
    rma_multilevel(data,r_pre_post=r_pre_post_value,r_outocomes=r_outcomes_value),
    # Define an analysis target for each combination of
    # r_pre_post_value and r_outcomes_value.
    transform = cross( 
      r_pre_post_value = c("r_high","r_mediumh","r_mediuml", "r_low"),
      r_outcomes_value = !!(seq(.1,.9,by=.1)) # Why `!!`? See "Tidy Evaluation"
    )),
  
  # Get results for each combination
  summary_sens = target(
    summarize_fit_rma_mv(sens_fit_rma_mv),
    transform = map(sens_fit_rma_mv)
  ),
  
  # Summarize 
  sens_summary = target(
    dplyr::bind_rows(summary_sens),
    transform = combine(summary_sens) 
  ),
  
  # Plot results sensitivity
  
  plot_sens_summary = sens_summary_plot(sens_summary),
  
  #----    Sensitivity leave-one-out    ----
  
  # Compute fit excluding one study at time
  fit_rma_loo = target(
    rma_multilevel(data, excluded_study= excluded_study_value),
    # Define an analysis target for each combination of
    # r_pre_post_value and r_outcomes_value.
    transform = map(excluded_study_value=!!seq(1,19, by=1))
    ),
  
  # Get results for each combination
  summary_sens_loo = target(
    summarize_fit_rma_mv(fit_rma_loo),
    transform = map(fit_rma_loo)
  ),
  
  # Summarize 
  sens_loo_summary = target(
    dplyr::bind_rows(summary_sens_loo),
    transform = combine(summary_sens_loo) 
  ),
  
  # Plot summary sensitivity loo
  plot_sens_loo = sens_loo_plot(sens_loo_summary, data),
  
  # Cook distances
  sens_cook_summary = sens_cook(fit_rma_mv),
  
  # Cook plot
  plot_cook=sens_cook_plot(sens_cook_summary),
  
  
  #----    Moderator-Analysis    ----
  
  # Fit separate multilevel meta-analysis for each moderator
  mod_rma_mv = target(
    rma_multilevel_mod(data,r_pre_post="r_mediumh",r_outocomes=.5,
                       moderator = moderator_value),
    # Define an analysis target for each moderator
    transform = map(moderator_value=c("pub","grade","weeks","intensity",
                                      "device", "mot")
    )),
  
  # Test of moderator
  test = target(
    anova(mod_rma_mv),
    transform = map(mod_rma_mv))
  
  )


