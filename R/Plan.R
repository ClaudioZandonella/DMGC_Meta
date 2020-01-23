####################################
####    Plan of the Analysis    ####
####################################

#----    Plan    ----
plan <- drake_plan(
  #----    Data Preparation   -----
  
  # Load the dataset
  data_raw = load_dataset(file_path="Data/Dataset_01_23_2020.csv"),
  
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
  
  # Frequencies studies according to publication, school-grade, math_area, device, dependence
  table_freq = target(freq_table(data, var_name = ddd),
                      transform = map(ddd=c(pub,grade,math_area, device,dependence))),
  
  # Frequencies studies according  to weeks
  table_freq_weeks = freq_table_weeks(data),
  
  # Plot participants by studies
  plot_participants_studies = participants_studies(data),
  
  # Plot effect according to participants
  plot_effects_participants = effects_participants(data),
  
  # Table different motivation outcomes measured
  table_freq_mot = freq_table_mot(data),
  
  #----    Multilevel meta-analysis    ----

  # model fit
  fit_rma_mv = rma_multilevel(data,r_pre_post="r_mediumh",r_outocomes=.5),
  
  # forest plot
  
  plot_forest = forest_plot(fit_rma_mv),

  #-----   Robumeta_analysis    ----
  
  # Robust Variance Analysis results
  fit_robumeta = rva_meta(data),

  #----    Meta-analysis data_aggregated    ----
  
  # Obtain data with aggregate effect sizes using Borenstein formula
  data_aggregated = aggregate_data(data, cor = .5, method = "BHHR"),
  
  # Random-effect meta-analysis with aggregated effects
  fit_rma = rma(yi = yi_dppc2, vi = vi_dppc2, method = "REML",
                data = data_aggregated, slab = author_y),
  
  
  
  
  #----    Sensitivity correlations    ----
  
  # Fit multilevel meta-analysis wit all possible combinations
  # of the correlation pre-post test and correlation between outcomes
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
  
  # Summarize results sensitivity correlations analysis
  sens_summary = target(
    dplyr::bind_rows(summary_sens),
    transform = combine(summary_sens) 
  ),
  
  # Get min, max and mean of the results
  table_sens_summary = table_sens_results(sens_summary),
  
  # Plot results sensitivity correlations analysis
  plot_sens_summary = sens_summary_plot(sens_summary),
  
  
  #----    Sensitivity leave-one-out    ----
  
  # Fit multilevel meta-analysis excluding one study at time
  fit_rma_loo = target(
    rma_multilevel(data, excluded_study= excluded_study_value),
    # Define an analysis target for each study removed (excluded_study_value)
    transform = map(excluded_study_value=!!seq(1,19, by=1))
    ),
  
  # Get results for each study removed
  summary_sens_loo = target(
    summarize_fit_rma_mv(fit_rma_loo),
    transform = map(fit_rma_loo)
  ),
  
  # Summarize results sensitivity loo
  sens_loo_summary = target(
    dplyr::bind_rows(summary_sens_loo),
    transform = combine(summary_sens_loo) 
  ),
  
  # Get min, max and mean of the results
  table_loo_summary = table_sens_results(sens_loo_summary),
  
  # Plot summary sensitivity loo
  plot_sens_loo = sens_loo_plot(sens_loo_summary, fit_rma_mv, data),
  
  # Cook's distances
  sens_cook_summary = sens_cook(fit_rma_mv),
  
  #  Plot Cook's distances
  plot_cook=sens_cook_plot(sens_cook_summary),
  
  
  #----    Publication-bias    ---- 
  
  # Funnel plot multilevel meta-analysis
  funnel_plot = funnel(fit_rma_mv),
  
  # Egger's regression test considering study sample size or 
  # variance effect as predictors
  
  egger_regression = target(
    rma_multilevel(data, r_pre_post = "r_mediumh",r_outocomes = .5,
                   moderator = moderator_value),
    # Define an analysis target for each moderator
    transform = map(moderator_value = c("N", "vi_dppc2"))
  ),
  
  # Rank correlation test  observed outcomes and the corresponding sampling variances
  rank_test = ranktest(fit_rma_mv),
  
  # Funnel plot meta-analysis aggregated effect with trim-and-fill
  trim_fill_aggregated = trimfill(fit_rma),
  
  funnel_plot_aggregated = funnel(trim_fill_aggregated),
  
  
  
  #----    Moderator-Analysis    ----
  
  # Fit separate multilevel meta-analysis for each moderator
  mod_rma_mv = target(
    rma_multilevel(data,r_pre_post="r_mediumh",r_outocomes=.5,
                       moderator = moderator_value),
    # Define an analysis target for each moderator
    transform = map(moderator_value=c("pub","grade","weeks","intensity",
                                      "math_area","device", "mot")
    )),
  
  # Test of moderator
  test = target(
    anova(mod_rma_mv),
    transform = map(mod_rma_mv)),
  
  # Table with summary results
  
  table_moderator_analysis = summary_moderator_analysis(
    mod_rma_mv_pub,mod_rma_mv_grade,mod_rma_mv_weeks,
    mod_rma_mv_intensity,mod_rma_mv_math_area,
    mod_rma_mv_device,mod_rma_mv_mot
  )
  
  #----    Report Analysis    ----
  #report_analysis = knitr_in("Report_analysis/Report_analysis.Rnw")
  )


