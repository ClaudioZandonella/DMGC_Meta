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
  
  # Plot participants by studies
  plot_participants_studies = participants_studies(data),
  
  # Plot effect according to participants
  plot_effects_participants = effects_participants(data)
)

