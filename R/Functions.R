
#########################
####    Functions    ####
#########################

################################
####    Data preparation    ####
################################

#----    load_dataset function   ----

load_dataset <- function(){
#---- load_dataset ----
  data_raw <- read.csv("Data/Dataset.csv",sep=";", header = T, stringsAsFactors = F)
#----
  return(data_raw)
}

#----    clean_data   ----

clean_data <- function(data){
  data%>% 
    # Define which variable are factor
    mutate_at(vars("study","id","pub","dependence","grade","device","mot"), factor)%>% 
    # Redefine factor labels for grade and device
    mutate(grade=recode_factor(grade,"1"="Primary","2"="Secondary"),
           device=recode_factor(device,"app"="App","con"="Console","pc"="PC"),
           mot=recode_factor(mot, "1"="expectancy","2"="value"),
           # Obtain authors (year) label
           author_y=paste0(author," (",year,")"),
           # Compute intensity of the intervention
           intensity = sessions*minutes/weeks)%>%
    select("study":"minutes","intensity",everything())
}

#----    pareto_post function   ----

# Pareto et al (2011) does not report post-test scores but 
# gain scores (mean and sd) for the two effects:

# To obtain the post-test scores we have:
# m_t2_* = m_t1_* + m_gain_*
# sd_t2_* = sqrt(sd_t1_*^2 + sd_gain_*^2)
# Because variances are independent
# (* stands for control group/experimental group)

#----    pareto_post    ----
pareto_post <- function(data){
  # Control group gain 
  m_gain_cg<-c(1.5,-.23)    # Mean gain
  sd_gain_cg<-c(3.82,3.62)  # Sd gain
  
  # Experimental group gain
  m_gain_eg<-c(.93,1.44)    # Mean gain
  sd_gain_eg<-c(3.09,3.52)  # Sd gain
  
  data %>%
    mutate_cond(study=="11",  # Change only rows satisfying the condition
                # Control group
                m_t2_cg = m_t1_cg + m_gain_cg,
                sd_t2_cg = sqrt(sd_t1_cg^2+sd_gain_cg^2),
                #Experimental group
                m_t2_eg = m_t1_eg + m_gain_eg,
                sd_t2_eg = sqrt(sd_t1_eg^2+sd_gain_eg^2))
  }

#----    compute_N_study    ----

# Compute the total number of participants in the studies

compute_N_study <-  function(data){
 
  # Ke(2006) - independent groups
  N_ke2006 = data%>%
    filter(id=="961" & !duplicated(id_effect))%>%
    select(n_cg,n_eg)%>%
    sum()
  
  # Ke(2008) - multiple experimental groups 
  N_ke2008 = data%>%
    filter(id=="432" & !duplicated(id_effect))%>%
    select(n_cg,n_eg)%>%
    summarise(n_cg=n_cg[1],
              n_eg=sum(n_eg))%>%
    sum()
  
  data%>%
    mutate(N_study = N)%>%
    mutate_cond(id=="961", N_study=N_ke2006)%>%
    mutate_cond(id=="432", N_study=N_ke2008)
}

#----    munge_data   ----

munge_data <- function(data){
  
  data %>%
    clean_data() %>%
    pareto_post() %>%
    compute_N_study()
  
}

#----    compute_dppc2 function    ----

## Compute effect size according to Morris (2008) we used the index dppc2
## https://journals.sagepub.com/doi/10.1177/1094428106291059

## We used the same formulas reported in:
## http://www.metafor-project.org/doku.php/analyses:morris2008

# To compute the variance we need the correlation between pre- and post-test scores
# correlation are not reported so we try with different values
# high correlation (.8), medium-high correlation (.6),
# medium-low correlation (.4), and low correlation (.2)

#----    compute_dppc2    ----
compute_dppc2 <- function(data){
  
  data%>%
    mutate(
      # compute pooled standard deviation
      sd_pool= sqrt(((n_eg-1)*sd_t1_eg^2+(n_cg-1)*sd_t1_cg^2)/(n_eg+n_cg-2)),
      # Compute yi_dppc2
      yi_dppc2 = metafor:::.cmicalc(n_eg+n_cg-2)*((m_t2_eg - m_t1_eg) - (m_t2_cg-m_t1_cg))
                                    /sd_pool,
      # add correlations pre-post 
      r_high=.8,      # high correlation
      r_mediumh=.6,   # medium-high correlation
      r_mediuml=.4,   # medium-low correlation
      r_low=.2)%>%    # low correlation
    
    # Rearrange dataset from wide format to long format
    gather("r_high":"r_low", key="r_size",value="r_value", factor_key = TRUE)%>%  
    # compute variance
    mutate(vi_dppc2=2*(1-r_value)*(1/n_eg + 1/n_cg)+ yi_dppc2^2/(2*(n_eg + n_cg)))%>%
    arrange(study,n_effect,r_size)  # ordering dataset
}
#----    rm_hung function    ----

# We remove the second effect in Hung et al. (2014) because evident problems.
# In the measure of the pre-test sd is very low (.02 and .00).
# In turns, this lead to an implausible effect of 38
# That is probably given by the fact that the measure used was not able to
# evaluate properly the variation among individuals (floor effect)

#----    rm_hung    ----
rm_hung <- function(data){
  data%>%
    filter(study!= "13" | n_effect!=2)
  }

#----    aggregated_data_function   ----

# Obtain dataset with aggregate effects within studies 
# using MAd::agg function and setting method="BHHR" raccomanded by 
# Hoyt & Del Re (2017): https://doi.org/10.1080/10503307.2017.1405171

#----    aggregated_data    ----

aggregate_data = function (data, r_pre_post="r_mediumh", cor=.5,
                           method="BHHR"){
  selected_data = data%>%
    filter(r_size==r_pre_post)
  
  # Aggregate effects
  agg_effects = MAd::agg(id = study, es = yi_dppc2, var = vi_dppc2,
                         cor=cor, n.1=n_eg, n.2=n_cg, 
                         method=method, data=selected_data)
  
  # Arrenge dataset to keep only useful variables
  data_aggregated = selected_data%>%
    group_by(study)%>%
    mutate(N=round(mean(N),0),
           n_cg=round(mean(n_cg),0),
           n_eg=round(mean(n_eg),0))%>%
    filter(!duplicated(study))%>%
    left_join(.,agg_effects, by= "id")%>%
    mutate(yi_dppc2=es,
           vi_dppc2=var)%>%
    select("study":"n_eg","author_y","yi_dppc2":"vi_dppc2")
  
  return(data_aggregated)
}
#----



######################################
####    Descriptive Statistics    ####
######################################

####    Study level    ####

#----    table_n_studies_effects    ----

# Obtain the frequency table of number of effects for studies

n_effects_studies <- function(data){
  data%>%
    filter(!duplicated(id_effect))%>%
    select(study)%>%
    table()%>%table()%>%
    as.data.frame()%>%
    select("Freq",".")%>%
    rename("n_studies"="Freq","n_effects"=".")
}

#----    plot_publication_year    ----

# Plot the frequency of studies by year

publication_year <- function(data){
  data%>%
    filter(!duplicated(id))%>%
    ggplot()+
    geom_bar(aes(x=year), col="black", fill="lightblue")+
    scale_x_continuous(breaks = seq(from=2005, to=2018, by=1))+
    theme(panel.grid.minor = element_blank())+
    xlab("Year of publication")+
    ylab("Number of studies")
}

#----    freq_table    ----

# Frequency table of studies in given level of variable 
# Used for publication, school-grade 

freq_table <- function(data, var_name){
  var_name = enquo(var_name)
  data%>%
    filter(!duplicated(id))%>%
    group_by(!!var_name)%>%
    count()%>%
    ungroup()

}

#----    freq_table_weeks   ----

# Frequency table of studies in given level of variable
#  Used for weeks 

freq_table_weeks <- function(data){
  data%>%
    filter(!duplicated(id))%>%
    mutate(week_groups=if_else(weeks==1, "1 week",
                               if_else(weeks<=4, "4 weeks or less",
                                       if_else(weeks<=10,"10 weeks or less","more than 10 weeks"))),
           week_groups=factor(week_groups,levels=c("1 week","4 weeks or less","10 weeks or less","more than 10 weeks")))%>%
    group_by(week_groups)%>%
    count()%>%
    ungroup()
  }

#----    plot_participants_studies -----

# Plot number of participants for each study

participants_studies <- function(data){
  
  data%>%
    filter(!duplicated(id))%>%
    mutate(author_y=reorder(author_y,N_study))%>%
    ggplot()+
    geom_bar(aes(x=author_y, y=N_study),col="black", fill="lightblue",stat="identity")+
    scale_y_continuous(breaks = seq(from=0, to=1200, by=150))+
    theme(axis.text.x = element_text(angle = 45, hjust=1))+
    xlab("Study")+
    ylab("Sample Size")
    
}


####    Effects level    ####
#----    plot_effects_participants    ----

# Plot effects according to number of subjects

effects_participants <- function(data){
  my_breaks = c(50, 100, 200, 400,1000)
  data%>%
    filter(r_size=="r_mediumh")%>%
    mutate(author_y=reorder(author_y,N_study),
           SE =1.96*sqrt(vi_dppc2))%>%
    ggplot()+
    geom_point(aes(x=author_y, y=yi_dppc2, col=N_study),position= position_dodge2(.5))+
    geom_errorbar(aes(ymin=yi_dppc2-SE, ymax=yi_dppc2+SE,x=author_y, col=N_study/2),
                  position= position_dodge2(.5), width=.5)+
    scale_y_continuous(breaks = seq(from=-.5, to=1, by=.25))+
    scale_color_gradient(name = "Sample Size", trans = "log",
                         breaks = my_breaks, labels = my_breaks,
                         low="lightblue", high="darkblue")+
    scale_size_continuous(guide=FALSE)+
    xlab("Studies (ordered by sample size)")+
    ylab("Estimated Effect")+
    theme(legend.position = c(.9,.83))+
    coord_flip()
  
}

#----    freq_table_mot    ----

# Frequency table of studies in given level of variable
#  Used for weeks

freq_table_mot <- function(data){
  data%>%
    filter(!duplicated(id_effect))%>%
    group_by(mot)%>%
    count()%>%
    ungroup%>%
    mutate(motivation = c("expectation","value","not differentiated"))%>%
    select(motivation,n)
}





#############################
####    Meta Analysis    ####
#############################


#----    gleser2009    ----

# Function to compute variance-covariance matrix when multiple treatment groups
# are compared to the same control group:
# http://www.metafor-project.org/doku.php/analyses:gleser2009
# (section "Quantitative Response Variable")

calc_vcv <- function(data) {
  Ni <- rep(sum(data$n_eg) + data$n_cg[1], each=nrow(data))
  v <- matrix(1/data$n_cg[1] + outer(data$yi_dppc2, data$yi_dppc2, "*")/(2*Ni[1]), nrow=nrow(data), ncol=nrow(data))
  diag(v) <- data$vi_dppc2
  v
}

#----    compute_vcv_matrix_function    ----

# Compute variance covariance matrix for given correlation value
# according to the type of dependecy between outcomes

# Effect sizes in Ke (2008) are computed with different treatment group but same control group
# Gleser & Olkin (2009) provide formula to compute variance of Cohen's d considering
# this dependence (see "gleser2009" section). However, formula are unknown for dppc2.
# Thus, Ke (2008) effects are considered correlated as multiple outcomes varying correlation.

# To compute values using Gleser & Olkin (2009) formula uncomment the following line
# cov_dppc2[[3]] = calc_vcv(data[data$study==3,])

#----    compute_vcv_matrix    ----
compute_vcv_matrix <- function(data, r=.5){
  
  # Considering all effects correlated
  cov_dppc2= with(data, clubSandwich::impute_covariance_matrix(vi = vi_dppc2, 
                                                               cluster =study, r = r))
  
  # Effect sizes in Ke (2006) are independent so we set covariances to 0
  cov_dppc2[["2"]] = cov_dppc2[["2"]]*diag(1,3,3) # Study id is "2"
  
  return(cov_dppc2)
}

#----    rma_multilevel_function    ----

# Mulitlevel meta-analysis

#----    rma_multilevel    ----
rma_multilevel <-  function(data, r_pre_post="r_mediumh", r_outocomes=.5, excluded_study=NULL, moderator=NULL){
  
  # Filter data
  data = data%>%
    filter(r_size==r_pre_post)
  
  # compute variance-covariance matrix
  cov_dppc2 = compute_vcv_matrix(data, r=r_outocomes)
  
  # check if study have to be excluded (for the sens_loo)
  if(!is.null(excluded_study)){
    # remove study from variance-covariance matrix
    cov_dppc2 = compute_vcv_matrix(data, r=.5)[-excluded_study]
    # remove study from data
    data = data%>%
      filter(study!=excluded_study)
  }
  
  # Get mod formula
  if (is.null(moderator)){
    mod_formula = NULL
  }else{
    mod_formula = as.formula(paste0("~",moderator))
  }
  
  # multilevel meta-analysis
  fit_rma_mv = rma.mv(yi = yi_dppc2, V = cov_dppc2, random =  ~ 1|study, 
                      mod = mod_formula, method = "REML", data = data, slab=author_y)
  
  # add useful information
  fit_rma_mv$I_squared = I_squared(fit_rma_mv)
  fit_rma_mv$coef_test = coef_test(fit_rma_mv, cluster = data$study, vcov = "CR2")
  fit_rma_mv$r_pre_post=r_pre_post
  fit_rma_mv$r_outocomes=r_outocomes
  fit_rma_mv$data = data
  fit_rma_mv$excluded_study = excluded_study
  
  return(fit_rma_mv)
}

#----    forest_plot    ----

# Forest plot multilevel meta-analysis

forest_plot <- function(fit_rma_mv){
  
  data =fit_rma_mv$data
  
  labels <- data %>%
    transmute(autor_y= if_else(n_effect==1,true=author_y,false="-"),
              n_effect, N,
              ci = paste0("[",format(round(yi_dppc2-1.96*sqrt(vi_dppc2),2),2),
                          ";",format(round(yi_dppc2+1.96*sqrt(vi_dppc2),2),2),"]"),
              yi_dppc2 = format(round(yi_dppc2, 2),2))
  
  par(mar = c(4, 0, 0, 0))
  forest(fit_rma_mv, slab =labels$autor_y, xlim=c(-3,3),ilab=labels[,-1],
         ilab.pos=2, ilab.xpos=c(-1,1.8,3,2.3), annotate = F)
  
  text(-3,44, "Author(s) Year",  pos=4,font=2)
  text(-1.1,44, "Effect",font=2)
  text(3,44,"[95\\% CI]",font=2,pos=2)
  text(3,-1,paste0("[",round(fit_rma_mv$ci.lb,2),";",
                   round(fit_rma_mv$ci.ub,2),"]"), pos=2)
  text(2.3,44,expression(d[ppc2]),font=2, pos=2)
  text(2.3,-1,round(fit_rma_mv$b,2),pos=2)
  text(1.6,44,"Sample Size",font=2)
  
  p <- recordPlot()
  
  return(p)
}



#----    rva_meta    ----
rva_meta <- function(data){
  # Robust Variance Analysis
  robumeta::robu(yi_dppc2~1, data=data%>%filter(r_size=="r_mediumh"), 
                 modelweights = "CORR", studynum = study,
                 var.eff.size = vi_dppc2, small=TRUE, rho=.5)
}
#----
########################################
####    Sensitivity correlations    ####
########################################

#----    summarize_fit_rma_mv    ----

# summarize info about mulitlevel meta analysis

summarize_fit_rma_mv<- function(fit_rma_mv){
  
  conf_int_sigma = confint(fit_rma_mv)$random[2,]
  
  summary_fit_rma_mv = data.frame(
    model = deparse(substitute(fit_rma_mv)),
    r_pre_post = fit_rma_mv$r_pre_post,
    r_outcomes = fit_rma_mv$r_outocomes,
    
    sigma2 = fit_rma_mv$sigma2,
    sigma=conf_int_sigma[1],
    sigma_lb=conf_int_sigma[2],
    sigma_ub=conf_int_sigma[3],
    
    QE=fit_rma_mv$QE,
    I_squared=fit_rma_mv$I_squared,
    
    fit_rma_mv$coef_test,
    stringsAsFactors = F)
  
  rownames(summary_fit_rma_mv)=NULL
  
  if(!is.null(fit_rma_mv$excluded_study)){
    summary_fit_rma_mv$excluded_study = fit_rma_mv$excluded_study
    
    summary_fit_rma_mv = summary_fit_rma_mv%>%
      select("model","excluded_study", everything())
  }
  
  return(summary_fit_rma_mv)
}


#----    table_sens_results    ----

# Compute min, max, an mean value of the results of the sensitivity analysis

table_sens_results <- function(sens_summary){
  col_names = c("sigma2","sigma","sigma_lb","sigma_ub","QE","I_squared","beta","SE","beta_lb","beta_ub")
  
  sens_summary=cbind(sens_summary, 
                     compute_CI(estimate = "beta", SE = "SE", data=sens_summary))
  
  min = apply(sens_summary[,col_names],2,min)
  max = apply(sens_summary[,col_names],2,max)
  mean = apply(sens_summary[,col_names],2,mean)
  names = names(sens_summary[,col_names])
  
  res = data.frame(names,min,mean,max, row.names = NULL)
  return(res)
}

#----    sens_summary_plot    ----

# Plot summary information about the sensitivity analysis

sens_summary_plot <- function(sens_summary){
  sens_summary = sens_summary%>%
    mutate(beta_lb = beta-1.96*SE,
           beta_ub = beta+1.96*SE,
           r_pre_post=factor(r_pre_post, levels = c("r_high","r_mediumh","r_mediuml","r_low")))
  
  legend_title= "Correlation pre-post test"
  legend_labels=c("high .8","medium-high .6","medium-low .4","low .2")
 
  p1 = ggplot(sens_summary)+
 #   geom_errorbar(aes(x=r_outcomes, ymin=sigma_lb, ymax=sigma_ub, col=r_pre_post))+
    geom_point(aes(x=r_outcomes, y=sigma, col=r_pre_post, shape=r_pre_post))+
    geom_line(aes(x=r_outcomes, y=sigma, col=r_pre_post,linetype=r_pre_post))+
    scale_x_continuous(breaks = seq(from=0.1, to=.9, by=.1))+
    scale_color_discrete(name = legend_title, labels = legend_labels)+
    scale_shape_discrete(name = legend_title, labels = legend_labels)+
    scale_linetype_discrete(name = legend_title, labels = legend_labels)+
    labs(x="Correlation between outcomes",
         y="$\\sigma$",
         legend="Correlation pre-post score")+
    theme(legend.position = "top")
  
  p2 = ggplot(sens_summary)+
 #   geom_errorbar(aes(x=r_outcomes, ymin=beta_lb, ymax=beta_ub, col=r_pre_post))+
    geom_point(aes(x=r_outcomes, y=beta, col=r_pre_post, shape=r_pre_post))+
    geom_line(aes(x=r_outcomes, y=beta, col=r_pre_post,linetype=r_pre_post))+
    scale_x_continuous(breaks = seq(from=0.1, to=.9, by=.1))+
    labs(x="Correlation between outcomes",
         y="$d_{ppc2}$")+
    theme(legend.position = "none")
  
  p3 = ggplot(sens_summary)+
    geom_point(aes(x=r_outcomes, y=I_squared, col=r_pre_post, shape=r_pre_post))+
    geom_line(aes(x=r_outcomes, y=I_squared, col=r_pre_post, linetype=r_pre_post))+
    scale_x_continuous(breaks = seq(from=0.1, to=.9, by=.1))+
    scale_y_continuous(breaks = c(60,70,80,90),labels = c("60\\%","70\\%","80\\%","90\\%"))+
    labs(x="Correlation between outcomes",
         y="$I^2$")+
    theme(legend.position = "none")
  
  legend = get_legend(p1)
  
  gridExtra::grid.arrange(p1+theme(legend.position = "none"),
               p2,p3,legend,
               ncol=1, heights=c(6,6,6,1))
}



#########################################
####    Sensitivity leave-one-out    ####
#########################################

#----    sens_loo_plot    ----

# Plot results of sensitivity leave one out analysis

sens_loo_plot <- function(sens_loo_summary, fit_rma_mv, data, label_I_squared = "$I^2$",
                          label_dppc2 = "$d_{ppc2}$", label_ci = "95\\%CI"){
  
  # Add original results when all studies are considered
  original_results = summarize_fit_rma_mv(fit_rma_mv)
  
  sens_loo_summary%>%
    bind_rows(original_results)%>%
    mutate(author_y =c(unique(data$author_y),"Original result"),
           lab_I_squared=paste0(round(I_squared,0),"\\%"),
           lab_beta=round(beta,2),
           lab_ci_lb=beta-1.96*SE,
           lab_ci_ub=beta+1.96*SE,
           lab_ci=paste0("[",format(round(lab_ci_lb,2),2),
                         "; ",format(round(lab_ci_ub,2),2),"]"))%>%
    ggplot()+
    geom_errorbarh(aes(xmin=lab_ci_lb, xmax=lab_ci_ub, y=author_y))+
    geom_point(aes(x=beta ,y=author_y), size=4)+
    geom_vline(xintercept=0, linetype=1)+
    geom_vline(xintercept=original_results$beta, linetype=2)+
    scale_y_discrete(breaks=rev(c(unique(data$author_y),"Original result")),
                     limits=c(rev(c(unique(data$author_y),"Original result"))," "))+
    geom_text(aes(x=.50,y=author_y,label=lab_I_squared),size=3)+
    geom_text(aes(x=.54,y=author_y,label=lab_beta),size=3)+
    geom_text(aes(x=.60,y=author_y,label=lab_ci),size=3)+
    geom_text(aes(x=.50,y=21,label=label_I_squared),size=4)+
    geom_text(aes(x=.54,y=21,label=label_dppc2),size=4)+
    geom_text(aes(x=.60,y=21,label=label_ci),size=4)+
    scale_x_continuous(breaks = seq(0,.45, by=.05))+
    labs(y="Obmitted study",
         x="Beta coefficient")+
    theme(panel.grid = element_blank())

  }

#----    sens_cook    ----

# compute Cook's distances for each study

sens_cook <- function(fit_rma_mv){
  sens_cook = as.data.frame(
    cooks.distance(fit_rma_mv, cluster = fit_rma_mv$data$study))%>%
    mutate(author_y=factor(unique(fit_rma_mv$data$author_y), levels=unique(fit_rma_mv$data$author_y)),
           id=1:19)
  names(sens_cook)[1]="cooks_distance"
  
  return(sens_cook)
}
#----    sens_cook_plot    ----

# Ploot Cook's distances

sens_cook_plot <- function(sens_cook_summary){
  ggplot(sens_cook_summary)+
    geom_point(aes(x=author_y, y=cooks_distance),size=3)+
    geom_line(aes(x=id, y=cooks_distance), linetype=1)+
    theme(axis.text.x = element_text(angle=45,hjust = 1))+
    labs(x="Study",
         y="Cook's distance")
}



##################################
####    Moderator Analysis    ####
##################################

# For moderator analysis the rma_multilevel() function was used setting moderator = "" option

#----    table_moderator_analysis    ----

summary_moderator_analysis <- function(mod_rma_mv_pub,mod_rma_mv_grade,
                                     mod_rma_mv_weeks,mod_rma_mv_intensity,
                                     mod_rma_mv_device,mod_rma_mv_mot){
  
  cbind(Moderator=c("pub","grade","weeks","intensity","device","mot"),
        rbind(
          get_info_moderator_analysis(mod_rma_mv_pub),
          get_info_moderator_analysis(mod_rma_mv_grade),
          get_info_moderator_analysis(mod_rma_mv_weeks),
          get_info_moderator_analysis(mod_rma_mv_intensity),
          get_info_moderator_analysis(mod_rma_mv_device),
          get_info_moderator_analysis(mod_rma_mv_mot)))

}

################################
####    Publication Bias    ####
################################


#-------------


















