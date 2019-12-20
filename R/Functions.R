
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

#----    munge_data function   ----

munge_data <- function(data){
  #----   munge_data   ----
  data<-data%>% 
    # Define which variabel are factor
    mutate_at(vars("study","id","pub","dependence","grade","device","mot"), factor)%>% 
    # Redefine factor labels for grade and device
    mutate(grade=recode_factor(grade,"1"="Primary","2"="Secondary"),
           device=recode_factor(device,"app"="App","con"="Console","pc"="PC"),
           author_y=paste0(author," (",year,")"))
  #----
  
  # Pareto et al (2011) does not report post-test scores but gain scores (mean and sd)
  # for the two effects:
  
  # To obtain the post-test scores we have:
  # m_t2_* = m_t1_* + m_gain_*
  # sd_t2_* = sqrt(sd_t1_*^2 + sd_gain_*^2)
  # Because variances are independent 
  
  #----- pareto_post -------
  
  # Control group
  m_gain_cg<-c(1.5,-.23)    # Mean gain
  sd_gain_cg<-c(3.82,3.62)  # Sd gain
  
  # Experimental group
  m_gain_eg<-c(.93,1.44)    # Mean gain
  sd_gain_eg<-c(3.09,3.52)  # Sd gain
  
  
  # Control group
  data$m_t2_cg[data$study=="11"]<-data$m_t1_cg[data$study=="11"]+m_gain_cg
  data$sd_t2_cg[data$study=="11"]<-sqrt(data$sd_t1_cg[data$study=="11"]^2+sd_gain_cg^2)
  
  # Experimental group
  data$m_t2_eg[data$study=="11"]<-data$m_t1_eg[data$study=="11"]+m_gain_eg
  data$sd_t2_eg[data$study=="11"]<-sqrt(data$sd_t1_eg[data$study=="11"]^2+sd_gain_eg^2)
  
  #----
  
  return(data)
}

#----    compute_dppc2 function    ----

## Compute effect size according to Morris (2008) we used the index dppc2
## https://journals.sagepub.com/doi/10.1177/1094428106291059

## We used the same formulas reported in:
## http://www.metafor-project.org/doku.php/analyses:morris2008

compute_dppc2 <- function(data){
  
  #----    compute_yi_dppc2    ----
  
  # Compute yi_dppc2
  data<-data%>%
    mutate(sd_pool= sqrt(((n_eg-1)*sd_t1_eg^2+(n_cg-1)*sd_t1_cg^2)/(n_eg+n_cg-2)),
           yi_dppc2 = metafor:::.cmicalc(n_eg+n_cg-2)*((m_t2_eg - m_t1_eg) - (m_t2_cg-m_t1_cg))/sd_pool)
  #----
  
  # To compute the variance we need the correlation between pre- and post-test scores
  # correlation are not reported so we try with different values
  # high correlation (around .8), medium-high correlation (around .6),
  # medium-low correlation (around .4), and low correlation (around .2)
  
  #----    compute_vi_dppc2    ----
  
  # Compute vi_dppc2
  
  # add correlations pre-post 
  set.seed(2020)
  
  data<-data%>%
    mutate(r_high=.8,      # high correlation
           r_mediumh=.6,   # medium-high correlation
           r_mediuml=.4,   # medium-low correlation
           r_low=.2)%>%    # low correlation
    # Rearrange dataset from wide format to long format
    gather("r_high":"r_low", key="r_size",value="r_value", factor_key = TRUE)  
  
  # compute variance
  data<-data%>%
    mutate(vi_dppc2=2*(1-r_value)*(1/n_eg + 1/n_cg)+ yi_dppc2^2/(2*(n_eg + n_cg)))
  
  # ordering dataset
  data<-data%>%
    arrange(study,n_effect,r_size)
  
  
}


#----    rm_hung function    ----

rm_hung <- function(data){
  # We remove the second effect in Hung et al. (2014) because evident problems.
  # In the measure of the pre-test sd is very low (.02 and .00).
  # In turns, this lead to an implausible effect of 38
  # That is probably given by the fact that the measure used was not able to
  # evaluate properly the variation among individuals (floor effect?)
  
  #----    rm_hung    ----
  
  # Remove effect
  data<-data%>%
    filter(study!= "13" | n_effect!=2)
  #----
  return(data)
}


######################################
####    Descriptive Statistics    ####
######################################

####    Studies level    ####

#----    table_n_studies_effects    ----

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

#  Used for publication, school-grade 
freq_table <- function(data, var_name){
  var_name = enquo(var_name)
  data%>%
    filter(!duplicated(id))%>%
    group_by(!!var_name)%>%
    count()

}

#----    freq_table_weeks   ----

#  Used for publication, school-grade 
freq_table_weeks <- function(data){
  data%>%
    filter(!duplicated(id))%>%
    mutate(week_groups=if_else(weeks==1, "1 week",
                               if_else(weeks<=4, "4 weeks or less",
                                       if_else(weeks<=10,"10 weeks or less","more than 10 weeks"))),
           week_groups=factor(week_groups,levels=c("1 week","4 weeks or less","10 weeks or less","more than 10 weeks")))%>%
    group_by(week_groups)%>%
    count()
  }

#----    plot_participants_studies -----

participants_studies <- function(data){
  # Compute the total number of participants in 
  # Ke(2006)
  N_ke2006 = data%>%
    filter(id=="961" & !duplicated(id_effect))%>%
    select(n_cg,n_eg)%>%
    sum()
  
  # Ke(2008)
  N_ke2008 = data%>%
    filter(id=="432" & !duplicated(id_effect))%>%
    select(n_cg,n_eg)%>%
    summarise(n_cg=n_cg[1],
              n_eg=sum(n_eg))%>%
    sum()
  
  data%>%
    filter(!duplicated(id))%>%
    mutate(N = ifelse(id=="961", N_ke2006, N),
           N = ifelse(id=="432", N_ke2008, N),
           author_y=reorder(author_y,N))%>%
    ggplot()+
    geom_bar(aes(x=author_y, y=N),col="black", fill="lightblue",stat="identity")+
    scale_y_continuous(breaks = seq(from=0, to=1200, by=150))+
    theme(axis.text.x = element_text(angle = 45, hjust=1))+
    xlab("Study")+
    ylab("Sample Size")
    
}

#----    plot_effects_participants    ----

effects_participants <- function(data){
  my_breaks = c(50, 100, 200, 400,1000)
  data%>%
    filter(r_size=="r_mediumh")%>%
    mutate(author_y=reorder(author_y,N))%>%
    ggplot()+
    geom_point(aes(x=author_y, y=yi_dppc2, col=N, size=N),position= position_dodge2(.5))+
    geom_errorbar(aes(ymin=yi_dppc2-vi_dppc2, ymax=yi_dppc2+vi_dppc2,x=author_y, col=N),
                  position= position_dodge2(.5), width=.5)+
    scale_y_continuous(breaks = seq(from=-.5, to=1, by=.25))+
    scale_color_gradient(name = "Sample Size", trans = "log",
                         breaks = my_breaks, labels = my_breaks,
                         low="lightblue", high="darkblue")+
    scale_size_continuous(guide=FALSE)+
    xlab("Studies (ordered by sample size)")+
    ylab("Estimated Effect")+
    theme(legend.position = c(.9,.88))+
    coord_flip()
  
}

#----    freq_table_mot    ----

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
# function to compute variance-covariance matrix when multiple treatment groups
# are compared to the same control group:
# http://www.metafor-project.org/doku.php/analyses:gleser2009 (section "Quantitative Response Variable")

calc_vcv <- function(data) {
  Ni <- rep(sum(data$n_eg) + data$n_cg[1], each=nrow(data))
  v <- matrix(1/data$n_cg[1] + outer(data$yi_dppc2, data$yi_dppc2, "*")/(2*Ni[1]), nrow=nrow(data), ncol=nrow(data))
  diag(v) <- data$vi_dppc2
  v
}

#----    compute_vcv_matrix    ----  
compute_vcv_matrix <- function(data, r=.5){
  
  # Considering all effects correlated
  cov_dppc2= with(data,clubSandwich::impute_covariance_matrix(
    vi = vi_dppc2, cluster =study, r = r))
  
  # Effect sizes in Ke (2006) are independent so we set covariances to 0
  cov_dppc2[[2]] = cov_dppc2[[2]]*diag(1,3,3)
  
  # Effect sizes in Ke (2008) are computed with different treatment group but same control group
  #cov_dppc2[[3]] = calc_vcv(data[data$study==3,])
  
  return(cov_dppc2)
}

#----    rma_multilevel    ----

rma_multilevel <-  function(data, r_pre_post="r_mediumh", r_outocomes=.5, excluded_study=NULL){
  data = data%>%
    filter(r_size==r_pre_post)
  
  # compute variance-covariance matrix
  cov_dppc2 = compute_vcv_matrix(data, r=r_outocomes)
  
  # check if study have to be excluded (for the sens_loo)
  if(!is.null(excluded_study)){
    cov_dppc2 = compute_vcv_matrix(data, r=.5)[-excluded_study]
    data = data%>%
      filter(study!=excluded_study)
  }
  
  # multilevel meta-analysis
  fit_rma_mv = rma.mv(yi = yi_dppc2, V = cov_dppc2, random =  ~ 1|study, 
                      method = "REML", data = data, slab=author_y)
  
  fit_rma_mv$I_squared = I_squared(fit_rma_mv)
  fit_rma_mv$coef_test = coef_test(fit_rma_mv, cluster = data$study, vcov = "CR2")
  fit_rma_mv$r_pre_post=r_pre_post
  fit_rma_mv$r_outocomes=r_outocomes
  fit_rma_mv$data = data
  fit_rma_mv$excluded_study = excluded_study
  
  return(fit_rma_mv)
}

#----    forest_plot    ----

forest_plot <- function(fit_rma_mv){
  
  data =fit_rma_mv$data
  
  labels <- data %>%
    transmute(autor_y= if_else(n_effect==1,true=author_y,false="-"),
              n_effect, N,
              ci = paste0("[",format(round(yi_dppc2-1.96*sqrt(vi_dppc2),2),2),
                          ";",format(round(yi_dppc2+1.96*sqrt(vi_dppc2),2),2),"]"),
              yi_dppc2 = format(round(yi_dppc2, 2),2))
    
  forest(fit_rma_mv, slab =labels$autor_y, xlim=c(-2.5,3),ilab=labels[,-1],
         ilab.pos=2, ilab.xpos=c(-1,1.8,3,2.3), annotate = F)
  
  text(-2.5,44, "Author(s) and Year",  pos=4,font=2)
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

########################################
####    Sensitivity correlations    ####
########################################

#----    summarize_fit_rma_mv    ----

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

#----    sens_summary_plot    ----

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
         y="Sigma",
         legend="Correlation pre-post score")+
    theme(legend.position = "top")
  
  p2 = ggplot(sens_summary)+
 #   geom_errorbar(aes(x=r_outcomes, ymin=beta_lb, ymax=beta_ub, col=r_pre_post))+
    geom_point(aes(x=r_outcomes, y=beta, col=r_pre_post, shape=r_pre_post))+
    geom_line(aes(x=r_outcomes, y=beta, col=r_pre_post,linetype=r_pre_post))+
    scale_x_continuous(breaks = seq(from=0.1, to=.9, by=.1))+
    labs(x="Correlation between outcomes",
         y="Beta")+
    theme(legend.position = "none")
  
  p3 = ggplot(sens_summary)+
    geom_point(aes(x=r_outcomes, y=I_squared, col=r_pre_post, shape=r_pre_post))+
    geom_line(aes(x=r_outcomes, y=I_squared, col=r_pre_post, linetype=r_pre_post))+
    scale_x_continuous(breaks = seq(from=0.1, to=.9, by=.1))+
    labs(x="Correlation between outcomes",
         y="I^2")+
    theme(legend.position = "none")
  
  legend = get_legend(p1)
  
  grid.arrange(p1+theme(legend.position = "none"),
               p2,p3,legend,
               ncol=1, heights=c(6,6,6,1))
  
}


#########################################
####    Sensitivity leave-one-out    ####
#########################################

#----    sens_loo_plot    ----


#-------------


















