
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
    mutate(r_high=rnorm(nrow(.),.8,.05),      # high correlation
           r_mediumh=rnorm(nrow(.),.6,.05),   # medium-high correlation
           r_mediuml=rnorm(nrow(.),.4,.05),   # medium-low correlation
           r_low=rnorm(nrow(.),.2,.05))%>%    # low correlation
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

#----    plot_publication_year

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



#-------------













