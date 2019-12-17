#################################
#######   Meta Analysis  ########
#################################



#-----   Settings ---------
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # setting wd to file location

library(metafor)
library(clubSandwich)
library(tidyverse)
library(knitr)
library(kableExtra)

set.seed(2019)
theme_set(theme_bw())

#############################################
#########    Data preparation  ##############
#############################################

#------   Load dataset  -------
data= read.csv("Dataset.csv",sep=";", header = T, stringsAsFactors = F)

data<-data%>% 
  # Define which variabel are factor
  mutate_at(vars("study","id","pub","dependence","grade","device","mot"), factor)%>% 
  # Redefine factor labels for grade and device
  mutate(grade=recode_factor(grade,"1"="Primary","2"="Secondary"),
         device=recode_factor(device,"app"="App","con"="Console","pc"="PC"),
         author_y=paste0(author," (",year,")"))


head(data)
str(data)
summary(data)

#----- Pareto et al. (2011) -------

# Pareto et al (2011) does not report post-test scores but gain scores (mean and sd)
# for the two effects:

# Control group
m_gain_cg<-c(1.5,-.23)    # Mean gain
sd_gain_cg<-c(3.82,3.62)  # Sd gain

# Experimental group
m_gain_eg<-c(.93,1.44)    # Mean gain
sd_gain_eg<-c(3.09,3.52)  # Sd gain

# To obtain the post-test scores we have:
# m_t2_* = m_t1_* + m_gain_*
# sd_t2_* = sqrt(sd_t1_*^2 + sd_gain_*^2)
# Because variances are independent 

# Control group
data$m_t2_cg[data$study=="11"]<-data$m_t1_cg[data$study=="11"]+m_gain_cg
data$sd_t2_cg[data$study=="11"]<-sqrt(data$sd_t1_cg[data$study=="11"]^2+sd_gain_cg^2)

# Experimental group
data$m_t2_eg[data$study=="11"]<-data$m_t1_eg[data$study=="11"]+m_gain_eg
data$sd_t2_eg[data$study=="11"]<-sqrt(data$sd_t1_eg[data$study=="11"]^2+sd_gain_eg^2)

#-----   compute dpcc2 effect and variance ---------

## Compute effect size according to Morris (2008) we used the index dppc2
## https://journals.sagepub.com/doi/10.1177/1094428106291059

## We used the same formulas reported in:
## http://www.metafor-project.org/doku.php/analyses:morris2008


### Compute effects
data<-data%>%
  mutate(sd_pool= sqrt(((n_eg-1)*sd_t1_eg^2+(n_cg-1)*sd_t1_cg^2)/(n_eg+n_cg-2)),
         yi_dppc2 = metafor:::.cmicalc(n_eg+n_cg-2)*((m_t2_eg - m_t1_eg) - (m_t2_cg-m_t1_cg))/sd_pool)


### Compute variance

# To compute the variance we need the correlation between pre- and post-test scores
# correlation are not reported so we try with different values
# high correlation (around .6), medium correlation (around .4), and low correlation (around .2)

# add correlation 
data<-data%>%
  mutate(r_high=rnorm(nrow(.),.6,.05),      # high correlation
         r_medium=rnorm(nrow(.),.4,.05),    # medium correlation
         r_low=rnorm(nrow(.),.2,.05))%>%    # low correlation
  # Rearrange dataset from wide format to long format
  gather(r_high,r_medium,r_low, key="r_size",value="r_value", factor_key = TRUE)  

# compute variance
data<-data%>%
  mutate(vi_dppc2=2*(1-r_value)*(1/n_eg + 1/n_cg)+ yi_dppc2^2/(2*(n_eg + n_cg)))

# ordering dataset
data<-data%>%
  arrange(study,n_effect,r_size)

data

#------- Hung et al. (2014) -----

# We remove the second effect in Hung et al. (2014) because evident problems.
# In the measure of the pre-test sd is very low (.02 and .00).
# In turns, this lead to an implausible effect of 38
# That is probably given by the fact that the measure used was not able to
# evaluate properly the variation among individuals (floor effect?)

# Have a look:
data%>%
  filter(study=="13")%>%
  select("study":"year","N":"n_eg","m_t1_cg":"vi_dppc2")

# Remove effect
data<-data%>%
  filter(study!= "13" | n_effect!=2)


#############################################
#########   Analysis for r_high  ############
#############################################
#-------- select only r_high ------
data_high <- data %>% 
  filter(r_size=="r_high")

#-------- Descreptive Statistics ------------

# Number of effects for each study
table(data_high$id)
table(table(data_high$id))

data_high%>%
  select(id)%>%
  table()%>%table()%>%
  as.data.frame()%>%
  select("Freq",".")%>%
  kable(.,col.names = c("Number of Studies","Number of Effects"),align = "c" )%>%
  kable_styling(bootstrap_options = c("striped"),full_width = F)

# We have 10 studies with only one effect, 3 studies with two effects, 2 studies with 3 effects,
# 3 studies with 4 effects, and 1 study with 8 effects

# In all studies, multiple effects were evaluated on the same subjects, except for Ke (2006) and Ke(2008)
# In Ke (2006) both control and treatment groups were independent fro each effect
# In Ke (2008) the treatment groups were independent but only one control group was considered

# Study by year
data_high%>%
  filter(!duplicated(id))%>%
  ggplot()+
  geom_bar(aes(x=year), col="black", fill="lightblue")+
  scale_x_continuous(breaks = seq(from=2005, to=2018, by=1))+
  scale_y_continuous(breaks = seq(from=1, to=4, by=1))+
  theme(panel.grid.minor = element_blank())+
  xlab("Year of Publication")+
  ylab("Frequency")


# Study by number of participants
data_high%>%
  filter(!duplicated(id))%>%
  mutate(author_y=reorder(author_y,N))%>%
  ggplot()+
  geom_bar(aes(x=author_y, y=N),col="black", fill="lightblue",stat="identity")+
  scale_y_continuous(breaks = seq(from=0, to=1200, by=150))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Study")+
  ylab("Sample Size")
  

##### Effects according to cluster (ordered by  number of participants)  ####
my_breaks = c(50, 100, 200, 400,1000)
data_high%>%
  mutate(author_y=reorder(author_y,N))%>%
  ggplot()+
  geom_point(aes(x=author_y, y=yi_dppc2, col=N, size=N),position= position_dodge2(.5))+
  geom_errorbar(aes(ymin=yi_dppc2-vi_dppc2, ymax=yi_dppc2+vi_dppc2,x=author_y, col=N),
                position= position_dodge2(.5), width=.5)+
  scale_y_continuous(breaks = seq(from=-.5, to=1, by=.25))+
  scale_color_gradient2(name = "Sample Size", trans = "log",
                        breaks = my_breaks, labels = my_breaks)+
  scale_size_continuous(guide=FALSE)+
  ylab("Studies (ordered by sample size)")+
  xlab("Estimated Effect")+
  theme(legend.position = c(.9,.88))+
  coord_flip()


##### Effects gruped into cluster (ordered by  number of participants) according to published or not ####
table(data_high$pub[!duplicated(data_high$id)])

data_high%>%
  mutate(author_y=reorder(author_y,N))%>%
  ggplot()+
  geom_point(aes(x=author_y, y=yi_dppc2, col=pub, shape=pub, size=N),position= position_dodge2(.5))+
  geom_errorbar(aes(ymin=yi_dppc2-vi_dppc2, ymax=yi_dppc2+vi_dppc2,x=author_y, col=pub),
                position= position_dodge2(.5), width=.5)+
  scale_y_continuous(breaks = seq(from=-.5, to=1, by=.25))+
  scale_size_continuous(guide=FALSE)+
  scale_color_discrete("Published")+
  scale_shape_discrete("Published")+
  ylab("Studies (ordered by sample size)")+
  xlab("Estimated Effect")+
  theme(legend.position = c(.9,.88))+
  coord_flip()

##### Effects gruped into cluster (ordered by  number of participants) according to grade ####
table(data_high$grade[!duplicated(data_high$id)])

data_high%>%
  mutate(author_y=reorder(author_y,N))%>%
  ggplot()+
  geom_point(aes(x=author_y, y=yi_dppc2, col=grade, shape=grade, size=N),position= position_dodge2(.5))+
  geom_errorbar(aes(ymin=yi_dppc2-vi_dppc2, ymax=yi_dppc2+vi_dppc2,x=author_y, col=grade),
                position= position_dodge2(.5), width=.5)+
  scale_y_continuous(breaks = seq(from=-.5, to=1, by=.25))+
  scale_size_continuous(guide=FALSE)+
  scale_color_discrete("School")+
  scale_shape_discrete("School")+
  ylab("Studies (ordered by sample size)")+
  xlab("Estimated Effect")+
  theme(legend.position = c(.9,.88))+
  coord_flip()

##### Effects gruped into cluster (ordered by  number of participants) according to weeks #####
table(data_high$weeks[!duplicated(data_high$id)])

my_breaks = c(1,2,3,4,6,9,12,18,25,30)
data_high%>%
  mutate(author_y=reorder(author_y,N))%>%
  ggplot()+
  geom_point(aes(x=author_y, y=yi_dppc2, col=weeks, size=N),position= position_dodge2(.5))+
  geom_errorbar(aes(ymin=yi_dppc2-vi_dppc2, ymax=yi_dppc2+vi_dppc2,x=author_y, col=weeks),
                position= position_dodge2(.5), width=.5)+
  scale_y_continuous(breaks = seq(from=-.5, to=1, by=.25))+
  scale_size_continuous(guide=FALSE)+
  scale_color_gradient("Weeks",trans="log",
                       breaks = my_breaks, labels = my_breaks)+
  ylab("Studies (ordered by sample size)")+
  xlab("Estimated Effect")+
  theme(legend.position = c(.9,.88))+
  coord_flip()

##### Effects gruped into cluster (ordered by  number of participants) according to motivation #####
table(data_high$mot)

data_high%>%
  mutate(author_y=reorder(author_y,N))%>%
  ggplot()+
  geom_point(aes(x=author_y, y=yi_dppc2, col=mot,shape=mot, size=N),position= position_dodge2(.5))+
  geom_errorbar(aes(ymin=yi_dppc2-vi_dppc2, ymax=yi_dppc2+vi_dppc2,x=author_y, col=mot),
                position= position_dodge2(.5), width=.5)+
  scale_y_continuous(breaks = seq(from=-.5, to=1, by=.25))+
  scale_size_continuous(guide=FALSE)+
  scale_color_discrete("Motivation")+
  scale_shape_discrete("Motivation")+
  ylab("Studies (ordered by sample size)")+
  xlab("Estimated Effect")+
  theme(legend.position = c(.9,.88))+
  coord_flip()

#-------- Meta-Analysis ---------

# To account for dependence we conducted a multilevel Meta-Analysis
# https://doi.org/10.3758/s13428-014-0527-2
# https://doi.org/10.1080/13645579.2016.1252189

# Multilevel Meta-Analysis allows us to take into account dependency between multiple effects in the same study
# Moreover it allows us to define the specific kind of dependence in each study using the variance-covariance matrix.

# In our case the majority of studies have multiple measure on the same subjects
# in this cases we impute a correlation between the different  task to compute the variance-covariance matrix
# In a sensitivity analysis we will evaluate the role of the imputed correlation value

# In Ke (2006) the multiple measures are independent so we can set covariance to zero but multilevel meta-analisi
# will still take into account that these effects come from the same study

# In Ke (2008) treatment group are independet but the control group is the same.
# To take into account this dependence we use formula proposed by  Gleser and Olkin (2009) [DA RIVEDERE!!!]


### Create variance-covarinance matrix  ######

# 1) Considering all effects correlated
cov_dppc2<-with(data_high, 
                impute_covariance_matrix(vi = vi_dppc2, cluster =study, r = 0.5))

# 2) Effect sizes in Ke (2006) are independent so we set covariances to 0
cov_dppc2[[2]]<-cov_dppc2[[2]]*diag(1,3,3)

# 3) Effect sizes in Ke (2008) are computed with different treatment group but same control group
# function to compute variance-covariance matrix we used referred to:
# http://www.metafor-project.org/doku.php/analyses:gleser2009 (section "Quantitative Response Variable")

calc.vcv <- function(x) {
  Ni <- rep(sum(x$n_ec) + x$n_cg[1], each=nrow(x))
  v <- matrix(1/x$n_cg[1] + outer(x$yi_dppc2, x$yi_dppc2, "*")/(2*Ni[1]), nrow=nrow(x), ncol=nrow(x))
  diag(v) <- x$vi
  v
}
cov_dppc2[[3]]<-calc.vcv(data_high[data_high$study==3,])

rma_mv_fit <- rma.mv(yi = yi_dppc2, V = cov_dppc2, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
rma_mv_fit
coef_test(rma_mv_fit, cluster = data_high$study, vcov = "CR2") # to get estimated with correction for small sample size

forest(rma_mv_fit)


# compute I squared using formula reported in
# http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate (section: Multilevel Models)

I_squared<-function(model){
  W <- diag(1/model$vi)
  X <- model.matrix(model)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * sum(model$sigma2) / (sum(model$sigma2) + (model$k-model$p)/sum(diag(P)))
}
I_squared(rma_mv_fit)

#------  Sensitivity Analysis -------

## 1) Sensitivity for values of correlation between outcomes
sensitivity_corr_outcomes<-matrix(NA, ncol=9, nrow=9)
colnames(sensitivity_corr_outcomes)<-c("r_value","Sigma_squared","Q_value","I_squared","Estimate","SE","t_value","df","p_value")

k=1
for (i in seq(0.1,.9,by=.1)){
  cov_sensitivity<-with(data_high, impute_covariance_matrix(vi = vi_dppc2, cluster =study, r = i))
  cov_sensitivity[[2]]<-cov_sensitivity[[2]]*diag(1,3,3)
  cov_sensitivity[[3]]<-calc.vcv(data_high[data_high$study==3,])
  
  rma_sensitivity <- rma.mv(yi = yi_dppc2, V = cov_sensitivity, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
  sensitivity_corr_outcomes[k,]<-as.matrix(cbind(i,rma_sensitivity$sigma2,rma_sensitivity$QE,I_squared(rma_sensitivity),
                                       coef_test(rma_sensitivity, cluster = data_high$study, vcov = "CR2")))
  k=k+1
}

sensitivity_corr_outcomes
summary(sensitivity_corr_outcomes)

# Values do not change considerably

## 2) Leave one out Sensitivity

sensitivity_loo<-matrix(NA,ncol=8, nrow = 19)
colnames(sensitivity_loo)<-c("Sigma_squared","Q_value","I_squared","Estimate","SE","t_value","df","p_value")
rownames(sensitivity_loo)<-data_high$author_y[!duplicated(data_high$author_y)]

k=1
for (i in data_high$id[!duplicated(data_high$id)]){
  cov_sensitivity<-cov_dppc2[-k]
  rma_sensitivity<-data_high %>% filter(id!=i) %>%
    rma.mv(yi = yi_dppc2, V = cov_sensitivity, random =  ~ 1|study, data = ., slab=author_y)
  sensitivity_loo[k,]<-as.matrix(cbind(rma_sensitivity$sigma2,rma_sensitivity$QE,I_squared(rma_sensitivity),
                                       coef_test(rma_sensitivity, cluster = data_high$study[data_high$study!=k], vcov = "CR2")))
  k=k+1
}

sensitivity_loo
summary(sensitivity_loo)

# Estimated values range from .22 to.27
# Values do not change considerably


## 3) Sensitivity correlation pre-post test

# r_medium
data_medium<-data%>%
  filter(r_size=="r_medium")
cov_dppc2_medium<-with(data_medium, impute_covariance_matrix(vi = vi_dppc2, cluster =study, r = 0.5))
cov_dppc2_medium[[2]]<-cov_dppc2_medium[[2]]*diag(1,3,3)
cov_dppc2_medium[[3]]<-calc.vcv(data_medium[data_medium$study==3,])

rma_mv_medium<-rma.mv(yi = yi_dppc2, V = cov_dppc2_medium, random =  ~ 1|study, method = "REML", data = data_medium, slab=author_y)
rma_mv_medium
coef_test(rma_mv_medium, cluster = data_high$study, vcov = "CR2")
I_squared(rma_mv_medium)

# r_low
data_low<-data%>%
  filter(r_size=="r_low")
cov_dppc2_low<-with(data_low, impute_covariance_matrix(vi = vi_dppc2, cluster =study, r = 0.5))
cov_dppc2_low[[2]]<-cov_dppc2_low[[2]]*diag(1,3,3)
cov_dppc2_low[[3]]<-calc.vcv(data_low[data_low$study==3,])

rma_mv_low<-rma.mv(yi = yi_dppc2, V = cov_dppc2_low, random =  ~ 1|study, method = "REML", data = data_low, slab=author_y)
rma_mv_low
coef_test(rma_mv_low, cluster = data_high$study, vcov = "CR2") 
I_squared(rma_mv_low)


## EXTRA: 4) Sensitivity varying correlation pre-post test and correlation between outcomes

sensitivity_extra<-matrix(NA, ncol=9, nrow=27)
colnames(sensitivity_extra)<-c("r_outcomes","Sigma_squared","Q_value","I_squared","Estimate","SE","t_value","df","p_value")

k=1
for(j in c("r_high","r_medium","r_low")){
  data_extra<-data%>%
    filter(r_size==j)
  for (i in seq(0.1,.9,by=.1)){
    cov_sensitivity<-with(data_extra, impute_covariance_matrix(vi = vi_dppc2, cluster =study, r = i))
    cov_sensitivity[[2]]<-cov_sensitivity[[2]]*diag(1,3,3)
    cov_sensitivity[[3]]<-calc.vcv(data_extra[data_extra$study==3,])
    
    rma_sensitivity <- rma.mv(yi = yi_dppc2, V = cov_sensitivity, random =  ~ 1|study, method = "REML", data = data_extra, slab=author_y)
    sensitivity_extra[k,]<-as.matrix(cbind(i,rma_sensitivity$sigma2,rma_sensitivity$QE,I_squared(rma_sensitivity),
                                         coef_test(rma_sensitivity, cluster = data_extra$study, vcov = "CR2")))
    k=k+1
  }
}

sensitivity_extra<-cbind(r_pre_post=rep(c(.6,.4,.2),each=9),sensitivity_extra)
sensitivity_extra
summary(sensitivity_extra)

# Values do not change considerably even when all possible combinations are evaluated


#------  Moderator Analysis  ---------

## 1) Publication

rma_mod<-rma.mv(yi = yi_dppc2, V = cov_dppc2,mods = ~pub, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
rma_mod
AIC(rma_mv_fit,rma_mod)
# Publication is not an important moderator


## 2) Grade

rma_mod<-rma.mv(yi = yi_dppc2, V = cov_dppc2,mods = ~grade, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
rma_mod
AIC(rma_mv_fit,rma_mod)
# Grade is not an important moderator


## 3) Weeks

rma_mod<-rma.mv(yi = yi_dppc2, V = cov_dppc2,mods = ~weeks, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
rma_mod
AIC(rma_mv_fit,rma_mod)
# Weeks not an important moderator


## 4) intensity

data_high<-data_high%>%
  mutate(intensity=sessions*minutes/weeks)
rma_mod<-rma.mv(yi = yi_dppc2, V = cov_dppc2,mods = ~intensity, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
rma_mod
AIC(rma_mv_fit,rma_mod)
# Intensity is not an important moderator  (only 14 studies, 31 effects included)


## 5) device

rma_mod<-rma.mv(yi = yi_dppc2, V = cov_dppc2,mods = ~device, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
rma_mod
AIC(rma_mv_fit,rma_mod)
# Device is not an important moderator

## 6) Outcome

rma_mv_mot<-rma.mv(yi = yi_dppc2, V = cov_dppc2,mods = ~mot, random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
rma_mv_mot
# Outcome is an important moderator (only 13 studies, 32 effects included)

#------  Publication Bias  ------

# Funnel plot representation (all effects)
funnel(rma_mv_fit)

# Funnel plot representation (one effect for study)
k<-funnel(rma_mv_fit)
k<-k%>%
  mutate(study=data_high$study)%>%
  group_by(study)%>%
  summarise(x=mean(x),y=mean(y))%>%
  mutate(author_y=unique(data_high$author_y))
plot(k$x,k$y, ylim = c(.4,0), xlab="Mean yi_dppc2", ylab="Mean SE")
abline(v=c(0,rma_mv_fit$b), lty=c(1,2), col=c("black","red"))

#Egger's regression test
# 1) considering se as predictor
rma.mv(yi = yi_dppc2, V = cov_dppc2,mods=sqrt(vi_dppc2) ,random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)

# 2) considering N as predictor
bias_rma<-rma.mv(yi = yi_dppc2, V = cov_dppc2,mods=N ,random =  ~ 1|study, method = "REML", data = data_high, slab=author_y)
bias_rma

# There is evidence for asymmetry in the funnel plot

