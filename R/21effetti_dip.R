#################################
#######   Meta Analysis  ########
#################################



#-----   Settings ---------
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # setting wd to file location

library(metafor)
library(tidyverse)
library(MAd)
library(robumeta)
library(grid)
library(clubSandwich)

set.seed(2019)
theme_set(theme_bw())

#############################################
#########    Data preparation  ##############
#############################################

#------   Load dataset  -------
data= read.csv("Dataset.csv",sep=";", header = T, stringsAsFactors = F)

data<-data%>%
  mutate_at(vars("study","id","pub","dependence","grade","device","mot"), factor)%>%
  mutate(grade=recode_factor(grade,"1"="Primary","2"="Secondary"),
         device=recode_factor(device,"app"="App","con"="Console","pc"="PC"))


head(data)
str(data)
summary(data)

#----- Pareto et al. (2011) -------

# Pareto et al (2011) does not report post-test scores but gain scores (mean and sd)
# for the two effects:

# Control group
m_gain_cg<-c(1.5,-.23)
sd_gain_cg<-c(3.82,3.62)

# Experimental group
m_gain_eg<-c(.93,1.44)
sd_gain_eg<-c(3.09,3.52)

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
  mutate(r_high=rnorm(nrow(.),.6,.05),
         r_medium=rnorm(nrow(.),.4,.05),
         r_low=rnorm(nrow(.),.2,.05))%>%
  gather(r_high,r_medium,r_low, key="r_size",value="r_value", factor_key = TRUE)

# compute variance
data<-data%>%
  mutate(vi_dppc2=2*(1-r_value)*(1/n_eg + 1/n_cg)+ yi_dppc2^2/(2*(n_eg + n_cg)))

# ordering dataset
data<-data%>%
  arrange(study,effect,r_size)

data

#------- Hung et al. (2014) -----

# We remove the second effect in Hung et al. (2014) because evident problems.
# In the measure of the pre-test, sd is very low (.02 and .005).
# In turns, this lead to an implausible effect of 37

# Have a look:
data%>%
  filter(study=="13")%>%
  select("study":"year","N":"n_eg","m_t1_cg":"vi_dppc2")

# Remove effect
data<-data%>%
  filter(study!= "13" | effect!=2)


#############################################
#########   Analysis for r_high  ############
#############################################
#-------- select only r_high ------
data_high <- data %>% 
  filter(r_size=="r_high")
  
#-------- robumeta ---------
# Robumeta uses robust variance estimation method
# see: https://arxiv.org/pdf/1503.02220.pdf

### Correlation method
meta_robu<-robu(yi_dppc2~1, var.eff.size = vi_dppc2, studynum = study,
                modelweights = "CORR",rho=.5, small = TRUE, data=data_high)
sensitivity(meta_robu)
meta_robu
forest.robu(meta_robu, es.lab = "effect", study.lab = "study")

### Hierarchical method
meta_robu_h<-robu(yi_dppc2~1, var.eff.size = vi_dppc2, studynum = study,
                modelweights = "HIER",rho=.5, small = TRUE, data=data_high)
meta_robu_h

forest.robu(meta_robu_h, es.lab = "effect", study.lab = "study")

#--------- metafor mv --------
# Multilevel meta-analysis using metafor

### Considering all effects correlated
cov_dppc2<-with(data_high, 
                impute_covariance_matrix(vi = vi_dppc2, cluster =study, r = 0.5))
rma_mv_fit <- rma.mv(yi = yi_dppc2, V = cov_dppc2, random =  ~ 1|study, data = data_high)
coef_test(rma_mv_fit, cluster = data_high$study, vcov = "CR2")

forest(rma_mv_fit, slab = paste(data_high$author, data_high$year))

### adjusted covariance according to studies different dependences
cov_dppc2_rev<-cov_dppc2

# Effect sizes in Ke (2006) are independent so we set covariances to 0
cov_dppc2_rev[[2]]<-cov_dppc2[[2]]*diag(1,3,3)

# Effect sizes in Ke (2008) are computed with different treatment group but same control group
cov_dppc2[[3]]

# function to compute variance-covariance matrix
# http://www.metafor-project.org/doku.php/analyses:gleser2009 (section "Quantitative Response Variable")

calc.vcv <- function(x) {
  Ni <- rep(sum(x$n_ec) + x$n_cg[1], each=nrow(x))
  v <- matrix(1/x$n_cg[1] + outer(x$yi_dppc2, x$yi_dppc2, "*")/(2*Ni[1]), nrow=nrow(x), ncol=nrow(x))
  diag(v) <- x$vi
  v
}
cov_dppc2_rev[[3]]<-calc.vcv(data_high[data_high$study==3,])

rma_mv_fit_rev <- rma.mv(yi = yi_dppc2, V = cov_dppc2_rev, random =  ~ 1|study, data = data_high)
coef_test(rma_mv_fit_rev, cluster = data_high$study, vcov = "CR2")

forest(rma_mv_fit_rev, slab = paste(data_high$author, data_high$year))


#---------- Aggregating effects -------
# method = "BHHR"; from Borenstein et al. (2009) p.228
data_BHHR<-data.frame(data_high[!duplicated(data_high$study),],
                      agg(id = study, es = yi_dppc2, var = vi_dppc2, method = "BHHR",cor = .5, data = data_high))

rma_BHHR<-rma(yi=es, vi=var, method ="REML", data=data_BHHR )
rma_BHHR

forest(rma_BHHR, slab = paste("Study",data_BHHR$study))

# method= "GO1"; from Gleser & Olkin (2009) p.369
data_GO1<-data.frame(data_high[!duplicated(data_high$study),],
                      agg(id = study, es = yi_dppc2, var = vi_dppc2,
                          n.1 = n_eg, n.2 =n_cg, method = "GO1",cor = .5, data = data_high))

rma_GO1<-rma(yi=es1, vi=var1, method ="REML", data=data_GO1 )
rma_GO1

forest(rma_GO1, slab = paste("Study",data_GO1$study))

# another way that gives the same results as the metafor mv (first case)
# Function from https://www.jepusto.com/sometimes-aggregating-effect-sizes-is-fine/

agg_effects <- function(yi, vi, r = 0.6) {
  corr_mat <- r + diag(1 - r, nrow = length(vi))
  sd_mat <- tcrossprod(sqrt(vi))
  V_inv_mat <- chol2inv(chol(sd_mat * corr_mat))
  V <- 1 / sum(V_inv_mat)
  data.frame(es = V * sum(yi * V_inv_mat), var = V)
}

corrdat_agg <-
  data_high %>%
  group_by(study) %>%
  summarise(
    es = list(agg_effects(yi = yi_dppc2, vi = vi_dppc2, r = 0.5))) %>%
  unnest()

uni_fit <- rma(es ~ 1, vi = var, data = corrdat_agg, method = "REML")
uni_fit


##########################################################################
##########################################################################
##########################################################################

#####              CODICE VECCHIO (NON GUARDARE)                ##########

##########################################################################
##########################################################################
##########################################################################


#-------- Summarizing multiple effects ---------



# vedere i dati
data%>%
  filter(r_size=="r_high")%>%
  ggplot()+
  geom_histogram(aes(x=yi_dppc2), fill="lightblue", col="black", alpha=.9)

#-------- to be continued ---------


## effect size globale random con varianza high
met_rand_h = rma(dat1$dpc2, dat1$vi_s, method="REML")
met_rand_h 
## Larger values of Q reflect greater between-study heterogeneity
##  Higgins I2 estimates, in percentage, how much of the total variability in the 
#effect size estimates can be attributed to heterogeneity among the true effects
## To explain heterogeneity, study-level predictor variables can be specified,
# a mixed-effects model (Meta-Regression) will be performed.

forest(met_rand_h)

library(MAd) ## pacchetto MAd = Meta-Analysis with Mean Differences
# for conducting a mean differences meta-analysis
# using recommended procedures as described in 
# The Handbook of Research Synthesis and Meta-Analysis (Cooper, Hedges, and Valentine, 2009)

MA1 <- agg(id=study, es=yi_dppc2, var=vi_dppc2, n.1=n_eg, n.2=n_cg, cor = .5, method="GO1", data=data)
MA1 ## Aggregate Dependent Effect Sizes
## aggregate all within-study effect sizes 
# while taking into account the correlations among the within-study outcomes
## GO1= Gleser and Olkin (1994) procedure using pooled standard deviation (SD)
## la correlazioni tra effetti dello stesso studio ? .5 di default
## volendo posso anche scegliere correlazioni diverse per i vari studi

plot(MA1$es1)
hist(MA1$es1, breaks=10, freq = F) ## per avere i due grafici 
lines (density(MA1$es1))
boxplot(MA1$es1)

met_Mad = rma(MA1$es1, MA1$var1, method="REML", slab=paste(MA1$id)) ## per inserire i nomi degli studi nel plot
met_Mad
forest(met_Mad)


grade <- factor(c("2","1","1","1","1","2","1","1","2","2","2","1","1","1","1","1","1","2","2","1","2"))
MA1$grade <- grade   ## inserisco la variabile grade

week <- c("18", "4", "4", "4", "4", "18", "7","10","9","14","9","1","9","2","1","5","10","6","10","1","28")
MA1$week<- week
MA1$week=as.numeric (MA1$week)

exper<- factor(c("1","1", "1","1","1","1","1","1","2","2","2","2","1","1","1","1","1","1","1","1","1"))
MA1$exp <-exper

maschi <- c("46","51","51", "51", "52","53","46","53","50","58","44","45","48.5","48","52","44","45","62.5","50","50","73.7")
MA1$masch <- maschi ## percentuale di maschi nel campione
MA1$masch= as.numeric (MA1$masch)
str(MA1)


##sensitivity analysis, cosa accade se tolgo di volta in volta uno studio

leave1out(met_Mad, digits =3)

## con moderatori meta regressione (uno alla volta)
met_Mad = rma(MA1$es1, MA1$var1, method="REML")
met_Mad
met_Mad_m1 = rma(MA1$es1, MA1$var1, method="REML", mods = formula (~ MA1$grade))
met_Mad_m1   ## tiene come riferimento il valore 1 (primaria)
met_Mad_m2 = rma(MA1$es1, MA1$var1, method="REML", mods = formula (~ MA1$week))
met_Mad_m2
met_Mad_m3 = rma(MA1$es1, MA1$var1, method="REML", mods = formula (~ MA1$exp))
met_Mad_m3 ## 1 = quasi-exp, 2 = exper
met_Mad_m4 = rma(MA1$es1, MA1$var1, method="REML", mods = formula (~ MA1$masch))
met_Mad_m4

##publication bias

funnel(met_Mad) ## In the absence of publication bias, it assumes that the largest
# studies will be plotted near the average.
## Asymmetry in the funnel may be indicative of publication bias

## Fail-safe N method. 
# estimates the number (N) of null studies that have to be added to reduce 
# the signicance of the meta-analysis to alpha (usually 0.05)
# Large values of N suggests robustness of results

trimfill (met_Mad)
funnel (trimfill (met_Mad))
fsn(yi = es1, vi = var1, alpha=.05, data= MA1)## Is the number (N = 302) of null studies that have
# to be added to reduce the significance large enough. 
## in nero effetti osservati, in bianco: studi ipotetici
funnel (trimfill (met_Mad))

## somma moderatori

somma1_2 = rma(MA1$es1, MA1$var1, method="REML", mods = formula (~ MA1$grade + MA1$week))
somma1_2 ## ordine di ingresso scelto in base all'importanza del predittore
# intercetta, valore di d con tutti i moderatori a 0
somma1_2_3 = rma(MA1$es1, MA1$var1, method="REML", mods = formula (~ MA1$grade + MA1$week + MA1$exp))
somma1_2_3
somma1_2_3_4 = rma(MA1$es1, MA1$var1, method="REML", mods = formula (~ MA1$grade + MA1$week + MA1$exp + MA1$masch))
somma1_2_3_4

## AIC
AIC(met_Mad, met_Mad_m1, met_Mad_m2, met_Mad_m3, met_Mad_m4, somma1_2, somma1_2_3,somma1_2_3_4)

## forest
forest(met_Mad_m2) ##  in grigio i valori attesi dal modello (in nero quelli osservati)


### plot moderatori
library(ggplot2)


plotcon(g = es1, var = var1, mod = week, data = MA1, method= "random",
        modname= "Weeks")

#################


########












