
################################
####    Project settings    ####
################################

#----     R packages   ----
library(conflicted)
library(tidyverse)
library(metafor)
library(clubSandwich)
library(knitr)
library(kableExtra)
library(drake)


# renv::hydrate("drake")

#---- function conflicts   ----

conflict_prefer("filter", "dplyr")
conflict_prefer("gather", "tidyr")

# conflict_scout()

#-----    ggplot2 settings    ----
theme_set(theme_bw())
