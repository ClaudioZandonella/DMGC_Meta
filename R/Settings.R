
################################
####    Project settings    ####
################################

#----     R packages   ----

packages_list <- c("conflicted",
                   "tidyverse",
                   "metafor",
                   "clubSandwich",
                   "knitr",
                   "kableExtra",
                   "drake",
                   "gridExtra")

# load packages
lapply(packages_list,require, character.only = TRUE)


#----    Procedure to remove packages   -----
# ip <- as.data.frame(installed.packages())
# ip <- subset(ip, !grepl("MRO", ip$LibPath))
# ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# path.lib <- unique(ip$LibPath)
# pkgs.to.remove <- ip[,1]
# 
# sapply(pkgs.to.remove, renv::remove, lib = path.lib)

#----    renv comands    ----

# renv::purge()
# renv::hydrate("renv")
# renv::remove()
# renv::snapshot()

#---- function conflicts   ----
# conflict_scout()

conflict_prefer("filter", "dplyr")
conflict_prefer("gather", "tidyr")



#-----    ggplot2 settings    ----
theme_set(theme_bw())
