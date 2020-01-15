
################################
####    Project settings    ####
################################

#----     R packages   ----

packages_list <- c("conflicted",
                   "tidyverse",
                   "metafor",
                   "clubSandwich",
                   "drake",
                   "gridExtra",
                   "MAd",
                   "visNetwork")


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

# renv::settings$snapshot.type("simple")
# renv::purge()
# renv::hydrate("formatR")
# sapply(packages_list, renv::hydrate)
# renv::remove()
# renv::install("drake")
# renv::snapshot()
# renv::install
# renv::dependencies()




#---- function conflicts   ----
# conflicted::conflict_scout()

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("gather", "tidyr")



#-----    ggplot2 settings    ----
theme_set(theme_bw())
