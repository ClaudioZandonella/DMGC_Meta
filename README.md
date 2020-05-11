# Supplemental material of tha article “The effects of educational video-games on students motivation to math: A meta-analysis in K-12”.


### Summary

This repository contains all the supplemantal material and the analysis scripts used in the article *“The effects of educational video-games on students motivation to math: A meta-analysis in K-12”*. The aim of the meta-analysis was to synthesize the results of the studies in the literature, concerning the impact of educational video-games on students’ motivation towards mathematics.

### Analysis reproducibility

To guarantee the reproducibility of the results, the whole analysis is structured within an `R-project` named `DMGC_Meta.Rproj` that is possible to download from this repository <span style="color:red">add repository link</span>.

The R-package`drake` is used to manage the analysis workflow and to enhance the readability and transparency of the analysis. To know more about `drake` consider the [official Git-hub page](https://github.com/ropensci/drake) or the [user manual](https://books.ropensci.org/drake/). Summarizing, using `drake` the code of the analysis is organized into different scripts. The user defines the plan of the analysis where each step in the analysis is defined trough functions. Functions can be appropriately defined to obtain desired targets (i.e., R-output with results of interests) and they are declared in another script. Subsequently, `drake` manages the whole analysis recognizing the dependency structure of the different targets. When any change is made to the code `drake` evaluates the analysis and updates the results. Using functions to define each step of the analysis allows to avoid "*coping and paste*" in the code, it makes debugging easier, and it facilitates the reading of the code.

Moreover, the R-package `renv` is used to manage the dependencies of the R-packages used in the analysis. The `renv` package allows to create an isolated, portable, and reproducible environment where the analyses are run. However, `renv` is limited as it can not handle different R versions. Thus, to be fully reproducible users should use R version 3.6.1. To know more about `renv` consider the [official documentation](https://rstudio.github.io/renv/articles/renv.html).

Finally, git version control was used to track the changes during the analysis.


### R-project structure

The R-project `DMGC_Meta.Rproj` is organized into different folders. In the folder `Data/`, the raw datasets with the information regarding the studies selected in the literature review are stored.

In the folder `R/`, the R-scripts used in the analysis are stored. Using the `drake` package the analysis is organized into different R-scripts files:

- [Settings.R](R/Settings.R) contains the setting for the R sessions, including R-packages used. 
- [Plan.R](R/Plan.R) contains the plan of the analysis. Where each target (i.e., R-output with results of interests) is defined through functions.
- [Function.R](R/Functions.R) contains the main functions used in \texttt{Plan.R} to obtain the targets of interest.
- [Auxiliary_functions.R](R/Auxiliary_functions.R) contains other functions used in the analysis.
- [Analysis.R](R/Analysis.R) is the script used to run the whole analysis.


In the folder `Documents/`, it is possible to find the material used to compile the [Analysis Report](Documents/Report_analysis/Report_analysis.pdf) (see folder `Report_analysis`) and the [Prisma flow diagram](Documents/Prisma/Prisma.pdf) (see folder `Prisma`).


### Run the Analysis


To run the analysis you can follow two options: **A)** recreate a more reproducible enviroment using the same R version and packages versions; **B)** install the required packages and run the analysis.

**Option A: reproducible enviroment using `renv`.**

1.  Install R version 3.6.1 from CRAN and select it as the the R version used in RStudio (more info at this [link](https://support.rstudio.com/hc/en-us/articles/200486138-Changing-R-versions-for-RStudio-desktop).
2. Make sure you have already the `renv` R-package installed in your library. If not, run the command in R or R-studio `install.packages("renv")`
3. Open the R-project `DMGC_Meta`  by double-clicking the file `DMGC_Meta.Rproj` you can find in the main directory. A new R-studio session should open and a similar message should appear in the console if `renv` was correctly installed:

    `* Project '$\sim$/<your\_path>/DMGC\_Meta' loaded. [renv <version\_number>]`
4. Run the line `renv::restore()`, `renv` will ask the permission to install the R-packages used in the analysis, type `y` and return to confirm. If this fails, it could be due to `renv` attempting to install packages from sources but system prerequisites for compilation of a package are missing. In this case run `renv::equip()` and try again (only supported on Windows; if using other operative systems ensure to have the required development tools). If this does not solve the problem, just run the analysis following option B described below.}
5. Open the file [R/Analysis.R](R/Analysis.R) and run each line of the sections "Load", "Check", and "Make".
6. Now you can access the targets with the results using the functions `drake::loadd(<name_target>)` and `drake::readd(<name_target>)`.

**Option B: install required packages.**

1.  Open the R-project `DMGC_Meta`  by double-clicking the file `DMGC_Meta.Rproj` you can find in the main directory. A new R-studio session should open.
2. Install the required packages running the line `install.packages(c("conflicted",  "tidyverse", "metafor", "clubSandwich", "drake", "gridExtra", "MAd", "visNetwork", "robumeta"))`
3. Open the file [R/Analysis.R](R/Analysis.R) and run each line of the sections "Load", "Check", and "Make".
4. Now you can access the targets with the results using the functions `drake::loadd(<name_target>)` and `drake::readd(<name_target>)`.



