# Meta-analysis 


### Summary

This R-project contains all the material and the scripts used to conduct the meta-analysis.

### Replicability: drake, renv and git

In order to guarantee the replicability of the results, the Analysis was carried in R using the `drake` R-package. The `drake` package allows to organize the workflow of the analysis and to easily reproduce the results. To know more about `drake` consider the [official Github-page](https://github.com/ropensci/drake) or the [user manual](https://books.ropensci.org/drake/). In summary, using `drake` the code of the analysis is separated into different scripts. The user defines the plan of the analysis where targets (i.e., R-output with results of interests) are obtained through personalized functions that are declared in another script. Subsequently, `drake` can run the whole analysis.

Moreover, the R-package `renv` was used to manage the dependencies of the R-packages used in the analysis. The `renv` package allows to create an isolated, portable, and reproducible environment where the analyses are run. To know more about `renv` consider the [official documentation](https://rstudio.github.io/renv/articles/renv.html).

Finally, git version control was used to track the changes during the analysis.

### Folder structure

In the folder `Data/`, the raw datasets are saved with the information regarding the studies selected in the literature review.

In the folder `R/`, the R-scripts used in the analysis are saved. Using the `drake` package the analysis is organized into different R-scripts files: 

- [Settings.R](R/Settings.R) contains the setting for the R sessions, including R-packages used. 
- [Plan.R](R/Plan.R) contains the plan of the analysis. Where each target (i.e., R-output with results of interests) is defined through functions.
- [Function.R](R/functions.R) contains the main functions used in `Plan.R` to obtain the targets of interest.
- [Auxiliary_functions.R](R/Auxiliary_functions.R) contains other functions used in the analysis.
- [Analysis.R](R/Analysis.R) is the script used to run the whole analysis.


### Run the Analysis

In order to run the analysis follow these steps:

1. Open R/R-studio and install the package "renv": `install.packages("renv")`; then close R/R-studio.
2. Open the R-project "DMGC_Meta"  by double-clicking the file `DMGC_Meta.Rproj` in the folder.
3. Run the line `renv::restore()` to install the same version packages used in the original analysis.
4. Open the file `R/Analysis.R` and run each line of the sections "Load", "Check", and "Make".
5. Now you can access the targets with the results using the functions `drake::loadd(name_target)` and `drake::readd(name_target)`.


