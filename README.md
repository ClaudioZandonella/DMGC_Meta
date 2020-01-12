# Meta-analysis 


### Summary

This R-project contain all the material and analysis used to conduct the meta-analysis.

### Replicability: drake, renv and git

In order to guarantee the replicability of the results, the Analysis were carried in R using the `drake` R-package. The `drake` package allows to organize the workflow of the analysis and to easily reproduce the results. To know more about `drake` consider the [official Github-page](https://github.com/ropensci/drake) or the [user manual](https://books.ropensci.org/drake/). In summary, using `drake` the user write the plan of the analysis where targets (i.e., R-output with results of interests) are obtained through functions, subsequently `drake` can run the whole analysis.

Moreover, the R-package `renv` was used to manage the depandecies of the R-packages used in the analysis. The `renv` package allows to create an isolated, portable and reproducilble eviroment where the analyses are run. To know more about `renv` consider the [official documentation](https://rstudio.github.io/renv/articles/renv.html).

Finally, git version control was used to track the changes during the analysis.

### Folder structure

In the folder `Data/` the raw datasets are saved with the infromations regarding the studies selected in the litterature review.

In the folder `R/` the R-scripts used in the analysis are saved. Using the `drake` package the analysis is organized into different R-scripts files. 

- [Settings.R](R/Settings.R) contains the setting for the R sessions, including the R- packages used. 
- [Plan.R](R/Plan.R) contains the plan of the analysis. Where each target (ie., R-output with results of interests) is defined throught functions.
- [Function.R](R/functions.R) contains the main functions used in `Plan.R` to obtain the targets of interest.
- [Auxiliary_functions.R](R/Auxiliary_functions.R) contains other functions used in the analysis.
- [Analysis.R](R/Analysis.R) is the script used to run the whole analysis.





