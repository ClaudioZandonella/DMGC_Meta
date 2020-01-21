# Meta-analysis 


### Summary

This R-project contains all the material and the scripts used to conduct the meta-analysis.

### Analysis reproducibility

To guarantee the reproducibility of the results, the whole analysis is structured within an `R-project` named `DMGC_Meta.Rproj` that is possible to download from this repository <span style="color:red">add repository link</span>.

The R-package`drake` (cite{landauDrakePackagePipeline2018}) is used to manage the analysis workflow and to enhance the readability and transparency of the analysis. To know more about `drake` consider the [official Git-hub page](https://github.com/ropensci/drake) or the [user manual](https://books.ropensci.org/drake/). Summarizing, using `drake` the code of the analysis is organized into different scripts. The user defines the plan of the analysis where each step in the analysis is defined trough functions. Functions can be can be appropriately defined to obtain desired targets (i.e., R-output with results of interests) and they are declared in another script. Subsequently, `drake` manage the whole analysis recognizing the dependency structure of the different targets. When any change is make to the code `drake` evaluate the analysis and update the results. Using function to define each step of the analysis allows to avoid "*coping and paste*" in the code, it makes debugging easier, and it facilitates the reading of the code.

Moreover, the R-package `renv` (cite{usheyRenvProjectEnvironments2019}) is used to manage the dependencies of the R-packages used in the analysis. The `renv` package allows to create an isolated, portable, and reproducible environment where the analyses are run. To know more about `renv` consider the [official documentation](https://rstudio.github.io/renv/articles/renv.html).

Finally, git version control was used to track the changes during the analysis.


### R-project structure

The R-project `DMGC_Meta.Rproj` is organized into different folders. In the folder `Data/`, the raw datasets with the information regarding the studies selected in the literature review are stored.

In the folder `R/`, the R-scripts used in the analysis are stored. Using the `drake` package the analysis is organized into different R-scripts files:

- [Settings.R](R/Settings.R) contains the setting for the R sessions, including R-packages used. 
- [Plan.R](R/Plan.R) contains the plan of the analysis. Where each target (i.e., R-output with results of interests) is defined through functions.
- [Function.R](R/Functions.R) contains the main functions used in \texttt{Plan.R} to obtain the targets of interest.
- [Auxiliary_functions.R](R/Auxiliary_functions.R) contains other functions used in the analysis.
- [Analysis.R](R/Analysis.R) is the script used to run the whole analysis.


In the folder `Report_analysis/`, it is possible to find the script used to compile the present report.


### Run the Analysis


In order to run the analysis follow these steps:

1. Make sure you have already the `renv` R-package installed in your library. If not, run the command in R or R-studio `install.packages("renv")`
2. Open the R-project `DMGC_Meta`  by double-clicking the file `DMGC_Meta.Rproj` you can find in the main directory. A new R-studio session should open and a similar message should appear in the console if `renv` was correctly installed:

    `* Project '~ <your_path>/DMGC_Meta' loaded. [renv <version_number>]`
3. Run the line `renv::restore()`, `renv` will ask the permission to install the R-packages used in the analysis, type `y` and return to confirm.
4. Open the file [R/Analysis.R](R/Analysis.R) and run each line of the sections "Load", "Check", and "Make".
5. Now you can access the targets with the results using the functions `drake::loadd(<name_target>)` and `drake::readd(<name_target>)`.



