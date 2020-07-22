# dycdtools package

An R package for auto-calibration and output visualisation of DYRESM-CAEDYM.

* The auto-calibration function tries a large number of possible combinations of parameter values that users regard as potentially suitable for their model calibration, and calculates the values of nominated objective functions for each combination. 

* Four post-processing functions are included to visualise simulation results:

  * Scatter plot
  * Contour plot
  * Time series plot
  * Profile plot

## Installation
To install the latest development version run the following code:
```{r}
# install devtools
install.packages("devtools")

# install dycdTools package
devtools::install_github("SongyanYu/dycdTools")

# Remove the package zip after installation
unlink("dycdTools.zip")
```

### developer contact
Songyan (sunny) Yu: sunny.yu@griffith.edu.au
