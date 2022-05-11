# dycdtools package

[![CRAN status](https://www.r-pkg.org/badges/version/dycdtools)](https://cran.r-project.org/package=dycdtools)

# Overview
An R package for assisting calibration and visualising outputs of DYRESM-CAEDYM.

In the dycdtools package, there are two main function categories: calibration assistant and post-processing.

* The calibration assistant function ("calib.assist") carries out simulations with a large number of possible combinations of parameter values that users regard as potentially suitable for their model calibration, and calculates the values of nominated objective functions (i.e., statistical measures of goodness of fit) for each combination. Based on the calculated objective function values, users can determine the optimal set(s) of parameter values or narrow the ranges of possible parameter values. 

* Four post-processing functions provide multiple ways to visualise DYRESM-CAEDYM outputs as follows:

  * Function 'plot_cont' displays a heat map of variable values with depth within the water column and over time. This visualisation is particularly suitable for displaying temporal and depth dynamics of a variable at one lake site.
  * Function 'plot_prof' shows vertical profiles of the simulation and corresponding observations, for all dates where observations are available.
  * Function 'plot_ts' plots simulated values and observations for a specified variable and depth over time. It can be used to compare temporal changes of a variable for simulations and observations at specific depths.
  * Function 'plot_scatter' shows observations against simulated values for corresponding time and depth, with a colour scale representing measured depths. It can be used to demonstrate visually the goodness of fit for a variable across the water column.

A paper describing the package in detail and providing a case study is currently under review for publication. The DOI of the paper will be provided once it gets accepted for publication.

## Installation
To install the latest development version run the following code:
```{r}
# install devtools
install.packages("devtools")

# install dycdTools package
devtools::install_github("SongyanYu/dycdtools")

# Remove the package zip after installation
unlink("dycdtools.zip")
```

## Application
Below is a minimal case study example. The example data supporting the case study can be accessed via: https://github.com/SongyanYu/ExampleData_dycdtools.

### The calibration assistant function - calib.assist

Assume that you want to calibrate three model parameters: wind stirring efficiency, vertical mixing coefficient, and light extinction coefficient and each parameter have 4 possible values. The calib.assis function can be used to call DYRESM-CAEDYM to run for all possible combinations (n = 64) of the three parameters' values. For each model run, the objective function Nash-Sutcliffe Efficiency (NSE) coefficient is calcualted for temperature simulations.

```{r}
library(dycdtools)
calib.assist(cal.para = "calibration_data/Calibration_parameters.csv",
             combination = "all",
              model.var = c("TEMP"),
              obs.data = "calibration_data/Obs_data_template.csv",
              objective.function = c("NSE"),
              start.date = "2002-01-23",
              end.date = "2016-12-31",
              dycd.wd = "calibration_data/DYRESM_CAEDYM_Lake-Okareka/",
              dycd.output = "calibration_data/DYRESM_CAEDYM_Lake-Okareka/DYsim.nc",
              file_name = "calibration_data/Calibration_outputs.csv",
              write.out = TRUE,
              parallel = TRUE,
              verbose = TRUE)
```

## Visualise calibration results
Use a heatmap to visualise the calculated NSE values for all 64 model runs.
```{r}
# Read in model calibration results
calibration <- read.csv("calibration_data/Calibration_outputs.csv")

# Heat map
library(ggplot2)
ggplot(calibration, aes(x = wse,y = vmc,fill = NSE.TEMP)) +
       geom_tile() +
       scale_fill_distiller(palette = "PuBu", direction = 1) +
       facet_wrap(~lec, scales = "free") +
       xlab("Wind stirring efficiency") +
       ylab("Vertical mixing coefficient") +
       labs(title = "Light extinction coefficient", fill = "NSE") +
       theme_bw()  +
       theme(plot.title = element_text(size = 11, hjust = 0.5))
```

# Post-processing functions
Based on the 64 model runs, you can select a particular parameter value combination that generate the best performance (measured by NSE) and rerun DYRESM-CAEDYM for that particular parameter values. After that, you can use the post-processing functions in the dycdtools package to visualise the outputs of the calibrated model.

```{r}
# Extract temperature simulations
var.values <- ext.output(dycd.output = "DYCD_Okareka/DYsim.nc",
                         var.extract = c("TEMP"))

# Interpolation of temperature across water column at an interval of 0.5 m
temp.interpolated < -interpol(layerHeights = var.values$dyresmLAYER_HTS_Var,
                              var = var.values$dyresmTEMPTURE_Var,
                              min.dept = 0, max.dept = 33, by.value = 0.5)

# Read in observed water quality data
library(lubridate)
obs.okareka <- read.csv("plotting_data/Obs_data_template.csv")
obs.okareka$Date <- as.Date(obs.okareka$Date,format="%d/%m/%Y")
# subset observed data to remain temperature observations
obs.temp <- obs.okareka[, c('Date','Depth','TEMP')] 

# Contour plot
plot_cont_com(sim = temp.interpolated,
              obs = obs.temp,
              plot.start = "2002-01-23",
              plot.end = "2006-12-31",
              sim.start = "2002-01-23",
              sim.end = "2016-12-31"
              legend.title = "T\n(\u00B0C)",
              min.depth = 0,
              max.depth = 33,
              by.value = 0.5,
              nlevels = 20)
              
# Profile plot
plot_prof(sim = temp.interpolated,
          obs = obs.temp,
          sim.start = "2002-01-23",
          sim.end = "2016-12-31",
          plot.start = "2002-01-23",
          plot.end = "2002-12-31",
          min.depth = 0,
          max.depth = 33,
          by.value = 0.5,
          xlabel = "Temperature \u00B0C")
          
# Time series plot
plot_ts(sim = temp.interpolated,
        obs = obs.temp,
        target.depth = c(1, 14, 30),
        sim.start = "2002-01-23",
        sim.end = "2016-12-31",
        plot.start = "2002-01-23",
        plot.end = "2012-12-31",
        min.depth = 0,
        max.depth = 33,
        by.value = 0.5,
        ylabel = "Temperature \u00B0C")          

# Scatter plot
plot_scatter(sim=temp.interpolated,
             obs=obs.temp,
             sim.start="2002-01-23",
             sim.end="2016-12-31",
             plot.start = "2002-01-23",
             plot.end="2012-12-31",
             min.depth = 0,
             max.depth = 33,
             by.value = 0.5)
```

### Developer contact
Songyan (sunny) Yu: sunny.yu@griffith.edu.au
