## Test environments
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There was 1 NOTE.

cont.plot.comp: no visible binding for global variable 'obs_temp'
cont.plot.comp: no visible binding for global variable 'Value'
Undefined global functions or variables:
Value obs_temp

* The variable of "value" is a column name that the cont.plot.comp function gives to the data frame;
* The variable of "obs_temp" is an internal data set used in the executable example.

## R-hub check results
There was 2 NOTE

* checking CRAN incoming feasibility ... NOTE

  CAEDYM (2:25, 15:31)
  DYRESM (2:18, 15:24)
Maintainer: 'Songyan Yu <sunny.yu@griffith.edu.au>'
New submission
Possibly mis-spelled words in DESCRIPTION:

* I can confirm that the two words CAEDYM, DYRESM are not mis-spelled. They are the names of two lake models.

* checking R code for possible problems ... NOTE
  cont.plot.comp: no visible binding for global variable 'Value'
  Undefined global functions or variables:
    Value obs_temp

* Same note as in the R CMD check. Please see my previous response.

## This is a re-submission after addressing the issues raised by Martina Schmirl from CRAN.

## Response to CRAN feedback
* CRAN: Please add small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing.
Authors: We have added executable exmaples for each of exported functions, except for the auto-calibration function as it requires users to have DYRESM-CAEDYM model executables (.exe).

* CRAN: Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()).In your examples/vignettes/tests you can write to tempdir().
Authors: We have added a new argument "plot.save" in the exported plotting fucntions (e.g. cont.plot.comp, scatter.plot)to allow users to decide whether to save the plot or not.

* CRAN: You write information messages to the console that cannot be easily suppressed.
* It is more R like to generate objects that can be used to extract the 
* information a user is interested in, and then print() that object.
* Instead of print()/cat() rather use message()/warning() or 
* if(verbose)cat(..) (or maybe stop()) if you really have to write text to 
* the console.(except for print, summary, interactive functions)
Authors: We have added a "verbose" argument in such functions like "autoCalibration" to allow users to choose to print function processing information or not.

* CRAN: Please make sure that you do not change the user's options, par or 
* working directory. If you really have to do so within functions, please 
* ensure with an immediate call of on.exit() that the settings are reset 
* when the function is exited. e.g.:
* ...
* oldpar <- par(no.readonly = TRUE) # code line i
* on.exit(par(oldpar)) # code line i + 1
* ...
* par(mfrow=c(2,2)) # somewhere after
* ...
*
* ...
* oldwd <- getwd() # code line i
* on.exit(setwd(oldwd)) # code line i+1
* ...
* setwd(...) # somewhere after
* ...
* If you're not familiar with the function, please check ?on.exit. This 
* function makes it possible to restore options before exiting a function 
* even if the function breaks. Therefore it needs to be called immediately 
* after the option change within a function.
Authors: This comment is related to the "autoClibration" function.
We do need to change the working directory during the function processing.
We have added "on.exit" in the function. Thanks for the advice.

* CRAN: Please always add all authors, contributors and copyright holders in the 
* Authors@R field with the appropriate roles.
* e.g. MIT from the license file ?
Authors: We have changed the "MIT" in the license file to be "Songyan Yu" as the copyright holder.
