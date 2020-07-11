#---
# Code to use "usethis" package to prepare dycdtool package.
# Author: Songyan Yu
# Date Created: 29/03/2020
#---

library(usethis)

create_package(path="dycdtool3")
# then move the newly opened R session to run the following code.

# ingore some R scripts
use_build_ignore("R/Tutorial_usethis.R")
use_build_ignore("R/05_TempValidation.R")
use_build_ignore("R/package_dev.R")

# add package imported
use_package("dplyr",type = "Imports")
use_package("ncdf4",type = "Imports")
use_package("tidyr",type = "Imports")
use_package("hydroGOF",type = "Imports")
use_package("ggplot2",type = "Imports")

# add data
output_name<-read.csv("../dycdTools/Data/Output variables.csv")
use_data(output_name,overwrite = TRUE)


library(devtools)

devtools::load_all()
devtools::document()

# create R file
use_r(name = "test.R")



