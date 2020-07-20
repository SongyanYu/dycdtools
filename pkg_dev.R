#---
# Code to use "usethis" package to prepare dycdtool package.
# Author: Songyan Yu
# Date Created: 29/03/2020
#---

library(usethis)

create_package(path="dycdtool3")
# then move the newly opened R session to run the following code.

# ingore some R scripts
use_build_ignore("pkg_dev.R")
use_build_ignore("test.R")
use_build_ignore("example")
use_build_ignore(".Rhistory")
use_build_ignore("data/output_name.csv")
use_build_ignore("cran-comments.md")

# add package imported
use_package("dplyr",type = "Imports")
use_package("ncdf4",type = "Imports")
use_package("tidyr",type = "Imports")
use_package("hydroGOF",type = "Imports")
use_package("ggplot2",type = "Imports")
use_package("lubridate",type="Imports")
use_package("RColorBrewer",type = "Imports")

# add data
output_name<-read.csv("data/output_name.csv")
use_data(output_name,overwrite = TRUE)

# license
use_mit_license(name = "MIT")

library(devtools)

devtools::load_all()
devtools::document()

devtools::check()

# create R file
use_r(name = "test.R")

# run R CMD check on CRAN's servers
devtools::build()


