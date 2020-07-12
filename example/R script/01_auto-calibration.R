#---
# This script applies the auto-calibration funciton in "dycdTools" package
# to calibrate the DYRESM-CAEDYM model for temperature simulation in Lake Okareka.
# Three parameters were tried in this process.
# Author: SOngyan Yu
# Date created: 03/07/2020
#---

devtools::load_all("../../../R scripts/dycdtool3/")

autoCalibration(cal.para = "../R Data/Calibration parameters.csv",
                combination = "all",
                model.var = c("TEMP"),
                obs.data = "../R Data/Obs data_tempolate.csv",
                objective.function = c("nse"),
                start.date="2002-01-23",
                end.date="2016-12-31",
                dycd.wd = "../../../Sim_DYCD/191018-Okaraka_SY",
                dycd.output = "../../../Sim_DYCD/191018-Okaraka_SY/DYsim.nc",
                file_name = "../R output/auto-calibration.csv")
