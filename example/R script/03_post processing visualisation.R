#---
# This script visualise DYRESM-CAEDYM modelling outputs for Lake Okareka in four different ways.
# This script comes with the publicaiton of R package dycdTools.
# 1) contour plot
# 2) profile plot
# 3) time series plot
# 4) scatter plot
# Author: Songyan Yu
# Date created: 06/07/2020
#---

devtools::load_all("../../../R scripts/dycdtool3/")

# simulation period
library(lubridate)
sim.date<-seq.Date(from = as.Date("2002-01-23",format="%Y-%m-%d"),
                   to = as.Date("2016-12-31",format="%Y-%m-%d"),
                   by="day")

# DYCD temperature simulations using the optimal set of the three parameters
var.values<-extract.output(dycd.output = "../../../Sim_DYCD/191018-Okaraka_SY/DYsim.nc",
                           var.extract = c("TEMP"))

for(i in 1:length(var.values)){
  expres<-paste0(names(var.values)[i],"<-data.frame(var.values[[",i,"]])")
  eval(parse(text=expres))
}

# interpolation of temperature across water column at an intervals of 0.5m
temp.interpolated<-interpol(layerHeights = dyresmLAYER_HTS_Var,
                            var = dyresmTEMPTURE_Var,
                            min.dept = 0,max.dept = 33,by.value = 0.5)
#---
# Read in obs wq data
#---
obs.okareka<-read.csv("../R data/Obs data_tempolate.csv")
library(lubridate)
is.Date(obs.okareka$Date)
obs.okareka$Date<-as.Date(obs.okareka$Date,format="%d/%m/%Y")

obs.temp<-obs.okareka[,c(1,2,3)]

#---
# contour plot
#---
contour.plot(sim = temp.interpolated,
             obs = obs.temp,
             file_name = paste0("../R figures/contour_temp.png"),
             start.date="2002-01-01",
             end.date="2016-12-31",
             legend.title = "T\n(\u00B0C)",
             min.depth = 0,
             max.depth = 33,
             by.value = 0.5)

#---
# profile plot
#---
profile.plot(sim=temp.interpolated,
             obs = obs.temp,
             sim.start = "2002-01-23",
             sim.end = "2016-12-31",
             plot.start = "2002-01-23",
             plot.end = "2002-12-31",
             file_name = "../R figures/profile plot_temp.png",
             min.depth = 0,
             max.depth = 33,
             by.value = 0.5,
             xlabel = "Temperature \u00B0C",
             height = 4,
             width = 7)

#---
# time serise plot
#---
ts.plot(sim = temp.interpolated,
        obs = obs.temp,
        file_name=paste0("../R figures/TS_temp.png"),
        target.depth=c(1,14,30),
        sim.start="2002-01-23",
        sim.end="2016-12-31",
        plot.start = "2002-01-23",
        plot.end="2012-12-31",
        min.depth=0,
        max.depth=33,
        by.value=0.5,
        ylabel="Temperature \u00B0C",
        height = 4,
        width = 7)

#---
# scatter plot
#---
scatter.plot(sim=temp.interpolated,
             obs=obs.temp,
             sim.start="2002-01-23",
             sim.end="2016-12-31",
             plot.start = "2002-01-23",
             plot.end="2012-12-31",
             file_name = "../R figures/scatter plot_temp.png",
             min.depth = 0,
             max.depth = 33,
             by.value = 0.5,
             height = 4,
             width = 7)

