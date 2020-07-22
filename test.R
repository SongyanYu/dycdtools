#---
# Script to test the dycdtools functions.
# Author: Songyan (Sunny) Yu
# Date Created: 01/04/2020
#---

#---
# 1. load all developed functions and internal data (like the "library" function)
#---
devtools::load_all() # the working directory should be in the top level package folder

#---
# 2. test autoCalibration function to auto-calibrate dycd parameters
#    takes 10-15 minutes to run
#---
autoCalibration(cal.para = "../Example data/Data/Calibration parameters.csv",
                combination = "random",
                n = 2,
                model.var = c("TEMP","DO","SALINITY","TN","TP","NO3","PO4","NH4","CHLA"),
                phyto.group = c("NODUL","CYANO"),
                obs.data = "../Example data/Data/Obs LHM.csv",
                objective.function = c("nse"),
                start.date = "2017-06-06",
                end.date = "2020-02-29",
                dycd.wd = "../Example data/200318-lhm-ref/",
                dycd.output = "../Example data/200318-lhm-ref/DYsim.nc",
                file_name = "../Example data/Data/auto-calibration.csv")

#---
# 3. test the function of extracting dycd output
#---
var.values<-extract.output(dycd.output = "../Example data/200318-lhm-ref/DYsim.nc",
                           var.extract = c("TEMP"))

for(i in 1:length(var.values)){
  expres<-paste0(names(var.values)[i],"<-data.frame(var.values[[",i,"]])")
  eval(parse(text=expres))
}

#---
# 4. test post-processing functions for visualising dycd simulations
#---

# 4.1 Contour map of temperature simulations

# interpolation of temperature simulations from depth=0 to depth=maximum.
temp.interpolated<-interpol(layerHeights = dyresmLAYER_HTS_Var,
                            var = dyresmTEMPTURE_Var,
                            min.dept = 0,max.dept = 13,by.value = 0.5)

contour.plot(var.matrix = temp.interpolated,
             file_name = "../Example data/Figure/contour_TEMP.png",
             start.date="2017-06-06",end.date="2020-02-29",
             legend.title = "Temperature\n(\u00B0C)",
             min.z = min(temp.interpolated),
             max.z = max(temp.interpolated),
             min.depth = 0,max.depth = 13,by.value = 0.5)

# Before testing other plot functions, read in observed water quality data.
obs.lhm<-read.csv("../Example data/Data/Obs LHM.csv")
obs.lhm$Date<-as.Date(obs.lhm$Date,format="%d/%m/%Y")
obs.temp<-obs.lhm[,c(1,2,3)]

# 4.2 scatter plot of temperature
scatter.plot(sim=temp.interpolated,
             obs=obs.temp,
             start.date="2017-06-06",end.date="2020-02-29",
             file_name = "../Example data/Figure/Scatter plot_TEMP.png",
             min.depth = 0,max.depth = 13,by.value = 0.5)

# 4.3 profile plot of temperature
profile.plot(sim=temp.interpolated,
             obs = obs.temp,
             file_name = "../Example data/Figure/profile_TEMP.png",
             xlabel = "Temperature \u00B0C",
             min.depth = 0,max.depth = 13,by.value = 0.5)

# 4.4 time serise plot
ts.plot(sim = temp.interpolated,
        obs = obs.temp,
        file_name="../Example data/Figure/TS_TEMP.png",
        target.depth=c(0,10),
        start.date="2017-06-06",end.date="2020-02-29",
        min.depth=0,
        max.depth=13,
        by.value=0.5,
        ylabel="Temperature \u00B0C")

