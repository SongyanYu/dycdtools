#' Contour plot of a variable simulation, with observed data shown as dots in the generated contour plot.
#'
#' @description
#' Contour plot a matrix of a bio-geochemical variable values, which can be generated through "interpol" function.
#'
#' @param sim a matrix of simulated variables that have been interpolated
#' @param obs observed values of variable.
#' @param file_name the file path to save the generated contour figure.
#' @param sim.start,sim.end the start and end of the simulation period for the DYRESM-CAEDYM model run of interest. The date format must be "\%Y-\%m-\%d".
#' @param plot.start,plot.end the start and end of the plot period, in the format of "\%Y-\%m-\%d"
#' @param legend.title the legend title of the contour figure.
#' @param min.depth,max.depth,by.value minimum and maximum depth used to be the start of y axis of the contour plot, at the increment of by.value.
#' @param nlevels a set of levels which are used to partition the range of simulation variable.
#' @param height,width the relative height/width of the figure.
#' @param ppi the ppi value of the figure.
#' @param plot.save if TRUE, the plot is saved with the "height","width", and "ppi" parameters.
#'
#' @importFrom grDevices hcl.colors png dev.off
#' @importFrom graphics axis filled.contour mtext par points title
#' @importFrom lubridate year
#' @return a graph file of contour plot saved in the Figure folder.
#'
#' @examples
#' # extract simulated temperature values from DYRESM-CAEDYM simulation file
#'  var.values<-ext_output(dycd.output=system.file("extdata", "dysim.nc", package = "dycdtools"),
#'                        var.extract=c("TEMP"))
#'
#'   for(i in 1:length(var.values)){
#'      expres<-paste0(names(var.values)[i],"<-data.frame(var.values[[",i,"]])")
#'      eval(parse(text=expres))
#'   }
#'
#' # interpolate temperature for depths from 0 to 13 m at increment of 0.5 m
#'   temp.interpolated<-interpol(layerHeights = dyresmLAYER_HTS_Var,
#'                              var = dyresmTEMPTURE_Var,
#'                              min.dept = 0,max.dept = 13,by.value = 0.5)
#'
#'   data(obs_temp)
#' # contour plot of temperature simulations with observed data shown as colour-coded dots
#'   plot_cont_comp(sim=temp.interpolated,
#'                  obs=obs_temp,
#'                  sim.start = "2017-06-06",
#'                  sim.end = "2017-06-15",
#'                  plot.start="2017-06-06",
#'                  plot.end="2017-06-15",
#'                  legend.title="T \u00B0C",
#'                  min.depth=0,max.depth=13,by.value=0.5,
#'                  nlevels=20,
#'                  plot.save=FALSE,
#'                  file_name="Contour_temp.png",
#'                  height=5,width=8,ppi=150)
#'
#' @export

plot_cont_comp<-function(sim=temp.interpolated,
                         obs=obs_temp,
                         file_name="Contour_temp.png",
                         sim.start = "2002-01-23",
                         sim.end = "2016-12-31",
                         plot.start="2017-06-06",
                         plot.end="2017-06-15",
                         legend.title="T \u00B0C",
                         min.depth=0,max.depth=13,by.value=0.5,
                         nlevels=20,
                         plot.save=TRUE,
                         height=5,width=8,ppi=150){

  #---
  # 1. simulation period
  #---
  plot.date<-seq.Date(from = as.Date(plot.start,format="%Y-%m-%d"),
                      to = as.Date(plot.end,format="%Y-%m-%d"),
                      by="day")

  sim.date<-seq.Date(from = as.Date(sim.start,format="%Y-%m-%d"),
                     to = as.Date(sim.end,format="%Y-%m-%d"),
                     by="day")

  index<-match(seq(lubridate::year(plot.date)[1],lubridate::year(plot.date)[length(plot.date)],by=1),lubridate::year(plot.date))

  plot.sim<-temp.interpolated[,c(match(plot.date[1],sim.date):match(plot.date[length(plot.date)],sim.date))]

  levels<-pretty(range(plot.sim,obs[,3],na.rm = TRUE),nlevels)
  color.palette <- function(n)hcl.colors(n,"RdBu",rev=TRUE)
  colour<-unlist(lapply(obs[,3],FUN = function(x) color.palette(length(levels)-1)[length(which(levels<x))]))

  colnames(obs)<-c("Date","Depth","Value")
  obs<-obs%>%
    filter(Date>=plot.date[1]&Date<=plot.date[length(plot.date)],
           !is.na(Value))%>%
    mutate(Date=as.Date(Date,format="%Y-%m-%d"))

  #---
  # 2. contour plot the var matrix
  #---
  if(plot.save){
    png(filename = file_name,height = height*ppi,width = width*ppi)
  }

  filled.contour(x=seq(1,ncol(plot.sim),by=1),
                 y=seq(min.depth,max.depth,by=by.value),
                 z=t(plot.sim),
                 ylim = c(max.depth,min.depth),
                 zlim = c(min(plot.sim,obs[,3],na.rm=TRUE),max(plot.sim,obs[,3],na.rm = TRUE)),
                 nlevels = nlevels,
                 color.palette = function(n)hcl.colors(n,"RdBu",rev=TRUE),
                 plot.axes = {
                   axis(3,mgp=c(1,0.25,0),tcl=-0.1, cex.axis=1.8, lwd=0.5,
                        at=seq(1,ncol(plot.sim),by=1)[c(1,index)], labels=lubridate::year(plot.date[c(1,index)]))
                   axis(2,mgp=c(1,0.4,0),tcl=-0.2, cex.axis=1.8, cex.lab=0.8,lwd=0.5)
                   mtext("Depth (m)",side=2,line=1.3,las=0,cex = 1.8)
                   points(x=match(obs$Date,plot.date),y=obs$Depth,bg=colour,pch=21, cex=1.3)},
                 key.title = {
                   par(cex.main=1.3); title(main=legend.title)
                 })
  if(plot.save){
    dev.off()
  }
}
