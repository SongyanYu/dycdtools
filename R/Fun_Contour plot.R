#' Contour plot of a biogeochemical variable.
#'
#' @description
#' Contour plot a matrix of a biogeochemical variable values, which can be generated through "interpol" function.
#'
#' @param sim a matrix of simulated variabls that have been interpolated
#' @param obs observed values of variable.
#' @param file_name the file path to save the generated contour figure.
#' @param start.date,end.date the beginning and ending simulation dates for the intended DYRESM-CAEDYM model run. The date format must be "%Y-%m-%d".
#' @param legend.title the legend title of the contour figure.
#' @param min.depth,max.depth minimum and maximum depth used to be the start of y axis of the contour plot.
#' @param height,width the relative height/width of the figure.
#' @param ppi the ppi value of the figure.
#'
#' @import hydrostats
#' @import lubridate
#'
#' @return a graph file of contour plot saved in the Figure folder.
#'
#' @export

contour.plot<-function(sim=temp.interpolated,
                       obs=obs.temp,
                       file_name="Contour_temp.png",
                       start.date="2017-06-06",end.date="2020-02-29",
                       date=NULL,
                       legend.title="T\n(\u00B0C)",
                       min.depth=0,max.depth=33,by.value=0.5,
                       nlevels=20,
                       height=5,width=8,ppi=150){

  #---
  # 1. simulation period
  #---
  if(is.null(date)){
    date<-seq.Date(from = as.Date(start.date,format="%Y-%m-%d"),
                       to = as.Date(end.date,format="%Y-%m-%d"),
                       by="day")
  }

  index<-match(seq(year(date)[1],year(date)[length(date)],by=1),year(date))

  range(sim)
  range(obs[,3],na.rm = TRUE)
  levels<-pretty(range(sim,obs[,3],na.rm = TRUE),nlevels)
  #max(range(obs$TEMP,na.rm = TRUE),range(sim))
  #max(obs$TEMP,sim,na.rm = TRUE)
  #range(range(obs$TEMP,na.rm = TRUE),range(sim))
  color.palette <- function(n)hcl.colors(n,"RdBu",rev=TRUE)
  colour<-unlist(lapply(obs[,3],FUN = function(x) color.palette(length(levels)-1)[length(which(levels<x))]))

  obs=na.omit(obs)
  #---
  # 2. contour plot the var matrix
  #---
  png(filename = file_name,height = height*ppi,width = width*ppi)
  filled.contour(x=seq(1,ncol(sim),by=1),
                 y=seq(min.depth,max.depth,by=by.value),
                 z=t(sim),
                 ylim = c(max.depth,min.depth),
                 zlim = c(min(sim,obs[,3],na.rm=TRUE),max(sim,obs[,3],na.rm = TRUE)),
                 nlevels = nlevels,
                 color.palette = function(n)hcl.colors(n,"RdBu",rev=TRUE),
                 plot.axes = {
                   axis(3,mgp=c(1,0.25,0),tcl=-0.1, cex.axis=1.8, lwd=0.5,
                        at=seq(1,ncol(sim),by=1)[c(1,index)], labels=year(date[c(1,index)]))
                   axis(2,mgp=c(1,0.4,0),tcl=-0.2, cex.axis=1.8, cex.lab=0.8,lwd=0.5)
                   mtext("Depth (m)",side=2,line=1.3,las=0,cex = 1.8)
                   points(x=match(obs$Date,date),y=obs$Depth,bg=colour,pch=21, cex=1.3)},
                 key.title = {
                   par(cex.main=1.3); title(main=legend.title)
                 })
  dev.off()
}
