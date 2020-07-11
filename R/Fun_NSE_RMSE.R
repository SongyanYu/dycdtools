#' Calculate fit of goodness of DYCD model simulations.
#'
#' @description
#' Two objective functions can be calculated: 1) Nash-Sutcliffe efficiency coefficient (NSE),
#'   and 2) Root Mean Square Error (RMSE).
#'
#' @param sim a matrix of biogeochemical varaiable values with column of time and row of depth.
#' @param obs a data frame of observed value, with three columns: Date, depth, value.
#' @param min.dept,max.depth minimum and maximum depth to be compared between simulations and observations.
#' @param by.value the value of increment for depth.
#'
#' @import dplyr
#' @import tidyr
#' @import hydroGOF
#'
#' @return a vector of objective function values. The first is NSE and the second is RMSE.
#'
#' @export

nse.rmse<-function(sim,
                   obs,
                   start.date="2017-06-06",end.date="2020-02-29",
                   min.dept=0,max.dept=33,by.value=0.5){

  #---
  # 1. simulation period
  #---
  sim.date<-seq.Date(from = as.Date(start.date,format="%Y-%m-%d"),
                     to = as.Date(end.date,format="%Y-%m-%d"),
                     by="day")

  #---
  # 2. combine sim with obs by Date and Depth
  #---
  sim.var<-as.data.frame(sim)
  colnames(sim.var)<-sim.date
  sim.var$Depth<-seq(min.dept,max.dept,by=by.value)

  var.both<-sim.var%>%
    pivot_longer(-Depth,names_to = "Date",values_to = "sim")%>%
    mutate(Date=as.Date(Date,format="%Y-%m-%d"))%>%
    right_join(.,obs,by=c("Date","Depth"))

  colnames(var.both)[4]<-"obs"




  var.both%>%
    mutate(Date=as.Date(Date,format="%Y-%m-%d"))
  #---
  # 3. calculate nse and rmse to be stored in a vector
  #---
  nse.var<-NSE(var.both$sim,obs=var.both$obs)
  rmse.var<-sqrt(mean((var.both$sim-var.both$obs)^2,na.rm = TRUE))
  value<-c(nse.var,rmse.var)

  return(value)
}
