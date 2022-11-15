#' A post-processing function used to visualise model output in a profile graph.
#'
#' @description Profile plot shows vertical profiles of simulation outputs and
#'  corresponding observations for all dates where observations are available.
#'
#' @param sim a matrix of simulated variables. This matrix can be generated
#' by running the "interpol" function.
#'
#' @param obs a data frame having three columns to describe observed values of
#'  a water quality variable. These three columns are 'Date' (as '\%Y-\%m-\%d'),
#'  'Depth', and the designated variable name which can be found from the
#'  var.name column of 'data(output_name)'.
#'  An example of such a data frame can be found with 'data(obs_temp)'
#'  This function is based on ggplot2, and users can treat the object of this
#' function in the same way as a ggplot2 object.
#'
#' @param sim.start,sim.end the start and end dates of the simulation period
#' of the DYRESM-CAEDYM model run of interest.
#' The date format must be "\%Y-\%m-\%d".
#'
#' @param plot.start,plot.end the start and end dates of the period to be
#'  plotted in the format of "\%Y-\%m-\%d".
#'
#' @param xlabel the x axis label of the profile figure.
#'
#' @param min.depth,max.depth,by.value minimum and maximum depths in the profile
#'  plot at an increment of by.value.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return This function returns a ggplot object that can be modified
#' with ggplot package functions.
#'
#' @examples
#'  var.values<-ext_output(dycd.output=system.file("extdata", "dysim.nc",
#'                                                  package = "dycdtools"),
#'                        var.extract=c("TEMP"))
#'
#'  for(i in 1:length(var.values)){
#'    expres<-paste0(names(var.values)[i],"<-data.frame(var.values[[",i,"]])")
#'    eval(parse(text=expres))
#'   }
#'
#' # interpolate temperature for depths from 0 to 13 m at increment of 0.5 m
#'   temp.interpolated<-interpol(layerHeights = dyresmLAYER_HTS_Var,
#'                                var = dyresmTEMPTURE_Var,
#'                                min.dept = 0, max.dept = 13, by.value = 0.5)
#'
#'  data(obs_temp)
#' # profile plot of temperature sim and obs
#'   p <- plot_prof(sim=temp.interpolated,
#'             obs = obs_temp,
#'             sim.start="2017-06-06",
#'             sim.end="2017-06-15",
#'             plot.start="2017-06-06",
#'             plot.end="2017-06-15",
#'             xlabel = "Temperature \u00B0C",
#'             min.depth = 0, max.depth = 13, by.value = 0.5)
#'  p
#'
#' @export

plot_prof<-function(sim,
                    obs,
                    sim.start,
                    sim.end,
                    plot.start,
                    plot.end,
                    xlabel,
                    min.depth,
                    max.depth,
                    by.value){

  #---
  # 1. simulation period
  #---

  if(any(is.na(ymd(plot.start, quiet = TRUE)),
         is.na(ymd(plot.end, quiet = TRUE)),
         is.na(ymd(sim.start, quiet = TRUE)),
         is.na(ymd(sim.end, quiet = TRUE)))){

    stop('Make sure date format is \'%Y-%m-%d\'\n')

  }

  sim.date <- seq.Date(from = as.Date(sim.start, format = "%Y-%m-%d"),
                     to = as.Date(sim.end, format = "%Y-%m-%d"),
                     by="day")

  #---
  # 2. combine sim with obs by Date and Depth
  #---
  sim.temp<-as.data.frame(sim)
  colnames(sim.temp)<-sim.date
  sim.temp$Depth<-seq(min.depth,max.depth,by= by.value)

  colnames(obs)<-c("Date","Depth","Value")
  obs<-obs%>%
    mutate(Date=as.Date(Date,format="%Y-%m-%d"))

  temp.both<-sim.temp%>%
    pivot_longer(-Depth,names_to = "Date",values_to = "sim")%>%
    mutate(Date=as.Date(Date,format="%Y-%m-%d"))%>%
    right_join(.,obs,by=c("Date","Depth"))%>%
    filter(Date>=plot.start&Date<=plot.end)

  colnames(temp.both)[4]<-"obs"

  #---
  # 3. profile plot sim vs. obs, faceted by Date
  #---
  p <- temp.both %>%
    ggplot() +
    geom_point(aes(y = Depth, x = obs), col = "red") +
    geom_path(aes(y = Depth, x = sim)) +
    facet_wrap(~Date) +
    ylim(max.depth, min.depth) +
    xlab(xlabel) +
    ylab("Depth (m)") +
    theme_classic()

  return(p)
}

