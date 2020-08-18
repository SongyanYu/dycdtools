#' Profile plot of simulated variable values vs. depth
#'
#' @description
#' A post-processing function used to visualise model output in a profile graph.
#'
#' @param sim interpolated values of variable.
#' @param obs observed values of variable.
#' @param sim.start,sim.end the beginning and ending simulation dates for the intended DYRESM-CAEDYM model run. The date format must be "\%Y-\%m-\%d".
#' @param plot.start,plot.end the beginning and ending dates for the plotting purpose. The date format must be "\%Y-\%m-\%d".
#' @param xlabel the x axis label of the profile figure
#' @param min.depth,max.depth,by.value minimum and maximum depth for the profile plot at the depth increment of by.value.
#' @param plot.save if TRUE, the plot is saved with the "height","width", and "ppi" parameters.
#' @param file_name the file path to save the generated profile figure
#' @param height,width the height and width of the profile figure.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return a profile plot of sim~depth
#'
#' @examples
#'  var.values<-ext.output(dycd.output=system.file("extdata", "dysim.nc", package = "dycdtools"),
#'                        var.extract=c("TEMP"))
#'
#'  for(i in 1:length(var.values)){
#'    expres<-paste0(names(var.values)[i],"<-data.frame(var.values[[",i,"]])")
#'    eval(parse(text=expres))
#'   }
#'
#' # interpolate temperature for depths from 0 to 13 m at increment of 0.5 m
#'   temp.interpolated<-interpol(layerHeights = dyresmLAYER_HTS_Var,
#'                              var = dyresmTEMPTURE_Var,
#'                              min.dept = 0,max.dept = 13,by.value = 0.5)
#'
#'  data(obs_temp)
#' # profile plot of temperature sim and obs
#'   prof.plot(sim=temp.interpolated,
#'             obs = obs_temp,
#'             sim.start="2017-06-06",
#'             sim.end="2017-06-15",
#'             plot.start="2017-06-06",
#'             plot.end="2017-06-15",
#'             xlabel = "Temperature \u00B0C",
#'             min.depth = 0,max.depth = 13,by.value = 0.5)
#'
#'
#' @export

prof.plot<-function(sim=temp.interpolated,
                    obs=obs.temp,
                    sim.start="2017-06-06",
                    sim.end="2020-02-29",
                    plot.start="2017-06-06",
                    plot.end="2020-02-29",
                    xlabel="Temperature \u00B0C",
                    min.depth=0,max.depth,by.value,
                    plot.save=FALSE,
                    file_name,
                    height=11,width=18){

  #---
  # 1. simulation period
  #---
  sim.date<-seq.Date(from = as.Date(sim.start,format="%Y-%m-%d"),
                     to = as.Date(sim.end,format="%Y-%m-%d"),
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
  p<-temp.both%>%
    ggplot()+
    geom_point(aes(y=Depth,x=obs),col="red")+
    geom_path(aes(y=Depth,x=sim))+
    facet_wrap(~Date)+
    ylim(max.depth,min.depth)+
    xlab(xlabel)+
    ylab("Depth (m)")+
    theme_classic()

  plot(p)

  if(plot.save){
    p+ggsave(filename = file_name,height = height,width = width)
  }
}

