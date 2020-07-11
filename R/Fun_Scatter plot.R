#' Scatter plot of sim and obs var values
#' @param sim interpolated values of variable.
#' @param obs observed values of variable. This data need to have fixed types of colnames and orders.
#' @param file_name the file path to save the generated scatter plot.
#' @param sim.start,sim.end the beginning and ending simulation dates for the intended DYRESM-CAEDYM model run. The date format must be "%Y-%m-%d".
#' @param plot.start,plot.end the beginning and ending dates for the plotting purpose. The date format must be "%Y-%m-%d".
#' @param height,width the height and width of the scatter figure.
#' @import hydroGOF
#' @import ggplot2
#' @import RColorBrewer
#' @return a scatter plot of sim vs. obs
#' @export

scatter.plot<-function(sim=temp.interpolated,
                       obs=obs.temp,
                       sim.start="2017-06-06",
                       sim.end="2020-02-29",
                       plot.start="2017-06-06",
                       plot.end="2020-02-29",
                       file_name,
                       min.depth=0,max.depth,by.value,
                       height=4,
                       width=7){

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
  sim.temp$Depth<-seq(min.depth,max.depth,by=by.value)

  temp.both<-sim.temp%>%
    pivot_longer(-Depth,names_to = "Date",values_to = "sim")%>%
    mutate(Date=as.Date(Date,format="%Y-%m-%d"))%>%
    right_join(.,obs,by=c("Date","Depth"))%>%
    filter(Date>=plot.start&Date<=plot.end)

  colnames(temp.both)[4]<-"obs"

  #---
  # 3.report two objective functions' value and scatter plot sim vs. obs
  #---
  print(NSE(temp.both$sim,obs=temp.both$obs))   #NSE
  print(sqrt(mean((temp.both$sim-temp.both$obs)^2,na.rm = TRUE)))  # RMSE

  temp.both%>%
    ggplot(aes(x=obs,y=sim,colour=Depth))+
    geom_point()+
    geom_abline(color="red")+
    scale_color_gradientn(colors = brewer.pal(11, "Spectral"), name = "Depth (m)")+
    xlab("Observed")+
    ylab("Simulated")+
    theme_classic()+
    ggsave(filename = file_name,height = height,width = width)
}
