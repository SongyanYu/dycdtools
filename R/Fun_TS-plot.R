#' Time series plot of simulated and observed values
#' @param sim interpolated values of variable
#' @param obs observed values of variable
#' @param file_name the file path to save the generated ts plot.
#' @param target.depth a vector of depth (unit:m) to be used to extract and plot variable values.
#' @param sim.start,sim.end the beginning and ending simulation dates for the intended DYRESM-CAEDYM model run. The date format must be "\%Y-\%m-\%d".
#' @param plot.start,plot.end the beginning and ending dates for the plotting purpose. The date format must be "\%Y-\%m-\%d".
#' @param min.depth,max.depth,by.value minimum and maximum depth for the profile plot at the depth increment of by.value.
#' @param ylabel the y axis title.
#' @param height,width the height and width of the time series figure.
#' @return a plot of sim and obs time series.
#' @export

ts.plot<-function(sim=temp.interpolated,
                  obs=obs.temp,
                  file_name,
                  target.depth=c(1,6,12,30),
                  sim.start="2017-06-06",
                  sim.end="2020-02-29",
                  plot.start="2017-06-06",
                  plot.end="2020-02-29",
                  min.depth=0,max.depth=33,by.value=0.5,
                  ylabel="Temperature \u00B0C",
                  height=7,
                  width=11){

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
    left_join(.,obs,by=c("Date","Depth"))%>%
    filter(Date>=plot.start&Date<=plot.end)

  colnames(temp.both)[4]<-"obs"

  #---
  # 3. time series plot sim vs. obs, faceted by Depth
  #---
  temp.both%>%
    filter(Depth %in% target.depth)%>%
    ggplot()+
    geom_line(aes(x=Date,y=sim))+
    geom_point(aes(x=Date,y=obs),col="red")+
    facet_grid(~Depth)+
    theme_classic()+
    labs(y=ylabel)+
    ggsave(filename = file_name,height = height,width = width)
}
