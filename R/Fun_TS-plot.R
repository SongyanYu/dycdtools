#' Time series plot of simulated and observed values at target depths
#'
#' @param sim a matrix of simulated variables. This matrix can be generated
#' by running the "interpol" function.
#'
#' @param obs a data frame having three columns to describe observed values of
#'  a water quality variable. These three columns are 'Date' (as '\%Y-\%m-\%d'),
#'  'Depth', and the designated variable name which can be found from the
#'  var.name column of 'data(output_name)'.
#'  An example of such a data frame can be found with 'data(obs_temp)'
#'
#' @param target.depth a vector of depth (unit:m) for which time series
#'  simulation results will be plotted.
#'
#' @param sim.start,sim.end the start and end dates of the simulation period
#' of the DYRESM-CAEDYM model run of interest.
#' The date format must be "\%Y-\%m-\%d".
#'
#' @param plot.start,plot.end the start and end dates of the period to be
#'  plotted in the format of "\%Y-\%m-\%d".
#'
#' @param min.depth,max.depth,by.value minimum and maximum depths in the profile
#'  plot at an increment of by.value.
#'
#' @param ylabel the y axis title of the time series plot.
#'
#' @return This function returns a ggplot object that can be modified with
#'  ggplot package functions.
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
#'                              var = dyresmTEMPTURE_Var,
#'                              min.dept = 0, max.dept = 13, by.value = 0.5)
#'
#'  data(obs_temp)
#' # time series plot of temperature sim and obs
#'  p <- plot_ts(sim = temp.interpolated,
#'               obs = obs_temp,
#'               target.depth=c(1,6),
#'               sim.start="2017-06-06",
#'               sim.end="2017-06-15",
#'               plot.start="2017-06-06",
#'               plot.end="2017-06-15",
#'               ylabel="Temperature \u00B0C",
#'               min.depth=0,
#'               max.depth=13,
#'               by.value=0.5)
#'
#'  p
#'
#' @export

plot_ts<-function(sim,
                  obs,
                  target.depth,
                  sim.start,
                  sim.end,
                  plot.start,
                  plot.end,
                  min.depth,
                  max.depth,
                  by.value,
                  ylabel){

  #---
  # 1. simulation period
  #---
  sim.date<-seq.Date(from = as.Date(sim.start,format="%Y-%m-%d"),
                     to = as.Date(sim.end,format="%Y-%m-%d"),
                     by="day")

  #---
  # 2. combine sim with obs by Date and Depth
  #---
  sim.temp <- as.data.frame(sim)
  colnames(sim.temp) <- sim.date
  sim.temp$Depth <- seq(min.depth, max.depth, by=by.value)

  colnames(obs) <- c("Date","Depth","Value")
  obs <- obs %>%
    mutate(Date = as.Date(Date,format = "%Y-%m-%d"))

  temp.both <- sim.temp %>%
    pivot_longer(-Depth, names_to = "Date", values_to = "sim") %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    left_join(., obs, by=c("Date", "Depth")) %>%
    filter(Date >= plot.start & Date <= plot.end)

  colnames(temp.both)[4] <- "obs"

  #---
  # 3. time series plot sim vs. obs, faceted by Depth
  #---
  p <- temp.both %>%
    filter(Depth %in% target.depth) %>%
    ggplot () +
    geom_line(aes(x = Date, y = sim)) +
    geom_point(aes(x = Date, y = obs), col = "red") +
    facet_grid(~Depth) +
    theme_classic() +
    labs(y = ylabel)

  return(p)
}
