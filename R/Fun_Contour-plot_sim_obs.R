#' Contour plot of DYRESM-CAEDYM simulation outputs of a water quality variable,
#'  with observed data shown as dots in the generated contour plot.
#'
#'  This function is NOT based on ggplot2. To save the produced figure,
#'  users can use functions like png, bmp, jpeg, etc.
#'
#' @description
#' Contour plot a matrix of values of a water quality variable.
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
#' @param sim.start,sim.end the start and end dates of the simulation period
#' for the DYRESM-CAEDYM model run of interest.
#' The date format must be "\%Y-\%m-\%d".
#'
#' @param plot.start,plot.end the start and end dates of the period to
#'  be plotted, in the format of "\%Y-\%m-\%d".
#'
#' @param legend.title the legend title of the contour figure.
#'
#' @param min.depth,max.depth,by.value minimum and maximum depths used to be
#' the start of y axis of the contour plot, at the increment of by.value.
#'
#' @param nlevels Number of levels which are used to partition the range of
#' simulation variable.
#'
#' @importFrom grDevices hcl.colors png dev.off
#' @importFrom graphics axis filled.contour mtext par points title
#' @importFrom lubridate year ymd
#'
#' @return This function returns a filled.contour object.
#'
#' @examples
#' obs <- data.frame(Date = c(rep('2020-01-01', 6), rep('2020-01-02', 6)),
#'                   Depth = rep(0:5, 2),
#'                   TEMP = rep(29:24,2))
#'
#' sim <- matrix(c(28,28,28,27,25,24,12,13,14,15,16,17),
#'               nrow = 6,
#'               ncol = 2)
#'
#' # contour plot of temperature simulations
#' # with observed data shown as colour-coded dots
#'   p <- plot_cont_comp(sim = sim,
#'                  obs = obs,
#'                  sim.start = "2020-01-01",
#'                  sim.end = "2020-01-02",
#'                  plot.start = "2020-01-01",
#'                  plot.end = "2020-01-02",
#'                  legend.title = "T \u00B0C",
#'                  min.depth=0, max.depth=5, by.value=1,
#'                  nlevels=20)
#'
#'  p
#'
#' @export

plot_cont_comp<-function(sim,
                         obs,
                         sim.start,
                         sim.end,
                         plot.start,
                         plot.end,
                         legend.title,
                         min.depth,
                         max.depth,
                         by.value,
                         nlevels = 20){

  #---
  # 1. simulation period
  #---

  if(any(is.na(ymd(plot.start, quiet = TRUE)),
         is.na(ymd(plot.end, quiet = TRUE)),
         is.na(ymd(sim.start, quiet = TRUE)),
         is.na(ymd(sim.end, quiet = TRUE)))){

    stop('Make sure date format is \'%Y-%m-%d\'\n')

  }

  plot.date <- seq.Date(from = as.Date(plot.start, format = "%Y-%m-%d"),
                      to = as.Date(plot.end, format = "%Y-%m-%d"),
                      by = "day")

  sim.date <- seq.Date(from = as.Date(sim.start, format = "%Y-%m-%d"),
                     to = as.Date(sim.end, format = "%Y-%m-%d"),
                     by = "day")

  index <- match(seq(lubridate::year(plot.date)[1],
                     lubridate::year(plot.date)[length(plot.date)],
                     by = 1),
                 lubridate::year(plot.date))

  plot.sim <-
    sim[, c(match(plot.date[1],sim.date):
                            match(plot.date[length(plot.date)],sim.date))]

  levels <- pretty(range(plot.sim,obs[,3], na.rm = TRUE), nlevels)
  color.palette <- function(n)hcl.colors(n, "RdBu", rev=TRUE)
  colour <- unlist(lapply(obs[, 3], FUN = function(x)
    color.palette(length(levels)-1)[length(which(levels < x))]))

  colnames(obs) <- c("Date","Depth","Value")
  obs <-
    obs %>%
    filter(Date >= plot.date[1] & Date <= plot.date[length(plot.date)],
           !is.na(Value)) %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

  #---
  # 2. contour plot the var matrix
  #---

  p <- filled.contour(x = seq(1, ncol(plot.sim), by=1),
                 y = seq(min.depth, max.depth, by = by.value),
                 z = t(plot.sim),
                 ylim = c(max.depth, min.depth),
                 zlim = c(min(plot.sim, obs[,3], na.rm=TRUE),
                          max(plot.sim, obs[,3], na.rm = TRUE)),
                 nlevels = nlevels,
                 color.palette = function(n)hcl.colors(n, "RdBu", rev=TRUE),
                 plot.axes = {
                   axis(3, mgp = c(1, 0.25, 0), tcl = -0.1, cex.axis = 1.8,
                        lwd = 0.5,
                        at = seq(1, ncol(plot.sim), by=1)[c(1, index)],
                        labels = lubridate::year(plot.date[c(1, index)]))
                   axis(2, mgp = c(1, 0.4, 0), tcl = -0.2, cex.axis = 1.8,
                        cex.lab = 0.8, lwd = 0.5)
                   mtext("Depth (m)", side = 2, line = 1.3, las = 0, cex = 1.8)
                   points(x = match(obs$Date, plot.date), y = obs$Depth,
                          bg = colour, pch = 21, cex = 1.3)},
                 key.title = {
                   par(cex.main = 1.3); title(main = legend.title)
                 })

  return(p)
}
