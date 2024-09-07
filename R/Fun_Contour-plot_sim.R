#' Contour plot of only simulation results of a water quality variable.
#'
#'  This function is NOT based on ggplot2. To save the produced figure,
#'  users can use functions like png, bmp, jpeg, etc.
#'
#' @description
#' Contour plot a matrix of values of a water quality variable,
#'
#' @param sim a matrix of simulated variables. This matrix can be generated
#' by running the "interpol" function.
#'
#' @param sim.start,sim.end the start and end dates of the simulation period
#' for the DYRESM-CAEDYM model run of interest.
#' The date format must be "\%Y-\%m-\%d".
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
#' @importFrom lubridate year
#'
#' @return This function returns a filled.contour object.
#'
#' @examples
#' sim <- matrix(c(28,28,28,27,25,24,12,13,14,15,16,17),
#'               nrow = 6,
#'               ncol = 2)
#'
#' # contour plot of the sim data frame
#'   p <- plot_cont(sim = sim,
#'             sim.start = "2020-01-01",
#'             sim.end = "2020-01-02",
#'             legend.title = "T \u00B0C",
#'             min.depth = 0, max.depth = 5, by.value = 1,
#'             nlevels = 20)
#'
#'   p
#'
#' @export

plot_cont<-function(sim,
                    sim.start,
                    sim.end,
                    legend.title,
                    min.depth,
                    max.depth,
                    by.value,
                    nlevels){

  #---
  # 1. simulation period
  #---
  date <- seq.Date(from = as.Date(sim.start, format = "%Y-%m-%d"),
                   to = as.Date(sim.end,format = "%Y-%m-%d"),
                   by = "day")

  index <- match(seq(lubridate::year(date)[1],
                     lubridate::year(date)[length(date)],
                     by=1), lubridate::year(date))

  color.palette <- function(n) hcl.colors(n, "RdBu", rev=TRUE)

  #---
  # 2. contour plot the var matrix
  #---
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  p <- filled.contour(x = seq(1, ncol(sim), by=1),
                      y = seq(min.depth, max.depth, by = by.value),
                      z = t(sim),
                      ylim = c(max.depth, min.depth),
                      zlim = c(min(sim, na.rm=TRUE), max(sim, na.rm = TRUE)),
                      nlevels = nlevels,
                      color.palette =
                        function(n) hcl.colors(n, "RdBu", rev=TRUE),
                      plot.axes = {
                        axis(3, mgp = c(1,0.25,0), tcl = -0.1, cex.axis = 1.8,
                             lwd=0.5,
                             at = seq(1, ncol(sim), by = 1)[c(1, index)],
                             labels = lubridate::year(date[c(1, index)]))
                        axis(2, mgp = c(1, 0.4, 0), tcl = -0.2, cex.axis = 1.8,
                             cex.lab = 0.8, lwd = 0.5)
                        mtext("Depth (m)", side = 2, line = 1.3,
                              las = 0,cex = 1.8)},
                      key.title = {
                        par(cex.main=1.3); title(main = legend.title)
                      })

  return(p)
}
