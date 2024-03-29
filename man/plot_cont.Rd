% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fun_Contour-plot_sim.R
\name{plot_cont}
\alias{plot_cont}
\title{Contour plot of only simulation results of a water quality variable.}
\usage{
plot_cont(
  sim,
  sim.start,
  sim.end,
  legend.title,
  min.depth,
  max.depth,
  by.value,
  nlevels
)
}
\arguments{
\item{sim}{a matrix of simulated variables. This matrix can be generated
by running the "interpol" function.}

\item{sim.start, sim.end}{the start and end dates of the simulation period
for the DYRESM-CAEDYM model run of interest.
The date format must be "\%Y-\%m-\%d".}

\item{legend.title}{the legend title of the contour figure.}

\item{min.depth, max.depth, by.value}{minimum and maximum depths used to be
the start of y axis of the contour plot, at the increment of by.value.}

\item{nlevels}{Number of levels which are used to partition the range of
simulation variable.}
}
\value{
This function returns a filled.contour object.
}
\description{
Contour plot a matrix of values of a water quality variable,
}
\details{
This function is NOT based on ggplot2. To save the produced figure,
 users can use functions like png, bmp, jpeg, etc.
}
\examples{
sim <- matrix(c(28,28,28,27,25,24,12,13,14,15,16,17),
              nrow = 6,
              ncol = 2)

# contour plot of the sim data frame
  p <- plot_cont(sim = sim,
            sim.start = "2020-01-01",
            sim.end = "2020-01-02",
            legend.title = "T \u00B0C",
            min.depth = 0, max.depth = 5, by.value = 1,
            nlevels = 20)

  p

}
