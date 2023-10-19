#' Calculate statistical measures of goodness of fit for
#' DYRESM-CAEDYM model simulations.
#'
#' @description
#' calculate the below five objective functions that are commonly used to
#' measure goodness of fit:
#'    1) Nash-Sutcliffe Efficiency coefficient (NSE),
#'    2) Root Mean Square Error (RMSE),
#'    3) Mean Absolute Error (MAE),
#'    4) Relative Absolute Error (RAE), and
#'    5) Pearson's r (Pearson).
#'
#' @param sim a matrix of a simulated water quality variable values with
#'  column of time and row of depth. This matrix can be generated
#'  by running the "interpol" function.
#'
#' @param obs a data frame having three columns to describe observed values of
#'  a water quality variable. These three columns are 'Date' (as '\%Y-\%m-\%d'),
#'  'Depth', and the designated variable name which can be found from the
#'  var.name column of 'data(output_name)'.
#'  An example of such a data frame can be found with 'data(obs_temp)'
#'
#' @param fun objective function(s) to be calculated.
#' Select any from 'NSE', 'RMSE', 'MAE', 'RAE', and 'Pearson'.
#' Multiple selections are allowed.
#'
#' @param start.date,end.date the start and end simulation dates for the
#'  DYRESM-CAEDYM model run. The date format must be "\%Y-\%m-\%d".
#'
#' @param min.depth,max.depth the minimum and maximum depths of the
#'  simulation matrix.
#'
#' @param by.value the value of increment at which the depth of layers increases
#' from the mim.depth to max.depth in the simulation matrix.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats cor
#'
#' @return a list of objective function values.
#' @export

objective_fun<-function(sim,
                        obs,
                        fun = c('NSE','RMSE'),
                        start.date,
                        end.date,
                        min.depth,
                        max.depth,
                        by.value){

  #---
  # 1. simulation period
  #---
  sim.date <- seq.Date(from = as.Date(start.date,format="%Y-%m-%d"),
                     to = as.Date(end.date,format="%Y-%m-%d"),
                     by="day")

  #---
  # 2. combine sim with obs by Date and Depth
  #---
  sim.var <- as.data.frame(sim)
  colnames(sim.var) <- sim.date
  sim.var$Depth <- seq(min.depth, max.depth, by = by.value)

  var.both <- sim.var %>%
    pivot_longer(-Depth, names_to = "Date", values_to = "sim") %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
    right_join(., obs, by = c("Date", "Depth"))

  colnames(var.both)[4] <- "obs"

  #---
  # 3. calculate goodness of fit measures
  #---
  value.NSE <- 1 - sum((var.both$obs - var.both$sim)^2, na.rm = TRUE)/
    sum((var.both$obs - mean(var.both$obs, na.rm = TRUE))^2, na.rm = TRUE)
  value.RMSE <- sqrt(mean((var.both$sim-var.both$obs)^2, na.rm = TRUE))
  value.MAE <- mean(abs(var.both$sim-var.both$obs), na.rm=TRUE)
  value.Pearson <- cor(x = var.both$sim, y = var.both$obs, method = "pearson",
                       use = 'complete.obs')
  value.PAE <- mean(abs(var.both$sim - var.both$obs), na.rm=TRUE)/
    mean(abs(var.both$obs-mean(var.both$obs, na.rm = TRUE)), na.rm = TRUE)

  value.lst <- list(NSE = value.NSE, RMSE = value.RMSE,
                    MAE = value.MAE, Pearson = value.Pearson,
                    PAE = value.PAE)

  return(value.lst[fun])
}
