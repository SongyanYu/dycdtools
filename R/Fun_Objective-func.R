#' Calculate measures of goodness of fit for DYCD model simulations.
#'
#' @description
#' calculate any of the five objective functions that are commonly used to measure goodness of fit:
#'    1) Nash-Sutcliffe efficiency coefficient (NSE),
#'    2) Root Mean Square Error (RMSE),
#'    3) Mean Absolute Error (MAE),
#'    4) Relative Absolute Error (RAE), and
#'    5) Pearson's r (Pearson).
#'
#' @param sim a matrix of bio-geochemical variable values with column of time and row of depth.
#' @param obs a data frame of observed value, with three columns: Date, depth, value.
#' @param fun objective function to be calculated. select any from 'NSE', 'RMSE', 'MAE', 'RAE', and 'Pearson'. Multiple is allowed.
#' @param start.date,end.date the beginning and ending simulation dates for the intended DYRESM-CAEDYM model run. The date format must be "\%Y-\%m-\%d".
#' @param min.depth,max.depth the minimum and maximum depths of the simulation matrix .
#' @param by.value the value of increment for depth of the simulation matrix.
#'
#' @import dplyr
#' @import tidyr
#' @import hydroGOF
#' @importFrom stats cor
#'
#' @return a vector of objective function values. The first is NSE and the second is RMSE.

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
  value.NSE <- NSE(var.both$sim, obs = var.both$obs)
  value.RMSE <- sqrt(mean((var.both$sim-var.both$obs)^2, na.rm = TRUE))
  value.MAE <- mean(abs(var.both$sim-var.both$obs), na.rm=TRUE)
  value.Pearson <- cor(x = var.both$sim, y = var.both$obs, method = "pearson", use = 'complete.obs')
  value.PAE <- mean(abs(var.both$sim - var.both$obs), na.rm=TRUE)/mean(abs(var.both$obs-mean(var.both$obs, na.rm = TRUE)), na.rm = TRUE)

  value.lst <- list(NSE = value.NSE, RMSE = value.RMSE,
                    MAE = value.MAE, Pearson = value.Pearson,
                    PAE = value.PAE)

  return(value.lst[fun])
}
