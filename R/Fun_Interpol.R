#' Interpolation of DYRESM-CAEDYM simulation results
#' across a series of user-defined depths.
#'
#' @description The default simulation results of a water quality variable from
#' DYRESM-CAEDYM are usually at irregular layer heights.
#' This function convert it to a data frame with regular layer heights through
#' interpolation.
#'
#' @param layerHeights layer heights, outputs from a DYRESM-CAEDYM model run,
#' and can be generated with the 'ext_output' function.
#'
#' @param var simulation results of a water quality variable and can also be
#' generated with the 'ext_output' function.
#'
#' @param min.depth,max.depth,by.value minimum and maximum layer depths within
#' which interpolation will be conducted. by.value sets up the depth increments
#' between two immediate layers.
#'
#' @importFrom stats approx na.omit
#' @return a matrix of interpolated values of the water quality variable(s).
#'
#' @examples
#' # extract simulated temperature values from DYRESM-CAEDYM simulation file
#'  var.values<-ext_output(dycd.output=system.file("extdata", "dysim.nc",
#'                                                  package = "dycdtools"),
#'                        var.extract=c("TEMP"))
#'
#'  for(i in 1:length(var.values)){
#'   expres<-paste0(names(var.values)[i],"<-data.frame(var.values[[",i,"]])")
#'   eval(parse(text=expres))
#'  }
#'
#'  # interpolate temperature for depths from 0 to 13 m at increment of 0.5 m
#'  temp.interpolated<-interpol(layerHeights = dyresmLAYER_HTS_Var,
#'                             var = dyresmTEMPTURE_Var,
#'                             min.dept = 0,
#'                             max.dept = 13,
#'                             by.value = 0.5)
#'
#' @export

interpol<-function(layerHeights,
                   var,
                   min.depth,
                   max.depth,
                   by.value){
  x <- apply(layerHeights, 2, FUN = function(a) hgt_to_dpt(a[!is.na(a)]))
  # var is for the middle of each layer
  # x.mid<-lapply(x,FUN = function(a) c(diff(a)/2,a[length(a)]/2*-1)+a)
  y <- apply(var, 2, FUN = function(a) a[!is.na(a)])

  var.interpolated <- matrix(NA, nrow = length(seq(min.depth, max.depth,
                                                   by = by.value)),
                             ncol = length(x))
  for(i in 1:length(x)){
    var.interpolated[,i] <-
      approx(x[[i]], y[[i]], xout = seq(min.depth, max.depth, by = by.value),
             rule = 2, method = "linear")[[2]]
  }

  return(var.interpolated)
}
