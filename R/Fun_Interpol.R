#'Interpolation of simulation along depths.
#'
#'Convert simulated variable at irregular layer heights to a dataframe of the same variable at regular (i.e.sequenced) layer heights.
#'
#' @param layerHeights layer heights, extracted from DYCD outputs
#' @param var simulated variable values, extracted from DYCD outputs
#' @param min.dept,max.depth,by.value minimum and maximum depth for interpolation at the depth increment of by.value.
#'
#' @return a matrix of interpolated values of such variable.
#' @export

interpol<-function(layerHeights,
                   var,
                   min.dept,
                   max.dept,
                   by.value){
  x<-apply(layerHeights,2,FUN = function(a) hgt.to.dpt(a[!is.na(a)]))
  # var is for the middle of each layer
  # x.mid<-lapply(x,FUN = function(a) c(diff(a)/2,a[length(a)]/2*-1)+a)
  y<-apply(var,2,FUN = function(a) a[!is.na(a)])

  var.interpolated<-matrix(NA,nrow=length(seq(min.dept,max.dept,by=by.value)),ncol=length(x))
  for(i in 1:length(x)){
    var.interpolated[,i]<-approx(x[[i]],y[[i]],xout = seq(min.dept,max.dept,by=by.value),rule = 2,method = "linear")[[2]]
  }

  return(var.interpolated)
}
