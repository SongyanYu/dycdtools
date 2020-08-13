#' convert from height to depth
#' @param height a vector of height profile
#'

hgt.to.dpt<-function(height){
  depth<-max(height)-height
  return(depth)
}
