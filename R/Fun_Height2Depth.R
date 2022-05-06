#' convert from height to depth
#' @param height a vector of height profile
#'

hgt_to_dpt <- function(height){
  depth <- max(height) - height
  return(depth)
}
