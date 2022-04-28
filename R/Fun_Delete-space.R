#' Delete all whitespace until a non-whitespace character.
#' @param extract_val a vector.
#'

delete_space <- function(extract_val){
  while(extract_val[1] == ""){
    extract_val<-extract_val[-1]
  }
  return(extract_val)
}
