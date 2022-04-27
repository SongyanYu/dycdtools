#' change parameter value of input files to DYRESM_CAEDYM model.
#'
#' @param input_file vector of input format, such as "par","cfg".
#' @param row_no the number of row where the variable of interest is in the input file
#' @param new_value the new value that will be assigned to the variable of interest.
#'

change_input_file<-function(input_file="par",
                            row_no=12,
                            new_value){
  #---
  # 1. read in input file.
  #---
  par_data <- readLines(paste0(input_file))

  #---
  #2. remove all proceeding whitespace in the line of interest.
  #---
  extract_val <- unlist(strsplit(par_data[row_no], split = " "))
  extract_val <- delete_space(extract_val)

  #---
  # 3. replace old value with new value, and update the input file.
  #---
  par_data[row_no] <- paste(new_value,paste0(extract_val[-1],collapse = " "))
  writeLines(par_data, input_file)
}

