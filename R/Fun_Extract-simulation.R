#' Extract outputs from a DYRESM-CAEDYM model run
#'
#' @description
#' Extract simulation outputs from a DYRESM-CAEDYM  model run.
#'
#' @param dycd.output a string of characters describing the file path to
#' the output netcdf file of DYRESM-CAEDYM model.
#'
#' @param var.extract a vector of variables to be extracted from the output file.
#' Please refer to the var.name of data(output_name) for accepted variable name.
#' Apart from the user nominated variables, simulation period and
#' layer height data are also extracted.
#'
#' @param verbose if TRUE, the information about the extraction
#' process is printed.
#'
#' @import ncdf4
#'
#' @examples
#'  # extract simulated temperature values from DYRESM-CAEDYM simulation file
#'  var.values<-ext_output(dycd.output=system.file("extdata", "dysim.nc",
#'                                                  package = "dycdtools"),
#'                        var.extract=c("TEMP"))
#'
#'  for(i in 1:length(var.values)){
#'   expres<-paste0(names(var.values)[i],"<-data.frame(var.values[[",i,"]])")
#'   eval(parse(text=expres))
#'  }
#'
#' @return a list of values of those variables of interest,
#'    as well as two compulsory variables (i.e. simulation period, layer height)
#' @export
#'

ext_output <- function(dycd.output,
                     var.extract,
                     verbose = FALSE){


  simData <- nc_open(dycd.output)
  varNames <- names(simData$var)

  if(verbose){
    message("You are going to extract ", length(var.extract), " variable(s).\n")
  }

  if(any(is.na(match(var.extract, output_name$var.name)))){
    message(var.extract[is.na(match(var.extract, output_name$var.name))],
            "is(are) not on the optional variable list.\n")
  }

  actual.var <-
    as.character(na.omit(output_name$output.name[match(var.extract,
                                                       output_name$var.name)]))

  if("CHLA" %in% var.extract){
    actual.var <- append(actual.var,
                         as.character(output_name$output.name[output_name$var.name == "CHLA"]))
    actual.var <- unique(actual.var)
  }

  if(!all(actual.var %in% varNames)){

    if(verbose){
      message('but ', paste0(var.extract[which(!(actual.var %in% varNames))],
                             sep = ' '), ' not in the model outputs!\n')
      message('Only ', paste0(var.extract[which(actual.var %in% varNames)],
                              sep = ' '), 'get(s) extracted!\n')
    }

    actual.var <- actual.var[actual.var %in% varNames]
  }

  # add compulsory variables
  actual.var <- append(actual.var, c("dyresmTime", "dyresmLAYER_HTS_Var"))

  var.values <- lapply(actual.var, FUN = function(x)
    ncvar_get(simData, varNames[which(varNames==x)]))

  nc_close(simData)

  names(var.values) <- actual.var
  var.values[["dyresmTime"]] <- var.values[["dyresmTime"]] - 450275

  return(var.values)
}
