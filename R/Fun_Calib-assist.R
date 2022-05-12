#' Assist calibration of DYRESM-CAEDYM model.
#'
#' @description This function carries out simulations with a large number
#' of possible combinations of parameter values that users regard as
#' potentially suitable for their model calibration, and calculates
#' the values of nominated objective functions (i.e., statistical measures
#' of goodness of fit) for each combination. Based on the calculated
#' objective function values, users can determine the optimal set(s)
#' of parameter values or narrow the ranges of possible parameter values.
#'
#' @param cal.para a data frame or a character string naming an external
#' .csv file where below column names are mandatory: "Parameter" describing
#' parameter names (abbreviation is allowed), "Min", "Max", and
#' "Increment" describing the minimum and maximum parameter values
#' and expected increment in the value range, "Input_file" and
#' "Line_NO" listing in which configuration file at which line
#' the parameter can be found.
#'
#' @param combination a vector of string character of how to pick up
#'  combinations of parameter values. "random" - the function randomly
#'  picks up a given number of combinations; "all" - the function tries
#'  all possible combinations of parameter values.
#'
#' @param n the number of random selections.
#' Must be provided if combination = "random".
#'
#' @param model.var a vector of string character of modelled
#' variables for calibration. the character should be from the
#' 'var.name' column of 'data(output_name)'.
#' Note that if model calibration needs to regard chlorophyll
#' of multiple phytoplankton groups as a whole,
#' model.var should use "CHLA" and individual phytoplankton group
#' should be specified through the "phyto.group" argument.
#' If phytoplankton groups are separately calibrated,
#' simply list their character in this argument (model.var).
#'
#' @param phyto.group a vector of simulated phytoplankton groups,
#' including CHLOR, FDIAT, NODUL, CYANO and CRYPT.
#'
#' @param obs.data a data frame or a character string naming a csv file of
#' observed lake data.
#' The observed lake data need to include below columns:
#'  1) 'Date' in format of "\%Y-\%m-\%d"
#'  2) 'Depth' (integer)
#'  3) Water quality variables (use string characters of model var
#'     as column names).
#'  see example data 'data(obs_temp)'.
#'
#' @param objective.function a vector of string character describing which
#'  objective function(s) to be used for calibration. Selected from
#'  the following five functions:
#'  "NSE": Nash-Sutcliffe efficiency coefficient,
#'  "RMSE": Root Mean Square Error,
#'  "MAE": Mean Absolute Error,
#'  "RAE": Relative Absolute Error,
#'  "Pearson": Pearson's r.
#'
#' @param start.date,end.date the beginning and end simulation dates
#'  for the intended DYRESM-CAEDYM calibration.
#'  The date format must be "\%Y-\%m-\%d".
#'  The two dates should be consistent with model configurations.
#'
#' @param dycd.wd the directory where input files (including the bat file)
#'  to DYRESM-CAEDYM are stored.
#'
#' @param dycd.output a character string naming the output file of
#'  model simulation.
#'
#' @param file.name a character string naming a .csv file where
#'  the results of this function are written to. Needed if 'write.out'
#'  = TRUE.
#'
#' @param verbose if TRUE, model calibration information is printed.
#'
#' @param parallel if TRUE, the calibration process is run on multiple cores.
#'
#' @param n.cores When 'parallel' is TRUE, n.cores is the number of cores
#'  the calibration function will be run on. If not provided,
#'  the default value is the number of available cores on the computer -1.
#'
#' @param write.out if TRUE, model calibration results are saved in a file
#'  with a file name set by the "file.name" argument.
#'
#' @import dplyr
#' @importFrom utils read.csv write.csv
#' @return a data frame of all tested values of parameters and
#'  corresponding values of the objective function(s).
#'
#' @note No executable examples are provided to illustrate the
#'  use of this function, as this function relies on the
#'  DYRESM-CAEDYM executables to work.
#'
#' @export

calib_assist<-function(cal.para,
                       combination = 'random',
                       n,
                       model.var,
                       phyto.group = NA,
                       obs.data,
                       objective.function = c("NSE", "RMSE"),
                       start.date,
                       end.date,
                       dycd.wd,
                       dycd.output,
                       file.name,
                       verbose = TRUE,
                       parallel = FALSE,
                       n.cores = NULL,
                       write.out = TRUE){


  #---
  # 1.combination of parameter values
  #---
  if(is.data.frame(cal.para)){
    para.raw <- cal.para
  }else{
    para.raw <- read.csv(file = cal.para)
  }

  seq.list <- list()
  for(i in seq_len(nrow(para.raw))){
    seq.list[[i]] <- seq(para.raw$Min[i],para.raw$Max[i],
                       length.out =para.raw$N_values[i])
  }

  if(length(seq.list)==1){
    para.df<-data.frame(seq.list[[1]])
  }
  if(length(seq.list)==2){
    para.df<-data.frame(seq.list[[1]],
                        rep(seq.list[[2]],each=length(seq.list[[1]])))
  }
  if(length(seq.list)>2){
    para.df<-data.frame(seq.list[[1]],
                        rep(seq.list[[2]],each=length(seq.list[[1]])))
    for(i in 3:length(seq.list)){
      para.df<-data.frame(para.df,
                          rep(seq.list[[i]],each=nrow(para.df)))
    }
  }
  colnames(para.df)<-para.raw$Parameter

  #---
  # 2.all or random - number of iterations
  #---
  if(combination =="all"){
    iteration <- seq_len(nrow(para.df))
    para.df <- para.df[iteration, ]
  }
  if(combination =="random"){
    iteration <- base::sample(x = seq_len(nrow(para.df)), size = n)
    para.df <- para.df[iteration, ]
  }

  #---
  # 3. simulation period
  #---
  sim.date <- seq.Date(from = as.Date(start.date, format = "%Y-%m-%d"),
                       to = as.Date(end.date, format = "%Y-%m-%d"),
                       by = "day")

  #---
  # 4. read observed lake data for calculating objective functions
  #---
  if(is.data.frame(obs.data)){
    obs.lake <- obs.data
  }else{
    obs.lake <- read.csv(obs.data)
    obs.lake$Date <- as.Date(obs.lake$Date, format = "%Y-%m-%d")
  }

  obs.lake <- obs.lake %>%
    filter(Date >= sim.date[1] & Date <= sim.date[length(sim.date)])

  if(!all(model.var %in% colnames(obs.lake))){
    cat(model.var[!(model.var %in% colnames(obs.lake))],
    "are not provided for observed data,
    so that these variables will be excluded for model calibration.")
  }
  actual.model.var <- model.var[model.var %in% colnames(obs.lake)]
  obs.list <- lapply(match(actual.model.var, colnames(obs.lake)),
                     FUN = function(col.no){obs.lake[, c(1, 2, col.no)]})

  #---
  # 5. objective function
  #---
  for(obj.fun in objective.function){
    expres <- paste0(obj.fun, ".list <- list()")
    eval(parse(text = expres))
  }

  #---
  # 6. Calibration
  #---

  if(parallel){

    # folder where you want outputs from the cluster to be written
    dir.output <- paste0(dycd.wd,"clusteroutput")
    # create output directory
    dir.create(dir.output)

    # determine number of cores available to use
    if(is.null(n.cores)){
      n.cores <- parallel::detectCores() - 1
    }

    # allocate iterations to the available cores
    sim.cores <- rep(seq_len(n.cores), length.out=length(iteration))

    # find the files that are needed/wanted to run this model
    # (par, bio, chm, sed files will copy later)
    files.model <-
      list.files(path = dycd.wd,
                 pattern = '.*\\.(bat|cfg|con|inf|int|met|pro|stg|wdr)$',
                 recursive = FALSE, include.dirs = TRUE,
                 full.names = TRUE)

    # create sacrificial folders with a copy of the model for
    # each core being used
    for (c in seq_len(n.cores)) {
      dir.core <- paste0(dycd.wd,'/core',c)
      dir.create(dir.core)
      file.copy(files.model, to = dir.core, recursive = TRUE, overwrite = TRUE)
      R.utils::copyDirectory(paste0(dycd.wd, "/Bin"),
                             to = paste0(dir.core, "/Bin"),
                             recursive = TRUE, overwrite = TRUE)
      #R.utils::copyDirectory(paste0(dycd.wd, "/Files"),
      # to = paste0(dir.core, "/Files"),
      # recursive = TRUE, overwrite = TRUE)  # CAEDYM
    }

    ##### initiate and execute the cluster run
    cl <- parallel::makeCluster(n.cores)

    # export any necessary objects and/or functions to the cluster
    # before running
    parallel::clusterEvalQ(cl, library("dycdtools"))
    parallel::clusterExport(cl, varlist = c("dycd.wd","dir.output","sim.cores",
                                            "para.df","para.raw","iteration"),
                            envir = environment())

    parallel::clusterApply(cl, seq_len(length(iteration)),
                           run_iteration, dycd.wd)

    # clean up cluster
    try({parallel::stopCluster(cl)})

    ##### clean up the created folders
    unlink(paste0(dycd.wd, "/core", seq_len(n.cores)), recursive = TRUE)

    #---
    # calculate objective function values
    #---
    for(b in seq_along(iteration)) {
      var.values <- ext_output(dycd.output = paste0(dir.output,
                                                    "/DYsim_", b, ".nc"),
                               var.extract = actual.model.var)

      if("CHLA" %in% model.var){
        actual.model.var.2 <- append(actual.model.var, phyto.group)
        actual.model.var.2 <- actual.model.var.2[-which(actual.model.var.2 ==
                                                          "CHLA")]

        var.values <- ext_output(dycd.output = dycd.output,
                                 var.extract = actual.model.var.2)
      }

      for(nm in seq_along(var.values)){
        expres <- paste0(names(var.values)[nm],
                         "<-data.frame(var.values[[", nm, "]])")
        eval(parse(text = expres))
      }

      lake.depth.list <- apply(dyresmLAYER_HTS_Var, 2,
                               FUN = function(a) hgt_to_dpt(a[!is.na(a)]))

      max.depth <- ceiling(max(unlist(lake.depth.list)))

      for (var in actual.model.var){
        index <- match(var,actual.model.var)

        if(var == "CHLA"){
          for(phyto in phyto.group){
            expres <-
              paste0("sim.", phyto, "<-",
                     output_name$output.name[match(phyto,output_name$var.name)])
            eval(parse(text = expres))
          }
          expres <- paste0("sim.var<-",
                           paste0("sim.", phyto.group, collapse = "+"))
          eval(parse(text = expres))
        }

        if(var != "CHLA"){
          expres <- paste0("sim.var<-",
                           output_name$output.name[match(var,
                                                         output_name$var.name)])
          eval(parse(text = expres))
        }

        try.return <-
          try(interpolated <- interpol(layerHeights = dyresmLAYER_HTS_Var,
                                       var = sim.var,
                                       min.depth = 0,
                                       max.depth = max.depth,
                                       by.value = 0.5))

        if(class(try.return)[1]!="try-error"){

          obj.value <- objective_fun(sim = interpolated,
                                     obs = data.frame(obs.list[[index]]),
                                     fun = objective.function,
                                     start.date,
                                     end.date,
                                     min.depth = 0, max.depth = max.depth,
                                     by.value = 0.5)

          if(exists("NSE.list")){
            NSE.list[[var]][b] <- obj.value['NSE']
          }
          if(exists("RMSE.list")){
            RMSE.list[[var]][b] <- obj.value['RMSE']
          }

          if(exists("MAE.list")){
            MAE.list[[var]][b] <- obj.value['MAE']
          }

          if(exists("RAE.list")){
            RAE.list[[var]][b] <- obj.value['RAE']
          }

          if(exists("Pearson.list")){
            Pearson.list[[var]][b] <- obj.value['Pearson']
          }
        }
        else{
          if(exists("NSE.list")){
            NSE.list[[var]][b]<-NA
          }
          if(exists("RMSE.list")){
            RMSE.list[[var]][b]<-NA
          }
          if(exists("MAE.list")){
            MAE.list[[var]][b]<-NA
          }
          if(exists("RAE.list")){
            RAE.list[[var]][b]<-NA
          }
          if(exists("Pearson.list")){
            Pearson.list[[var]][b]<-NA
          }
        }
      }
    }

    if(exists("NSE.list")){
      nse.df <- data.frame(t(do.call(rbind.data.frame, NSE.list)))
      colnames(nse.df) <- paste0("NSE.", colnames(nse.df))
      para.df <- cbind(para.df, nse.df)
    }

    if(exists("RMSE.list")){
      rmse.df <- data.frame(t(do.call(rbind.data.frame, RMSE.list)))
      colnames(rmse.df) <- paste0("RMSE.", colnames(rmse.df))
      para.df <- cbind(para.df, rmse.df)
    }

    if(exists("MAE.list")){
      mae.df <- data.frame(t(do.call(rbind.data.frame, MAE.list)))
      colnames(mae.df) <- paste0("MAE.", colnames(mae.df))
      para.df <- cbind(para.df, mae.df)
    }

    if(exists("RAE.list")){
      rae.df <- data.frame(t(do.call(rbind.data.frame, RAE.list)))
      colnames(rae.df) <- paste0("RAE.", colnames(rae.df))
      para.df <- cbind(para.df, rae.df)
    }

    if(exists("Pearson.list")){
      pearson.df <- data.frame(t(do.call(rbind.data.frame, Pearson.list)))
      colnames(pearson.df) <- paste0("Pearson.", colnames(pearson.df))
      para.df <- cbind(para.df, pearson.df)
    }
  }

  if(!parallel){

    for(i in seq_along(iteration)){

      if(verbose){
        cat(i, "/", length(iteration), "\n")
      }

      #---
      # change the parameter values in the input files
      #---
      for(m in seq_len(ncol(para.df))){
        change_input_file(input_file = paste0(dycd.wd, "/",
                                              para.raw$Input_file[m]),
                          row_no = para.raw$Line_NO[m],
                          new_value = para.df[i, m])
      }

      #---
      # model simulation / run the .bat file
      #---
      user.wd <- getwd()
      on.exit(setwd(user.wd))
      setwd(dycd.wd)
      bat.file <- list.files(pattern = ".bat")
      shell(bat.file, intern = TRUE, wait=TRUE)
      setwd(user.wd)

      #---
      # calculate objective function values
      #---

      var.values <- ext_output(dycd.output = dycd.output,
                               var.extract = actual.model.var)

      if("CHLA" %in% model.var){
        actual.model.var.2 <- append(actual.model.var, phyto.group)
        actual.model.var.2 <-
          actual.model.var.2[-which(actual.model.var.2 == "CHLA")]

        var.values <- ext_output(dycd.output = dycd.output,
                                 var.extract = actual.model.var.2)
      }

      for(nm in seq_along(var.values)){
        expres <- paste0(names(var.values)[nm],
                         "<- data.frame(var.values[[", nm ,"]])")
        eval(parse(text = expres))
      }

      lake.depth.list <- apply(dyresmLAYER_HTS_Var, 2,
                               FUN = function(a) hgt_to_dpt(a[!is.na(a)]))
      max.depth <- ceiling(max(unlist(lake.depth.list)))

      for (var in actual.model.var){
        index <- match(var, actual.model.var)

        if(var == "CHLA"){
          for(phyto in phyto.group){
            expres <-
              paste0("sim.", phyto, "<-",
                     output_name$output.name[match(phyto,
                                                   output_name$var.name)])
            eval(parse(text = expres))
          }
          expres <- paste0("sim.var <- ",
                           paste0("sim.", phyto.group, collapse = "+"))
          eval(parse(text = expres))
        }

        if(var != "CHLA"){
          expres <- paste0("sim.var <- ",
                           output_name$output.name[match(var,
                                                         output_name$var.name)])
          eval(parse(text = expres))
        }

        try.return <- try(interpolated <-
                            interpol(layerHeights = dyresmLAYER_HTS_Var,
                                     var = sim.var,
                                     min.depth = 0,
                                     max.depth = max.depth, by.value = 0.5))

        if(class(try.return)[1] != "try-error"){

          obj.value <- objective_fun(sim = interpolated,
                                     obs = data.frame(obs.list[[index]]),
                                     fun = objective.function,
                                     start.date,
                                     end.date,
                                     min.depth = 0,
                                     max.depth = max.depth,
                                     by.value = 0.5)

          if(exists("NSE.list")){
            NSE.list[[var]][i] <- obj.value['NSE']
          }
          if(exists("RMSE.list")){
            RMSE.list[[var]][i] <- obj.value['RMSE']
          }

          if(exists("MAE.list")){
            MAE.list[[var]][i] <- obj.value['MAE']
          }

          if(exists("RAE.list")){
            RAE.list[[var]][i] <- obj.value['RAE']
          }

          if(exists("Pearson.list")){
            Pearson.list[[var]][i] <- obj.value['Pearson']
          }
        }
        else{
          if(exists("NSE.list")){
            NSE.list[[var]][i]<-NA
          }
          if(exists("RMSE.list")){
            RMSE.list[[var]][i]<-NA
          }
          if(exists("MAE.list")){
            MAE.list[[var]][i]<-NA
          }
          if(exists("RAE.list")){
            RAE.list[[var]][i]<-NA
          }
          if(exists("Pearson.list")){
            Pearson.list[[var]][i]<-NA
          }
        }
      }
    }

    if(exists("NSE.list")){
      nse.df <- data.frame(t(do.call(rbind.data.frame, NSE.list)))
      colnames(nse.df) <- paste0("NSE.", colnames(nse.df))
      para.df <- cbind(para.df, nse.df)
    }

    if(exists("RMSE.list")){
      rmse.df <- data.frame(t(do.call(rbind.data.frame, RMSE.list)))
      colnames(rmse.df) <- paste0("RMSE.", colnames(rmse.df))
      para.df <- cbind(para.df, rmse.df)
    }

    if(exists("MAE.list")){
      mae.df <- data.frame(t(do.call(rbind.data.frame, MAE.list)))
      colnames(mae.df) <- paste0("MAE.", colnames(mae.df))
      para.df <- cbind(para.df, mae.df)
    }

    if(exists("RAE.list")){
      rae.df <- data.frame(t(do.call(rbind.data.frame, RAE.list)))
      colnames(rae.df) <- paste0("RAE.", colnames(rae.df))
      para.df <- cbind(para.df, rae.df)
    }

    if(exists("Pearson.list")){
      pearson.df <- data.frame(t(do.call(rbind.data.frame, Pearson.list)))
      colnames(pearson.df) <- paste0("Pearson.", colnames(pearson.df))
      para.df <- cbind(para.df, pearson.df)
    }
  }

  if(write.out){
    write.csv(para.df, file = file.name, row.names = FALSE)
  }

  return(para.df)
}
