#' Assist calibration of DYRESM-CAEDYM model. Before using this function, make sure that you have set up "Bin" and "Files" sub-folders under the DYCD model folder.
#'
#' @description This function tries different combinations of selected parameter values and
#'   outputs corresponding values of fit-of-goodness by calculating some objective functions.
#'   Then users can choose the optimal set of parameter values to calibrate the model.
#'
#' @param cal.para a data frame or a character string naming an external .csv file where below columns are included:
#'                 "Parameter" where parameter names (abbreviation is allowed),
#'                 "Min", "Max", and "Increment" describing the minimum and maximum parameter values and expected increment in the value range,
#'                 "Input_file" and "Line_NO" listing in which configuration file at which line can the parameter can be found.
#' @param combination a vector of string character of how to pick up combinations of parameter values."random" - the function randomly
#'    picks up given number of combinations; "all" - the function tries all possible combinations of parameter values.
#' @param n the number of randomly selections. Must be provided if combination = "random".
#' @param model.var a vector of string character of modelled variables for calibration.
#'       If the chlorophyll of multiple phytoplankton groups is used collectively for calibration, use "CHLA" and further specify which phytoplankton groups are to be combined in the argument of "phyto.group".
#'       If phytoplankton groups are separately calibrated, list their abbreviation in this argument. Five abbreviations are supported: CHLOR, FDIAT, NODUL, CYANO and CRYPT.
#' @param phyto.group a vector of simulated phytoplankton groups, including CHLOR, FDIAT, NODUL, CYANO and CRYPT.
#' @param obs.data a character string naming a file of observed lake data. This file needs to be prepared in a given format (see example data).
#' @param objective.function a vector of string character claiming what objective function(s) to be used for calibration.
#' Selected from the following five functions: "NSE": Nash-Sutcliffe efficiency coefficient, "RMSE": Root Mean Square Error,
#'    "MAE": Mean Absolute Error, "RAE": Relative Absolute Error, "Pearson": Pearson's r.
#' @param start.date,end.date the beginning and ending simulation dates for the intended DYRESM-CAEDYM calibration. The date format must be "\%Y-\%m-\%d".
#' @param dycd.wd the directory where input files (including the bat file) to DYRESM-CAEDYM are stored. either relative or absolute path is allowed.
#' @param dycd.output a character string naming the output file of the model calibration.
#' @param file.name a character string naming a .csv file for writing out the auto-calibration results.
#' @param verbose if TRUE, the auto-calibration information is printed.
#' @param parallel if TRUE, the calibration process can be run on multiple cores.
#' @param n.cores When parallel is TRUE, n.cores is the number of cores the calibration function will be run on. If not provided,
#'        the default value is the number of available cores on the computer -1.
#' @param write.out if TRUE, the auto-calibration results are saved a file with a file name set by the "file.name" argument.
#'
#' @import dplyr
#' @importFrom utils read.csv write.csv
#' @return a dataframe of trialed values of parameters and corresponding values of objective function(s).
#'
#' @note No executable examples are provided to illustrate the use of this function, as this function relies on the DYRESM-CAEDYM executables to work.
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
    para.raw<-read.csv(file = cal.para)
  }

  seq.list<-list()
  for(i in 1:nrow(para.raw)){
    seq.list[[i]]<-seq(para.raw$Min[i],para.raw$Max[i],length.out =para.raw$N_values[i])
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
    iteration<-1:nrow(para.df)
  }
  if(combination =="random"){
    iteration<-base::sample(x=c(1:nrow(para.df)),size=n)
  }

  #---
  # 3. simulation period
  #---
  sim.date<-seq.Date(from = as.Date(start.date,format="%Y-%m-%d"),
                     to = as.Date(end.date,format="%Y-%m-%d"),
                     by="day")

  #---
  # 4. read observed lake data for calculating objective functions
  #---
  obs.lake<-read.csv(obs.data)
  obs.lake$Date<-as.Date(obs.lake$Date,format="%d/%m/%Y")
  obs.lake<-obs.lake%>%
    filter(Date>=sim.date[1]&Date<=sim.date[length(sim.date)])

  if(!all(model.var %in% colnames(obs.lake))){
    cat(model.var[!(model.var %in% colnames(obs.lake))],"are not provided for observed data, so that these variables will be excluded for model calibration.")
  }
  actual.model.var<-model.var[model.var %in% colnames(obs.lake)]
  obs.list<-lapply(match(actual.model.var,colnames(obs.lake)),FUN = function(col.no){obs.lake[,c(1,2,col.no)]})

  #---
  # 5. objective function
  #---
  for(obj.fun in objective.function){
    expres<-paste0(obj.fun,".list<-list()")
    eval(parse(text=expres))
  }

  #---
  # 6. Calibration
  #---

  if(parallel){

    # folder where you want outputs from the cluster to be written
    dir.output <- paste0(dycd.wd,"/clusteroutput")
    # create output directory
    dir.create(dir.output)

    # determine number of cores available to use
    if(is.null(n.cores)){
      n.cores <- parallel::detectCores() - 1
    }

    # allocate iterations to the available cores
    sim.cores <- rep(1:n.cores, length.out=length(iteration))

    # find the files that are needed/wanted to run this model (par, bio, chm, sed files will copy later)
    files.model <- list.files(path = dycd.wd, pattern = '.*\\.(bat|cfg|con|inf|int|met|pro|stg|wdr)$',
                              recursive = FALSE, include.dirs = TRUE, full.names = TRUE)

    # create sacrificial folders with a copy of the model for each core being used
    for (c in 1:n.cores) {
      dir.core <- paste0(dycd.wd,'/core',c)
      dir.create(dir.core)
      file.copy(files.model, to = dir.core, recursive = T, overwrite = T)
      R.utils::copyDirectory(paste0(dycd.wd, "/Bin"), to = paste0(dir.core, "/Bin"), recursive = T, overwrite = T)
      #R.utils::copyDirectory(paste0(dycd.wd, "/Files"), to = paste0(dir.core, "/Files"), recursive = T, overwrite = T)
    }

    ##### initiate and execute the cluster run
    # clean up cluster if left over from last time
    #try({parallel::stopCluster(cl)})

    cl <- parallel::makeCluster(n.cores)

    # export any necessary objects and/or functions to the cluster before running
    parallel::clusterEvalQ(cl, library("dycdtools"))
    parallel::clusterExport(cl, varlist = c("dycd.wd","dir.output","sim.cores","para.df","para.raw","iteration"),envir = environment())

    parallel::clusterApply(cl, 1:length(iteration), run_iteration, dycd.wd)

    # clean up cluster
    try({parallel::stopCluster(cl)})


    ##### clean up the created folders
    unlink(paste0(dycd.wd, "/core", seq(1:n.cores)), recursive = T)

    #---
    # calculate objective function values
    #---
    for(b in 1:length(iteration)){
      var.values <- ext_output(dycd.output = paste0(dir.output, "/DYsim_", b, ".nc"),
                               var.extract = actual.model.var)

      if("CHLA" %in% model.var){
        actual.model.var.2 <- append(actual.model.var, phyto.group)
        actual.model.var.2 <- actual.model.var.2[-which(actual.model.var.2 == "CHLA")]

        var.values <- ext_output(dycd.output = dycd.output,
                                 var.extract = actual.model.var.2)
      }

      for(n in 1:length(var.values)){
        expres <- paste0(names(var.values)[n], "<-data.frame(var.values[[", n, "]])")
        eval(parse(text = expres))
      }

      lake.depth.list <- apply(dyresmLAYER_HTS_Var, 2, FUN = function(a) hgt_to_dpt(a[!is.na(a)]))
      max.depth <- ceiling(max(unlist(lake.depth.list)))

      for (var in actual.model.var){
        index <- match(var,actual.model.var)

        if(var == "CHLA"){
          for(phyto in phyto.group){
            expres <- paste0("sim.", phyto, "<-", output_name$output.name[match(phyto, output_name$var.name)])
            eval(parse(text = expres))
          }
          expres <- paste0("sim.var<-", paste0("sim.", phyto.group, collapse = "+"))
          eval(parse(text = expres))
        }

        if(var != "CHLA"){
          expres <- paste0("sim.var<-", output_name$output.name[match(var, output_name$var.name)])
          eval(parse(text = expres))
        }

        try.return <- try(interpolated <- interpol(layerHeights = dyresmLAYER_HTS_Var,
                                                   var = sim.var,
                                                   min.depth = 0, max.depth = max.depth, by.value = 0.5))

        if(class(try.return)[1]!="try-error"){

          obj.value <- objective_fun(sim = interpolated,
                                     obs = data.frame(obs.list[[index]]),
                                     fun = objective.function,
                                     start.date, end.date,
                                     min.depth = 0, max.depth = max.depth, by.value = 0.5)

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
      nse.df<-data.frame(NSE.list)
      colnames(nse.df)<-paste0("NSE.",colnames(nse.df))
      para.df<-cbind(para.df[iteration,],NSE=nse.df)
    }

    if(exists("RMSE.list")){
      rmse.df<-data.frame(RMSE.list)
      colnames(rmse.df)<-paste0("RMSE.",colnames(rmse.df))
      para.df<-cbind(para.df[iteration,],RMSE=rmse.df)
    }

    if(exists("MAE.list")){
      mae.df<-data.frame(MAE.list)
      colnames(mae.df)<-paste0("MAE.",colnames(mae.df))
      para.df<-cbind(para.df[iteration,],MAE=mae.df)
    }

    if(exists("RAE.list")){
      rae.df<-data.frame(RAE.list)
      colnames(rae.df)<-paste0("RAE.",colnames(rae.df))
      para.df<-cbind(para.df[iteration,],RAE=rae.df)
    }

    if(exists("Pearson.list")){
      pearson.df<-data.frame(Pearson.list)
      colnames(pearson.df)<-paste0("Pearson.",colnames(pearson.df))
      para.df<-cbind(para.df[iteration,],PearsonR=pearson.df)
    }
  }

  if(!parallel){

    for(i in iteration){

      if(verbose){
        cat(i,"/",length(iteration),"\n")
      }

      #---
      # change the parameter values in the input files
      #---
      for(m in 1:ncol(para.df)){
        change_input_file(input_file = paste0(dycd.wd,"/",para.raw$Input_file[m]),
                          row_no = para.raw$Line_NO[m],
                          new_value = para.df[i,m])
      }

      #---
      # model simulation / run the .bat file
      #---
      user.wd<-getwd()
      on.exit(setwd(user.wd))
      setwd(dycd.wd)
      bat.file<-list.files(pattern = ".bat")
      shell(bat.file,intern = TRUE,wait=TRUE)
      setwd(user.wd)

      #---
      # calculate objective function values
      #---

      var.values <- ext_output(dycd.output = dycd.output,
                               var.extract = actual.model.var)

      if("CHLA" %in% model.var){
        actual.model.var.2<-append(actual.model.var,phyto.group)
        actual.model.var.2<-actual.model.var.2[-which(actual.model.var.2=="CHLA")]

        var.values <- ext_output(dycd.output = dycd.output,
                                 var.extract = actual.model.var.2)
      }

      for(n in 1:length(var.values)){
       expres<-paste0(names(var.values)[n],"<-data.frame(var.values[[",n,"]])")
       eval(parse(text=expres))
      }

      lake.depth.list<-apply(dyresmLAYER_HTS_Var,2,FUN = function(a) hgt_to_dpt(a[!is.na(a)]))
      max.depth<-ceiling(max(unlist(lake.depth.list)))

      for (var in actual.model.var){
        index <- match(var, actual.model.var)

        if(var == "CHLA"){
          for(phyto in phyto.group){
            expres<-paste0("sim.",phyto,"<-",output_name$output.name[match(phyto,output_name$var.name)])
            eval(parse(text=expres))
          }
          expres<-paste0("sim.var<-",paste0("sim.",phyto.group,collapse = "+"))
          eval(parse(text=expres))
        }

        if(!var=="CHLA"){
          expres<-paste0("sim.var<-",output_name$output.name[match(var,output_name$var.name)])
          eval(parse(text=expres))
        }

        try.return<-try(interpolated<-interpol(layerHeights = dyresmLAYER_HTS_Var,
                                               var = sim.var,
                                               min.depth = 0,max.depth = max.depth,by.value = 0.5))

        if(class(try.return)[1]!="try-error"){

          obj.value <- objective_fun(sim = interpolated,
                                     obs = data.frame(obs.list[[index]]),
                                     fun = objective.function,
                                     start.date, end.date,
                                     min.depth = 0, max.depth = max.depth, by.value = 0.5)

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
      nse.df<-data.frame(NSE.list)
      colnames(nse.df)<-paste0("NSE.",colnames(nse.df))
      para.df<-cbind(para.df[iteration,],NSE=nse.df)
    }

    if(exists("RMSE.list")){
      rmse.df<-data.frame(RMSE.list)
      colnames(rmse.df)<-paste0("RMSE.",colnames(rmse.df))
      para.df<-cbind(para.df[iteration,],RMSE=rmse.df)
    }

    if(exists("MAE.list")){
      mae.df<-data.frame(MAE.list)
      colnames(mae.df)<-paste0("MAE.",colnames(mae.df))
      para.df<-cbind(para.df[iteration,],MAE=mae.df)
    }

    if(exists("RAE.list")){
      rae.df<-data.frame(RAE.list)
      colnames(rae.df)<-paste0("RAE.",colnames(rae.df))
      para.df<-cbind(para.df[iteration,],RAE=rae.df)
    }

    if(exists("Pearson.list")){
      pearson.df<-data.frame(Pearson.list)
      colnames(pearson.df)<-paste0("Pearson.",colnames(pearson.df))
      para.df<-cbind(para.df[iteration,],PearsonR=pearson.df)
    }

  }

  if(write.out){
    write.csv(para.df,file = file.name,row.names = FALSE)
  }

  return(para.df)
}



