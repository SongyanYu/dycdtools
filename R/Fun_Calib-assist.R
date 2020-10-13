#' Assist calibration of DYRESM-CAEDYM model. Before using this function, make sure that you have set up "Bin" and "Files" sub-folders under the DYCD model folder.
#'
#' @description This function tries different combinations of selected parameter values and
#'   outputs corresponding values of fit-of-goodness by calculating some objective functions.
#'   Then users can choose the optimal set of parameter values to calibrate the model.
#'
#' @param cal.para a character string naming a file where parameters to be calibrated and their value ranges. this file need to have fixed colnames.
#' @param combination a vector of string character of how to pick up combinations of parameter values."random" or "all".
#' @param n the number of randomly selections. Must be provided if combination = random.
#' @param model.var a vector of string character of modelled variables for calibration.
#'       When multiple phytoplankton groups will be combined for calibration, use "CHLA" and the following argument of "phyto.group" to specify them.
#'       When phytoplankton groups will be calibrated separately, put their abbrev. in this argument. Currently, five abbrevs are supported: CHLOR, FDIAT, NODUL, CYANO and CRYPT.
#' @param phyto.group a vector of simulated phytoplankton groups, including CHLOR, FDIAT, NODUL, CYANO and CRYPT.
#' @param obs.data a character string naming a file of observed lake data. This file need to have fixed column names and orders.
#' @param objective.function a vector of string character claiming what objective function(s) to be used for calibration.
#' Selected from the following five functions: "NSE": Nash-Sutcliffe efficiency coefficient, "RMSE": Root Mean Square Error,
#'    "MAE": Mean Absolute Error, "RAE": Relative Absolute Error, "Pearson": Pearson's r.
#' @param start.date,end.date the beginning and ending simulation dates for the intended DYRESM-CAEDYM model run. The date format must be "\%Y-\%m-\%d".
#' @param dycd.wd working directory where input files (including the bat file) to DYRESM-CAEDYM are stored.
#' @param dycd.output a character string naming the output file from the model run.
#' @param file_name a character string naming a png file for writing out the auto-calibration results.
#' @param verbose if TRUE, the auto-calibration information is printed.
#' @param parallel if TRUE, the calibration process can be run on multiple cores.
#' @param n.cores When parallel is TRUE, n.cores is the number of cores the calibratio function will be run on. If not provided,
#'        the default value is the number of available cores on the computer -1.
#' @param write.out if TRUE, the auto-calibration results are saved a file with a file name set by the "file_name" argument.
#'
#' @import dplyr
#' @importFrom parallel detectCores makeCluster stopCluster clusterExport
#' @importFrom R.utils copyDirectory
#' @importFrom utils read.csv write.csv
#' @return a dataframe of trialed values of parameters and corresponding values of objective function(s).
#'
#' @note No executable examples are provided to illustrate the use of this function, as this function relies on the DYRESM-CAEDYM executables to work.
#'
#' @export

calib.assist<-function(cal.para="Data/Calibration parameters.csv",
                       combination="random",n=1,
                       model.var=c("TEMP","DO","TN","TP","NO3","PO4","NH4","SALINITY"),
                       phyto.group=NA,
                       obs.data="Data/Obs.csv",
                       objective.function=c("NSE","RMSE"),
                       start.date="2017-06-06",end.date="2020-02-29",
                       dycd.wd="work_directory",
                       dycd.output="work_directory/DYsim.nc",
                       file_name="work_directory/Calibration.csv",
                       verbose=TRUE,
                       parallel=TRUE,
                       n.cores=NULL,
                       write.out=FALSE){

  #---
  # 1.combination of parameter values
  #---
  para.raw<-read.csv(file=cal.para)

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
    sim.cores <- rep(1:n.cores, length.out=iteration)

    # find the files that are needed/wanted to run this model (par, bio, chm, sed files will copy later)
    files.model <- list.files(path = dycd.wd,pattern = '.*\\.(bat|cfg|con|inf|int|met|pro|stg|wdr)$',
                              recursive = FALSE, include.dirs = TRUE, full.names = TRUE)

    # create sacrificial folders with a copy of the model for each core being used
    for (c in 1:n.cores) {
      dir.core <- paste0(dycd.wd,'/core',c)
      dir.create(dir.core)
      file.copy(files.model, to = dir.core, recursive = T, overwrite = T)
      R.utils::copyDirectory(paste0(dycd.wd, "/Bin"), to = paste0(dir.core, "/Bin"), recursive = T, overwrite = T)
      R.utils::copyDirectory(paste0(dycd.wd, "/Files"), to = paste0(dir.core, "/Files"), recursive = T, overwrite = T)
    }

    #### define function to run (and process?) the model on each core
    run.iteration <- function(this.sim, dycd.wd) {

      # which core to run it on?
      core <- sim.cores[this.sim]

      # id folder to use
      dir.this <- paste0(dycd.wd,'/core', core)

      # copy the parameters files from the base model folder into the iteration's folder
      files.params <- list.files(path = dycd.wd,pattern = '.*\\.(par|chm|bio|sed)$',
                                 recursive = FALSE, include.dirs = TRUE, full.names = TRUE)
      file.copy(files.params, dir.this, overwrite = TRUE)


      ### !!!!!!! call code to modify the parameters files as needed here

      #---
      # change the parameter values in the input files
      #---
      for(m in 1:ncol(para.df)){
        change_input_file(input_file = paste(dir.this,"/",para.raw$Input_file[m]),
                          row_no = para.raw$Line_NO[m],
                          new_value = para.df[this.sim,m])
      }

      #---
      # model simulation / run the .bat file
      #---
      user.wd <- getwd()
      on.exit(setwd(user.wd))
      setwd(dir.this)
      bat.file <- list.files(pattern = ".bat")
      shell(bat.file,intern = TRUE,wait=TRUE)
      setwd(user.wd)

      ## verify that the sim has completed successfully
      log <- file(paste0(dir.this,'/dy.log'))
      open(log)
      out <- scan(log,10,what="char(0)",sep="\n",quiet=TRUE)

      while(TRUE){
        tmp <- scan(log,1,what="char(0)",sep="\n",quiet=TRUE)
        if(length(tmp)==0) {close(log) ; break }
        out <- c(out[-1],tmp)
      }

      if (!TRUE %in% grepl("END: LAKE SIMULATION",out)) {
        warning(paste0("Simulation No.",this.sim," did not complete successfully.\n"))
      }

      # copy the finished nc file for later processing (if needed)
      file.copy(from = paste0(dir.this,"/DYsim.nc"),
                to = paste0(dir.output,"/DYsim_",this.sim,".nc"))

      ### ! can also add additional model processing here and write to the output folder,
      ###   to take advantage of parallel speed if wanted

    } # end per core function

    ##### initiate and execute the cluster run
    # clean up cluster if left over from last time
    try({parallel::stopCluster(cl)})

    cl <- parallel::makeCluster(n.cores)

    # export any necessary objects and/or functions to the cluster before running
    parallel::clusterExport(cl, varlist = c("run.iteration","dycd.wd","dir.output","sim.cores"),
                            envir = .GlobalEnv)

    parallel::clusterApply(cl, 1:n.sims, run.iteration, dycd.wd)

    # clean up cluster
    try({parallel::stopCluster(cl)})


    ##### clean up the created folders
    unlink(paste0(dycd.wd,"/core",seq(1:n.cores)), recursive = T)

    #---
    # calculate objective function values
    #---
for(b in 1:iteration){
  var.values<-ext.output(dycd.output = paste0(dir.output,"/DYsim_",b,".nc"),
                         var.extract = actual.model.var)

  if("CHLA" %in% model.var){
    actual.model.var.2<-append(actual.model.var,phyto.group)
    actual.model.var.2<-actual.model.var.2[-which(actual.model.var.2=="CHLA")]

    var.values<-ext.output(dycd.output = dycd.output,
                           var.extract = actual.model.var.2)
  }

  for(n in 1:length(var.values)){
    expres<-paste0(names(var.values)[n],"<-data.frame(var.values[[",n,"]])")
    eval(parse(text=expres))
  }

  lake.depth.list<-apply(dyresmLAYER_HTS_Var,2,FUN = function(a) hgt.to.dpt(a[!is.na(a)]))
  max.depth<-ceiling(max(unlist(lake.depth.list)))

  for (var in actual.model.var){
    index<-match(var,actual.model.var)

    if(var=="CHLA"){
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

    if(class(try.return)!="try-error"){
      if(exists("NSE.list")){
        NSE.list[[var]][b]<-objective.fun(sim = interpolated,
                                          obs = data.frame(obs.list[[index]]),
                                          fun="NSE",
                                          start.date,end.date,
                                          min.depth = 0,max.depth = max.depth,by.value = 0.5)[1]
      }
      if(exists("RMSE.list")){
        RMSE.list[[var]][b]<-objective.fun(sim=interpolated,
                                           obs=data.frame(obs.list[[index]]),
                                           fun="RMSE",
                                           start.date,end.date,
                                           min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
      }

      if(exists("MAE.list")){
        MAE.list[[var]][b]<-objective.fun(sim=interpolated,
                                          obs=data.frame(obs.list[[index]]),
                                          fun="MAE",
                                          start.date,end.date,
                                          min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
      }

      if(exists("RAE.list")){
        RAE.list[[var]][b]<-objective.fun(sim=interpolated,
                                          obs=data.frame(obs.list[[index]]),
                                          fun="RAE",
                                          start.date,end.date,
                                          min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
      }

      if(exists("Pearson.list")){
        Pearson.list[[var]][b]<-objective.fun(sim=interpolated,
                                              obs=data.frame(obs.list[[index]]),
                                              fun="Pearson",
                                              start.date,end.date,
                                              min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
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
      para.df<-cbind(para.df[iteration,],NSE=nse.df[iteration,])
    }

    if(exists("RMSE.list")){
      rmse.df<-data.frame(RMSE.list)
      colnames(rmse.df)<-paste0("RMSE.",colnames(rmse.df))
      para.df<-cbind(para.df[iteration,],RMSE=rmse.df[iteration,])
    }

    if(exists("MAE.list")){
      mae.df<-data.frame(MAE.list)
      colnames(mae.df)<-paste0("MAE.",colnames(mae.df))
      para.df<-cbind(para.df[iteration,],MAE=mae.df[iteration,])
    }

    if(exists("RAE.list")){
      rae.df<-data.frame(RAE.list)
      colnames(rae.df)<-paste0("RAE.",colnames(rae.df))
      para.df<-cbind(para.df[iteration,],RAE=rae.df[iteration,])
    }

    if(exists("Pearson.list")){
      pearson.df<-data.frame(Pearson.list)
      colnames(pearson.df)<-paste0("Pearson.",colnames(pearson.df))
      para.df<-cbind(para.df[iteration,],PearsonR=pearson.df[iteration,])
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
      change_input_file(input_file = paste(dycd.wd,"/",para.raw$Input_file[m]),
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

    var.values<-ext.output(dycd.output = dycd.output,
                           var.extract = actual.model.var)

    if("CHLA" %in% model.var){
      actual.model.var.2<-append(actual.model.var,phyto.group)
      actual.model.var.2<-actual.model.var.2[-which(actual.model.var.2=="CHLA")]

      var.values<-ext.output(dycd.output = dycd.output,
                             var.extract = actual.model.var.2)
    }

    for(n in 1:length(var.values)){
      expres<-paste0(names(var.values)[n],"<-data.frame(var.values[[",n,"]])")
      eval(parse(text=expres))
    }

    lake.depth.list<-apply(dyresmLAYER_HTS_Var,2,FUN = function(a) hgt.to.dpt(a[!is.na(a)]))
    max.depth<-ceiling(max(unlist(lake.depth.list)))

    for (var in actual.model.var){
      index<-match(var,actual.model.var)

      if(var=="CHLA"){
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

      if(class(try.return)!="try-error"){
        if(exists("NSE.list")){
          NSE.list[[var]][i]<-objective.fun(sim = interpolated,
                                            obs = data.frame(obs.list[[index]]),
                                            fun="NSE",
                                            start.date,end.date,
                                            min.depth = 0,max.depth = max.depth,by.value = 0.5)[1]
        }
        if(exists("RMSE.list")){
          RMSE.list[[var]][i]<-objective.fun(sim=interpolated,
                                             obs=data.frame(obs.list[[index]]),
                                             fun="RMSE",
                                             start.date,end.date,
                                             min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
        }

        if(exists("MAE.list")){
          MAE.list[[var]][i]<-objective.fun(sim=interpolated,
                                            obs=data.frame(obs.list[[index]]),
                                            fun="MAE",
                                            start.date,end.date,
                                            min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
        }

        if(exists("RAE.list")){
          RAE.list[[var]][i]<-objective.fun(sim=interpolated,
                                            obs=data.frame(obs.list[[index]]),
                                            fun="RAE",
                                            start.date,end.date,
                                            min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
        }

        if(exists("Pearson.list")){
          Pearson.list[[var]][i]<-objective.fun(sim=interpolated,
                                                obs=data.frame(obs.list[[index]]),
                                                fun="Pearson",
                                                start.date,end.date,
                                                min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
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
    para.df<-cbind(para.df[iteration,],NSE=nse.df[iteration,])
  }

  if(exists("RMSE.list")){
    rmse.df<-data.frame(RMSE.list)
    colnames(rmse.df)<-paste0("RMSE.",colnames(rmse.df))
    para.df<-cbind(para.df[iteration,],RMSE=rmse.df[iteration,])
  }

  if(exists("MAE.list")){
    mae.df<-data.frame(MAE.list)
    colnames(mae.df)<-paste0("MAE.",colnames(mae.df))
    para.df<-cbind(para.df[iteration,],MAE=mae.df[iteration,])
  }

  if(exists("RAE.list")){
    rae.df<-data.frame(RAE.list)
    colnames(rae.df)<-paste0("RAE.",colnames(rae.df))
    para.df<-cbind(para.df[iteration,],RAE=rae.df[iteration,])
  }

  if(exists("Pearson.list")){
    pearson.df<-data.frame(Pearson.list)
    colnames(pearson.df)<-paste0("Pearson.",colnames(pearson.df))
    para.df<-cbind(para.df[iteration,],PearsonR=pearson.df[iteration,])
  }

}

  return(para.df)

  if(write.out){
    write.csv(para.df,file = file_name,row.names = FALSE)
  }
}



