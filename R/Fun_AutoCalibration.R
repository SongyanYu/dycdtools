#' Automatically calibrate DYRESM-CAEDYM model.
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
#' @param objective.function a vector of string character claiming what objective function(s) to be used for calibration. either Nash-Sutcliffe Efficiency coefficient ("nse") or Root Mean Square Error ("rmse")
#' @param start.date,end.date the beginning and ending simulation dates for the intended DYRESM-CAEDYM model run. The date format must be "\%Y-\%m-\%d".
#' @param dycd.wd working directory where input files (including the bat file) to DYCD are stored.
#' @param dycd.output a character string naming the output file from the model run.
#' @param file_name a character string naming a png file for writing auto-calibration results.
#'
#' @import dplyr
#' @importFrom utils read.csv write.csv
#' @return write out a csv file with trialed values of parameters and corresponding values of objective function (RMSE).
#'
#' @export

autoCalibration<-function(cal.para="Data/Calibration parameters.csv",
                          combination="random",n=1,
                          model.var=c("TEMP","DO","TN","TP","NO3","PO4","NH4","SALINITY"),
                          phyto.group=NA,
                          obs.data="Data/Obs LHM.csv",
                          objective.function=c("nse","rmse"),
                          start.date="2017-06-06",end.date="2020-02-29",
                          dycd.wd="Data/200318-lhm-ref",
                          dycd.output="Data/200318-lhm-ref/DYsim.nc",
                          file_name="Data/auto-calibration.csv"){

  #---
  # 1.combination of parameter values
  #---
  para.raw<-read.csv(file=cal.para)

  seq.list<-list()
  for(i in 1:nrow(para.raw)){
    seq.list[[i]]<-seq(para.raw$Min[i],para.raw$Max[i],by=para.raw$Increment[i])
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
  # 4. observed lake data
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
  # 6. auto-calibration
  #---
  for(i in iteration){
    cat(i,"/",length(iteration),"\n")

    #---
    # change the parameter values in the input files
    #---
    for(m in 1:ncol(para.df)){
      change_input_file(input_file = paste(para.raw$Input_file[m]),
                        row_no = para.raw$Line_NO[m],
                        new_value = para.df[i,m])
      #cat(m,"\n")
    }

    #---
    # model simulation / run the .bat file
    #---
    current.wd<-getwd()
    setwd(dycd.wd)
    bat.file<-list.files(pattern = ".bat")
    shell(bat.file,intern = TRUE,wait=TRUE)
    setwd(current.wd)

    #---
    # calculate objective function values (NSE, RMSE)
    #---

    var.values<-extract.output(dycd.output = dycd.output,
                               var.extract = actual.model.var)

    if("CHLA" %in% model.var){
      actual.model.var.2<-append(actual.model.var,phyto.group)
      actual.model.var.2<-actual.model.var.2[-which(actual.model.var.2=="CHLA")]

      var.values<-extract.output(dycd.output = dycd.output,
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
        if(exists("nse.list")){
          nse.list[[var]][i]<-nse.rmse(sim = interpolated,
                                       obs = data.frame(obs.list[[index]]),
                                       start.date,end.date,
                                       min.depth = 0,max.depth = max.depth,by.value = 0.5)[1]
        }
        if(exists("rmse.list")){
          rmse.list[[var]][i]<-nse.rmse(sim=interpolated,
                                        obs=data.frame(obs.list[[index]]),
                                        start.date,end.date,
                                        min.depth = 0,max.depth = max.depth,by.value = 0.5)[2]
        }
      }
      else{
        if(exists("nse.list")){
          nse.list[[var]][i]<-NA
        }
        if(exists("rmse.list")){
          rmse.list[[var]][i]<-NA
        }
      }
    }
  }

  if(exists("nse.list")){
    nse.df<-data.frame(nse.list)
    colnames(nse.df)<-paste0("NSE.",colnames(nse.df))
    para.df<-cbind(para.df[iteration,],nse.df[iteration,])
  }

  if(exists("rmse.list")){
    rmse.df<-data.frame(rmse.list)
    colnames(rmse.df)<-paste0("RMSE.",colnames(rmse.df))
    para.df<-cbind(para.df[iteration,],rmse.df[iteration,])
  }

  write.csv(para.df,file = file_name,row.names = FALSE)
}

