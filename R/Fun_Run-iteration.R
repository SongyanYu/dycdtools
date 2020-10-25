#' Internal function to provide parallel processing surport to the calibration assistant function.
#' @param this.sim a numeric denoting which parameter combination to be tried.
#' @param dycd.wd working directory where input files (including the bat file) to DYRESM-CAEDYM are stored.
#'
#' @importFrom parallel detectCores makeCluster stopCluster clusterExport
#' @importFrom R.utils copyDirectory

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
    change_input_file(input_file = paste0(dir.this,"/",para.raw$Input_file[m]),
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
