

############################################################################
##
#           Monte Carlo Function
##
#############################################################


#'@keywords internal
backtick_binaries<-function(vec_of_strings){
  for(i in 1:length(vec_of_strings)){
    if(substr(vec_of_strings[i], start=1, stop=1)=="%" && substr(vec_of_strings[i], start=nchar(vec_of_strings[i]), stop=nchar(vec_of_strings[i]))=="%"){
      vec_of_strings[i]<-paste("\'%",substr(vec_of_strings[i], start=2, stop=(nchar(vec_of_strings[i]))-1),"%\'", sep="",collapse="")
    }
  }
  vec_of_strings
}



#'Runs Monte Carlo. For details see documentation of wrapper function.
#'@keywords internal
#'@importFrom utils txtProgressBar
#'@import snowfall

MC_inner<-function(func, nrep, param_list, ret_vals, ncpus=2, max_grid=1000, packages=NULL, export_functions=NULL, debug=FALSE){
  
  # ------- extract information from parameter list
  n_param<-length(param_list)                                                       # number of parameters
  dim_vec<-numeric(n_param) 
  for(i in 1:n_param){dim_vec[i]<-length(param_list[[i]])}                          # construct vector with grid dimensions
  param_names<-names(param_list)                                                    # names of parameters
  for(i in 1:n_param)(assign(paste(param_names[i],"_grid",sep=""),param_list[[i]])) # create grid for each parameter
    
  subm_param<-paste(param_names,"=",param_names,",", sep="",collapse="")
  subm_param<-substr(subm_param,1,(nchar(subm_param)-1))
  eval(parse(text=paste("func2<-function(","aux,",subm_param,"){func(",subm_param,")}")))  
  
  grid_size<-prod(dim_vec)  
  cat(paste("Grid of ",grid_size, " parameter constellations to be evaluated.","\n","\n"))
  
  #------- setup progress bar
  
  if(ncpus>1){cat(paste("Simulation parallelized using",ncpus, "cpus.","\n","\n"))}
  cat(paste("Progress:","\n","\n"))
  pb <- txtProgressBar(min=0, max=grid_size, style=3)
  
  #------- create auxillary strings for loops and submission of parameters
  index<-paste("i",1:n_param, sep="")                                               # create indexes for loops
  assign_param_values<-numeric(n_param)
  for(i in 1:n_param){assign_param_values[i]<-paste(param_names[i],"_grid[",index[i],"]",sep="")} 
  assign_param_values<-paste(param_names,"=",assign_param_values,";",sep="",collapse="")

  
  #------- create list of arrays for results
  name_list<-list()
  for(i in 1:(n_param)){name_list[[i]]<-paste(param_names[i],"=",eval(parse(text=paste(param_names,"_grid",sep="")[i])), sep="")}
  name_list[[(n_param+1)]]<-paste("rep=",1:nrep,sep="")
  results<-list()
  for(i in 1:length(ret_vals)){
    results[[i]]<-array(NA, dim=c(dim_vec,nrep))
    dimnames(results[[i]])<-name_list
  }
  names(results)<-ret_vals
  
  #------- create loops
  
  libloc_strings<-.libPaths()
  
  s1<-paste(paste(c(paste("for(",index, " in 1:", dim_vec,"){", sep="",collapse=""),
                    "rep<-rep+1;setTxtProgressBar(pb, rep);"),collapse=""),assign_param_values, sep="",collapse="")
  
  in_export<-paste("'",c('func2','func','libloc_strings',export_functions,param_names),"'", collapse=",", sep="")
  aux.s2<-paste("if(ncpus>1){sfExport(",in_export,")};",sep="")
  
  s2<-paste("suppressMessages(sfInit(parallel=if(ncpus>1){TRUE}else{FALSE}, cpus = ncpus, type= 'SOCK'));",
            aux.s2,"sfClusterEval(.libPaths(libloc_strings));",
            if(length(packages)>0){paste("capture.output(suppressMessages(sfLibrary(",packages,")));", sep="", collapse="")}else{""},
            if(ncpus>1){"sfClusterSetupRNG();"}else{""},
            "erg<-sfApply(as.matrix(1:nrep,nrep,1),margin=1,fun=func2,",subm_param,");suppressMessages(sfStop());",sep="", collapse="")
  
  s3<-paste(paste(paste("results$",ret_vals,"[",sep=""),
            paste(paste(index,",", collapse="", sep=""),"]<-",sep="",collapse=""),sep=""),
            paste("as.numeric(unlist(erg)[which(gsub(' ', '_',names(unlist(erg)))==","'",ret_vals,"'",")]);",sep=""), collapse="")    
  s3<-substr(s3,1,(nchar(s3)-1))
  
  s4<-paste(rep("}",n_param),collapse="")
  
  all_loops<-paste(c(s1,s2,s3,s4), collapse="")
  rep<-0
  
  if(debug==TRUE){
    iterator<-c("nrep","ncpus","rep","pb","func","func2","results","libloc_strings")
    for(i in 1:length(iterator)){
      assign(iterator[i], get(iterator[i]), envir = .GlobalEnv)
    }
    for(i in 1:n_param){
      assign(paste(param_names[i], "_grid", sep=""), param_list[[i]], envir = .GlobalEnv)
    }
    format.aux<-gsub(pattern=";", replacement=" \n ", x=all_loops)
    format.aux<-gsub(pattern="\\{", replacement=" \\{\n ", x=format.aux)
    format.aux<-gsub(pattern="\\}", replacement=" \n\\} ", x=format.aux)
    cat("\n","\n")
    cat(format.aux)
    return("Inner loops printed and relevant variables defined in global environment.")
  }
  
  eval(parse(text=all_loops))
  return(results)
}


#############################################################

#' @title Parallized Monte Carlo Simulation
#' @description \code{MonteCarlo} runs a Monte Carlo simulation study for a correctly specified function and the desired parameter grids. 
#' See details for instructions on the specification of the function.
#' @details 
#' 
#' The user defined function \code{func} handles the generation of data, the application of the method of interest 
#' and the evaluation of the result for a single repetition and parameter combination. 
#' MonteCarlo handles the generation of loops over the desired parameter grids and the 
#' repetition of the Monte Carlo experiment for each of the parameter constellations.
#'  
#' There are two important formal requirements that \code{func} has to fulfill. 
#' 
#' 1. The arguments of \code{func} have to be scalar.
#' 
#' 2. The value returned by \code{func} has to be list of (unnamed) scalars (The list elements can be named). 
#' 
#' For the estimation of the required simulation time, 
#' a separate simulation is run on a reduced grid that only contains the extreme points 
#' for each parameter, e.g. the smallest and the largest sample size. 
#' This test simulation is carried out with \code{nrep/10} repetitions and the required 
#' simulation time is estimated by a linear interpolation. Since the computational complexity is
#' usually a convex function of the sample size and the dimension of the process, this approach 
#' tends to overestimate the time required. 
#' 
#' \code{export_also} allows to export data to the cluster in case parallized computations on a dataset are desired. 
#' It also allows to bypass the automatic export of functions and packages. 
#' To manually export a function or dataset or to load a package, pass a list to \code{export_also} where the list element is named
#' "functions", "data" and/or "packages". For example: \code{export_also=list("functions"=c("function_name_1", "function_name_2"), 
#' "packages"="package_name")}.
#' 
#' @param func The function to be evaluated. See details.
#' @param nrep An integer that specifies the desired number of Monte Carlo repetitions.
#' @param param_list A list whose components are named after the parameters of \code{func} and each component is a vector containing the desired grid values for that parameter
#' @param ncpus An integer specifying the number of cpus to be used. Default is \code{ncpus=1}. 
#' For \code{ncpus>1} the simulation is parallized automatically using \code{ncpus} cpu units.
#' @param raw Boolean that specifies whether the output should be averaged over the nrep repetitions. Default is \code{raw=TRUE}.
#' @param max_grid Integer that specifies for which grid size to throw an error, if grid becomes to large. Default is \code{max_grid=1000}.
#' @param time_n_test Boolean that specifies whether the required simulation time should be estimated (useful for large simulations or slow functions). 
#' See details. Default is \code{time_n_test=FALSE}.
#' @param save_res Boolean that specifies whether the results of \code{time_n_test} should be saved to the current directory. 
#' Default is \code{save_res=FALSE}.
#' @param debug Boolean that activates/deactivates the debug mode. If \code{debug=TRUE} all relevant variables are assigned to the global environment
#' and the core loop is printed. This allows to run it manually and to see how MonteCarlo works internally. Default is \code{debug=FALSE}.
#' @param export_also List specifying additional objects that are supposed to be exported to the cluster. 
#' This allows to export data or to bypass the automatic export of functions. Default is \code{export_also=NULL}. See details.
#' @return A list of type \code{MonteCarlo}.
#' @import codetools
#' @importFrom utils capture.output
#' @importFrom utils packageDescription
#' @examples
#' test_func<-function(n,loc,scale){
#'  sample<-rnorm(n, loc, scale)
#'  stat<-sqrt(n)*mean(sample)/sd(sample)
#'  decision<-abs(stat)>1.96
#'  return(list("decision"=decision))
#'}
#'
#'# Example without parallization
#'n_grid<-c(50,100,250,500)
#'loc_grid<-seq(0,1,0.2)
#'scale_grid<-c(1,2)
#'
#'param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
#'erg<-MonteCarlo(func=test_func, nrep=1000, param_list=param_list, ncpus=1)
#'summary(erg)
#'
#'rows<-c("n")
#'cols<-c("loc","scale")
#'MakeTable(output=erg, rows=rows, cols=cols, digits=2)
#'
#'@export

MonteCarlo<-function(func, nrep, param_list, ncpus=1, max_grid=1000, time_n_test=FALSE, save_res=FALSE, debug=FALSE, raw=TRUE, export_also=NULL){

  # -------- check whether arguments supplied to function are admissable 
    
  if(is.function(func)==FALSE)stop("func must be a function")
  if(is.list(param_list)==FALSE)stop("param_list must be a list containing the names of the function arguments and the vectors of values that are supposed to be passed as arguments.")
  for(i in 1:length(param_list)){if(is.vector(param_list[[i]])==FALSE)stop("Parameter grids have to be vectors.")}
  if((round(nrep)==nrep&&nrep>0)==FALSE)stop("nrep must be a positive integer.")
  
  # -------- Make test run so that error messages are thrown right away and extract names of list elements
  
  param_names<-names(param_list)
  test_run<-eval(parse(text=paste("func(",paste(param_names,"=param_list[[",1:length(param_names),"]][1]", sep="", collapse=","),")", collapse="", sep="")))
  ret_vals<-gsub(" ", "_", names(test_run))
  
  if(is.list(test_run)==FALSE)stop("func has to return a list with named components. each component has to be scalar.")
  for(i in 1:length(test_run)){if(is.null(names(test_run[[i]]))==FALSE)stop("Scalars in list components cannot be named.")}
  for(i in 1:length(test_run)){if(length(test_run[[i]])>1)stop("func has to return a list with named components. Each component has to be scalar.")}
  
  # --------- Check which functions that are called in func, match functions in the global environment and prepare to export those
  
  all<-ls(name=.GlobalEnv)
  all<-backtick_binaries(all)
  all_funcs<-NULL
  for(i in 1:length(all)){
    if(is.function(eval(parse(text=all[i])))){all_funcs<-c(all_funcs,all[i])}
  }
  globals_in_func<-findGlobals(func)
  in_func_aux<-globals_in_func # extract functions used in func

  # --------------   remove <- from function name for replacement functions
  
  in_func_aux2<-gsub("<-","",in_func_aux)
  in_func_aux<-backtick_binaries(in_func_aux2[in_func_aux2!=""])
  
  ####################################################
  
  in_func<-NULL # loop to sort out primitive functions
  for(i in 1:length(in_func_aux)){if(is.function(tryCatch(.Primitive(in_func_aux[i]), error=function(e)FALSE))==FALSE){in_func<-c(in_func,in_func_aux[i])}}
  
  new_funcs<-subset(in_func,in_func%in%all_funcs)
  export_functions<-new_funcs
    
  # -- loop through deeper functions to find functions called by functions in func and deeper nested functions
  
  all_funcs_found<-in_func # create array of names of all functions found so that the packages required can be found below
  while(length(new_funcs)>0){ # while loop runs as long as new functions are found in deeper layers 
    n_exp<-length(new_funcs)
    new_funcs2<-NULL
    for(i in 1:n_exp){# find functions used in every new function
      in_inner_aux<-findGlobals(eval(parse(text=new_funcs[i])))  
      in_inner<-NULL # loop to sort out primitive functions
      for(i in 1:length(in_inner_aux)){
        if(is.function(tryCatch(.Primitive(in_inner_aux[i]), error=function(e)FALSE))==FALSE){
          if(tryCatch(is.function(eval(parse(text=in_inner_aux[i]))), error=function(e)FALSE)){
            in_inner<-c(in_inner,in_inner_aux[i])
          }
        }
      }
      new_funcs2<-c(new_funcs2,subset(in_inner,in_inner%in%all_funcs)) # list those new functions found that are defined in global environment
      if(length(in_inner)>0){all_funcs_found<-unique(c(all_funcs_found,in_inner[apply(as.matrix(in_inner),1,exists)]))} # determine which of the functions defined in inner functions do exist and append to list of all functions that may come from packages
    }
    new_funcs<-new_funcs2
    export_functions<-c(export_functions,new_funcs)
  }
  
  # -- add everything that is specified in export_also
  
  export_functions<-c(export_functions,export_also$functions,export_also$data)
  
  
  # -------- Find out which packages have to be loaded into cluster
  
  all_env<-search() #list all environments
  env_names<-unlist(strsplit(all_env[grep(":", all_env)], split=":")) # keep only those environments that refer to packages
  env_names<-env_names[-which(env_names=="package")]
  
  packages<-NULL #loop through non-primitive functions used in func and check from which package they are
  for(i in 1:length(all_funcs_found)){
    if(environmentName(environment(eval(parse(text=all_funcs_found[i]))))%in%env_names){
      packages<-c(packages,env_names[which(env_names==environmentName(environment(eval(parse(text=all_funcs_found[i])))))])
    }
  }
  packages<-unique(packages[packages!="base"])
  
  dependencies_list<-NULL # loop through packages found and collect their dependencies in character vector
  if(length(packages)>0){
    for(i in 1:length(packages)){
      dependencies_list<-c(dependencies_list,unlist(strsplit(as.character(packageDescription(packages[i])$Depends), split=",")))
    }
    dependencies_list<-unique(dependencies_list)
    dependencies_list<-gsub(" ","",dependencies_list)
    sort_out<-which(packages%in%dependencies_list)
    if(length(sort_out)>0){packages<-packages[-sort_out]}  # keep only those packages that are not automatically included because they are dependencies
  }
  
  # -- add packages that are specified in export_also
  
  packages<-c(packages,export_also$packages)
  
  # --------- sort param list according to order in arg_list
  
  arg_list<-as.list(args(func))
  arg_list<-arg_list[which(names(arg_list)!="")]
  input.param<-0
  for(i in 1:length(param_list)){input.param[i]<-(names(param_list))[i]}
  param_list<-param_list[order(match(input.param,names(arg_list)))] 
  
  # -------- throw error if function arguments with no default are not supplied
  
  for(i in 1:length(arg_list)){
    if(is.symbol(arg_list[[i]])){
      if(names(arg_list)[i]%in%input.param==FALSE)stop(paste("param_list does not contain parameter values for", names(arg_list)[i]))
    }
  }
  
  # ------- extract information from parameter list
  
  n_param<-length(param_list)                                                       # number of parameters
  dim_vec<-numeric(n_param) 
  for(i in 1:n_param){dim_vec[i]<-length(param_list[[i]])}                          # construct vector with grid dimensions
  param_names<-names(param_list)                                                    # names of parameters
  grid_size<-prod(dim_vec)
  
  if(grid_size>max_grid)stop("Grid size is very large. If you still want to run the simulation change max_grid.")
  
  if(time_n_test==TRUE){
    cat("Running test simulation to estimate simulation time required.","\n","\n")
    param_list2<-list()
    for(i in 1:length(param_list)){
      param_list2[[i]]<-if(length(param_list[[i]])>1){c(min(param_list[[i]]), max(param_list[[i]]))}else{param_list[[i]]}
    }
    names(param_list2)<-names(param_list)
    dim_vec2<-numeric(length(param_list2)) 
    for(i in 1:n_param){dim_vec2[i]<-length(param_list2[[i]])}                          # construct vector with grid dimensions
    grid_size2<-prod(dim_vec2)
    t1<-Sys.time()
    erg_pre<-MC_inner(func=func, nrep=nrep/10, param_list=param_list2, ret_vals=ret_vals, ncpus=ncpus, max_grid=max_grid, packages=packages, export_functions=export_functions, debug=debug)
    t2<-Sys.time()
    time<-(t2-t1)*grid_size/grid_size2*10    
    cat(paste("Estimated time required:", round(as.numeric(time)), attributes(time)$units, "\n", "\n"))
    if(save_res==TRUE){
      time_done<-Sys.time()    
      filename<-paste("MonteCarloTest", "_", as.character(as.Date(time_done)), "_", gsub(":", "-", x=format(time_done,"%H:%M:%S")), ".Rdata", sep="")
      save(erg_pre, file=filename)
      cat(paste("Output of testrun written to:", paste(getwd(), "/", filename, sep=""), "\n", "\n"))
    }
  }
  t1<-Sys.time()
  erg<-MC_inner(func=func,nrep=nrep,param_list=param_list, ret_vals=ret_vals, ncpus=ncpus, max_grid=max_grid, packages=packages, export_functions=export_functions, debug=debug)
  t2<-Sys.time()
  out<-list()
  class(out)<-"MonteCarlo"
  out$results<-erg
  out$param_list<-param_list
  out$meta<-list("nrep"=nrep, "ncpus"=ncpus, "time"=round(t2-t1, digits=2),"func"=func, "raw"=raw)
  
  
  if(raw==TRUE){
    invisible(out)
  }else{
    results<-out$results
    output<-list()
    for(i in 1:length(ret_vals)){output[[i]]<-apply(results[[i]], 1:n_param, mean)}
    names(output)<-ret_vals
    cat("\n")
    cat(paste("\n", "Results:", "\n", "\n"))
    return(output)
  }
}


#'@export
summary.MonteCarlo<-function(object, ...){

  cat("Simulation of function: \n\n")
  print(object$meta$func)
  cat("\n")
  cat("Required time:", object$meta$time, units(object$meta$time), "for nrep =", object$meta$nrep, " repetitions on", object$meta$ncpus, "CPUs \n\n")
  cat("Parameter grid:", "\n\n")
  for(i in 1:length(object$param_list)){
    nchar_max<-max(nchar(names(object$param_list)))    
    cat(if(nchar(names(object$param_list)[i])!=nchar_max){paste(rep(" ",(nchar_max-nchar(names(object$param_list)[i]))),collapse="")}else{""},names(object$param_list)[i],":", object$param_list[[i]], "\n")
  }
  cat("\n","\n")
  cat(length(object$results),"output arrays of dimensions:",dim(object$results[[1]]))
  
}
