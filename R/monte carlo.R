

############################################################################
##
#           Monte Carlo Function
##
#############################################################

#'Runs Monte Carlo. For details see documentation of wrapper function.
#'@keywords internal
#'@import snowfall

MC.inner<-function(func,M,param.list, ret.vals, ncpus=1, raw=FALSE, max.grid=1000, packages=NULL, export.functions=NULL){
  
  # ------- extract information from parameter list
  n.param<-length(param.list)                                                       # number of parameters
  dim.vec<-numeric(n.param) 
  for(i in 1:n.param){dim.vec[i]<-length(param.list[[i]])}                          # construct vector with grid dimensions
  param.names<-names(param.list)                                                    # names of parameters
  for(i in 1:n.param)(assign(paste(param.names[i],".grid",sep=""),param.list[[i]])) # create grid for each parameter
  
  h1<-paste(param.names,",",sep="",collapse="")
  h1<-substr(h1,1,(nchar(h1)-1))
  eval(parse(text=paste("func2<-function(","aux,",h1,"){func(",h1,")}")))
  
  grid.size<-prod(dim.vec)
  # include stop condition for large grids
  cat(paste("Grid of ",grid.size, " parameter constellations to be evaluated.","\n","\n"))
  if(ncpus>1){cat(paste("Simulation parallelized using",ncpus, "cpus.","\n","\n"))}
  cat(paste("Progress:","\n","\n"))
  pb <- txtProgressBar(min=0, max=grid.size, style=3)
  
  #------- create auxillary strings for loops and submission of parameters
  index<-paste("i",1:n.param, sep="")                                               # create indexes for loops
  assign.param.values<-numeric(n.param)
  for(i in 1:n.param){assign.param.values[i]<-paste(param.names[i],".grid[",index[i],"]",sep="")} 
  assign.param.values<-paste(param.names,"=",assign.param.values,";",sep="",collapse="")
  subm.param<-paste(param.names,"=",param.names,",", sep="",collapse="")
  subm.param<-substr(subm.param,1,(nchar(subm.param)-1))
  
  #------- create list of arrays for results
  name.list<-list()
  for(i in 1:(n.param)){name.list[[i]]<-paste(param.names[i],"=",eval(parse(text=paste(param.names,".grid",sep="")[i])), sep="")}
  name.list[[(n.param+1)]]<-paste("rep=",1:M,sep="")
  results<-list()
  for(i in 1:length(ret.vals)){
    results[[i]]<-array(NA, dim=c(dim.vec,M))
    dimnames(results[[i]])<-name.list
  }
  names(results)<-ret.vals
  
  #------- create loops
  
  s1<-paste(paste(c(paste("for(",index, " in 1:", dim.vec,"){", sep="",collapse=""),"rep<-rep+1;setTxtProgressBar(pb, rep);"),collapse=""),assign.param.values, sep="",collapse="")
  in.export<-paste("'",c('func2',export.functions,param.names),"'", collapse=",", sep="")
  aux.s2<-paste("if(ncpus>1){sfExport(",in.export,")};",sep="")
  s2<-paste("suppressMessages(sfInit(parallel=if(ncpus>1){TRUE}else{FALSE}, cpus = ncpus, type= 'SOCK'));",if(length(packages)>0){paste("capture.output(suppressMessages(sfLibrary(",packages,")));", sep="", collapse="")}else{""},aux.s2,"erg<-sfApply(as.matrix(1:M,M,1),margin=1,fun=func2,",subm.param,");suppressMessages(sfStop());",sep="", collapse="")  
  s3<-paste(paste(paste("results$",ret.vals,"[",sep=""),paste(paste(index,",", collapse="", sep=""),"]<-",sep="",collapse=""),sep=""),paste("as.numeric(unlist(erg)[which(names(unlist(erg))==","'",ret.vals,"'",")]);",sep=""), collapse="")  
  s3<-substr(s3,1,(nchar(s3)-1))
  s4<-paste(rep("}",n.param),collapse="")
  all.loops<-paste(c(s1,s2,s3,s4), collapse="")
  rep<-0
  eval(parse(text=all.loops))
  cat("\n")
  cat(paste("\n","Results:","\n","\n"))
  if(raw==TRUE){return(results)}else{
    output<-list()
    for(i in 1:length(ret.vals)){output[[i]]<-apply(results[[i]],1:n.param,mean)}
    names(output)<-ret.vals
    return(output)
  }
}


#############################################################

#' @title Parallized Monte Carlo simulation.
#' @description \code{MonteCarlo} runs a Monte Carlo simulation study for a correctly specified function and the desired parameter grids. See details.
#' @details add details here.
#' @param func is the function to be evaluated. see details. must return a list
#' @param M specifies the number of monte carlo repetitions
#' @param param.list a list whose components are named after the parameters of func and each component is a vector containing the desired grid values for that parameter
#' @param ncpus number of cpus to be used. default is ncpus=1. For ncpus>1 the simulation is parallized automatically using ncpus cpu units.
#' @param raw specifies whether the output should be averaged over the M repetitions
#' @param max.grid specifies for which grid size to throw an error, if grid becomes to large.
#' @param timeNtest for large simulations or slow functions the required estimation time is estimated, see details. Default is timeNtest=FALSE.
#' @param save.res if timeNtest=TRUE and save.res=TRUE, the results of the auxillary simulation are saved to the current directory.
#' @import codetools
#' @examples
#' test.func<-function(n,loc,scale){
#'  sample<-rnorm(n, loc, scale)
#'  stat<-sqrt(n)*mean(sample)/sd(sample)
#'  decision<-abs(stat)>1.96
#'  return(list("decision"=decision))
#'}
#'
#'# Example without parallization
#'n.grid<-c(50,100,250,500)
#'loc.grid<-seq(0,1,0.1)
#'scale.grid<-1
#'
#'param.list=list("n"=n.grid, "loc"=loc.grid, "scale"=scale.grid)
#' MonteCarlo(func=test.func, M=1000, param.list=param.list, ncpus=1, timeNtest=TRUE, save.res=FALSE)
#' 
#'# Modify Test function to have slow example for parallized computation
#'
#'test.func2<-function(n,loc,scale){
#'  sample<-rnorm(n, loc, scale)
#'  stat<-sqrt(n)*mean(sample)/sd(sample)
#'  decision<-abs(stat)>1.96
#'  for(i in 1:(n)){for(j in 1:(n)){c(i,j)}}
#'  return(list("stat"=stat, "decision"=decision))
#'}
#'
#'n.grid<-200
#'loc.grid<-0#seq(0,1,0.1)
#'scale.grid<-seq(1,2,1)
#'param.list=list("n"=n.grid, "loc"=loc.grid, "scale"=scale.grid)
#'MonteCarlo(func=test.func2, M=500, param.list=param.list, ncpus=2, timeNtest=FALSE)
#'
#'@export

MonteCarlo<-function(func, M, param.list, ncpus=1, packages=NULL, raw=FALSE, max.grid=1000, timeNtest=FALSE, save.res=TRUE){

  # -------- check whether arguments supplied to function are admissable 
  
  # for raw==TRUE return array with results, otherwise return mean over repetitions
  if(is.function(func)==FALSE)stop("func must be a function")
  if(is.list(param.list)==FALSE)stop("param.list must be a list containing the names of the function arguments and the vectors of values that are supposed to be passed as arguments.")
  if((round(M)==M&&M>0)==FALSE)stop("M must be a positive integer.")
  
  # -------- Make test run so that error messages are thrown right away and extract names of list elements
  param.names<-names(param.list)
  test.run<-eval(parse(text=paste("func(",paste(param.names,"=param.list[[",1:length(param.names),"]][1]", sep="", collapse=","),")", collapse="", sep="")))
  ret.vals<-names(test.run)
  
  # --------- Check which functions that are called in func, match functions in the global environment and prepare to export those
  all<-ls(name=.GlobalEnv)
  all.funcs<-NULL
  for(i in 1:length(all)){if(is.function(eval(parse(text=all[i])))){all.funcs<-c(all.funcs,all[i])}}
  in.func.aux<-findGlobals(func, merge=FALSE)$functions # extract functions used in func
  in.func<-NULL # loop to sort out primitive functions
  for(i in 1:length(in.func.aux)){if(is.function(tryCatch(.Primitive(in.func.aux[i]), error=function(e)FALSE))==FALSE){in.func<-c(in.func,in.func.aux[i])}}
  
  new.funcs<-subset(in.func,in.func%in%all.funcs)
  export.functions<-new.funcs
    
  # -- loop through deeper functions to find functions called by functions in func and deeper nested functions
  all.funcs.found<-in.func # create array of names of all functions found so that the packages required can be found below
  while(length(new.funcs)>0){ # while loop runs as long as new functions are found in deeper layers 
    n.exp<-length(new.funcs)
    new.funcs2<-NULL
    for(i in 1:n.exp){# find functions used in every new function
      in.inner.aux<-findGlobals(eval(parse(text=new.funcs[i])))  
      in.inner<-NULL # loop to sort out primitive functions
      for(i in 1:length(in.inner.aux)){if(is.function(tryCatch(.Primitive(in.inner.aux[i]), error=function(e)FALSE))==FALSE){if(tryCatch(is.function(eval(parse(text=in.inner.aux[i]))), error=function(e)FALSE)){in.inner<-c(in.inner,in.inner.aux[i])}}}
      new.funcs2<-c(new.funcs2,subset(in.inner,in.inner%in%all.funcs)) # list those new functions found that are defined in global environment
      all.funcs.found<-unique(c(all.funcs.found,in.inner[apply(as.matrix(in.inner),1,exists)])) # determine which of the functions defined in inner functions do exist and append to list of all functions that may come from packages
    }
    new.funcs<-new.funcs2
    export.functions<-c(export.functions,new.funcs)
  }
  
  # -------- Find out which packages have to be loaded into cluster
  
  all.env<-search() #list all environments
  env.names<-unlist(strsplit(all.env[grep(":", all.env)], split=":")) # keep only those environments that refer to packages
  env.names<-env.names[-which(env.names=="package")]
  
  packages<-NULL #loop through non-primitive functions used in func and check from which package they are
  for(i in 1:length(all.funcs.found)){if(environmentName(environment(eval(parse(text=all.funcs.found[i]))))%in%env.names){packages<-c(packages,env.names[which(env.names==environmentName(environment(eval(parse(text=all.funcs.found[i])))))])}}
  packages<-unique(packages[packages!="base"])
  
  dependencies.list<-NULL # loop through packages found and collect their dependencies in character vector
  for(i in 1:length(packages)){dependencies.list<-c(dependencies.list,unlist(strsplit(as.character(packageDescription(packages[i])$Depends), split=",")))}
  dependencies.list<-unique(dependencies.list)
  dependencies.list<-gsub(" ","",dependencies.list)
  packages<-packages[-which(packages%in%dependencies.list)] # keep only those packages that are not automatically included because they are dependencies
  
  print(packages)
  
  # --------- sort param list according to order in arg.list
  arg.list<-as.list(args(func))
  arg.list<-arg.list[which(names(arg.list)!="")]
  input.param<-0
  for(i in 1:length(param.list)){input.param[i]<-(names(param.list))[i]}
  param.list<-param.list[order(match(input.param,names(arg.list)))] 
  
  # -------- throw error if function arguments with no default are not supplied
  
  for(i in 1:length(arg.list)){if(is.symbol(arg.list[[i]])){if(names(arg.list)[i]%in%input.param==FALSE)stop(paste("param.list does not contain parameter values for", names(arg.list)[i]))}}
  
  # ------- extract information from parameter list
  n.param<-length(param.list)                                                       # number of parameters
  dim.vec<-numeric(n.param) 
  for(i in 1:n.param){dim.vec[i]<-length(param.list[[i]])}                          # construct vector with grid dimensions
  param.names<-names(param.list)                                                    # names of parameters
  grid.size<-prod(dim.vec)
  
  if(grid.size>max.grid)stop("Grid size is very large. If you still want to run the simulation change max.grid.")
  
  if(timeNtest==TRUE){
    cat("Running test simulation to estimate simulation time required.","\n","\n")
    param.list2<-list()
    for(i in 1:length(param.list)){param.list2[[i]]<-if(length(param.list[[i]])>1){c(min(param.list[[i]]),max(param.list[[i]]))}else{param.list[[i]]}}
    names(param.list2)<-names(param.list)
    dim.vec2<-numeric(length(param.list2)) 
    for(i in 1:n.param){dim.vec2[i]<-length(param.list2[[i]])}                          # construct vector with grid dimensions
    grid.size2<-prod(dim.vec2)
    t1<-Sys.time()
    erg.pre<-MC.inner(func=func,M=M/10,param.list=param.list2, ret.vals=ret.vals, ncpus=ncpus, raw=raw, max.grid=max.grid, packages=packages, export.functions=export.functions)
    t2<-Sys.time()
    time<-(t2-t1)*grid.size/grid.size2*10    
    cat(paste("Estimated time required:",round(as.numeric(time)),attributes(time)$units,"\n","\n"))
    if(save.res==TRUE){
      time.done<-Sys.time()    
      filename<-paste("MonteCarloTest","_",as.character(as.Date(time.done)),"_",gsub(":","-",x=format(time.done,"%H:%M:%S")),".Rdata", sep="")
      save(erg.pre, file=filename)
      cat(paste("Output of testrun written to:",paste(getwd(),"/",filename,sep=""),"\n","\n"))
    }
  }
  erg<-MC.inner(func=func,M=M,param.list=param.list, ret.vals=ret.vals, ncpus=ncpus, raw=raw, max.grid=max.grid, packages=packages, export.functions=export.functions)
  return(erg)  
}







