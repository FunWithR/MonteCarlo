
#' @title Merge lists returned from separate simulations with MonteCarlo().
#' @description \code{MergeResults} is a utility function that allows to merge the output from separate simulations
#' using the same function and parameter grid.
#' @details To merge two or more files with simulation results they have to be saved using \code{save}.
#' The identifier string has to be part of the name of all targeted files, but not part of the names of any 
#' other files in the directory.
#' @param identifier string that is common to the names of the files that are supposed to be merged.
#' @param path string specifying the path to directory that contains the files.
#' @import abind
#' @examples 
#' \donttest{out<-MergeResults(identifier="MonteCarloResults", path="C:/Users/")
#' summary(out)}
#' @export
MergeResults<-function(identifier,path){
  
  old_path<-getwd()
  setwd(path)  
  selector<-list.files()[grep(identifier, x=list.files())]
  cat("selected files:","\n",paste(selector,"\n"))
  
  name1<-load(selector[1])
  out1<-get(name1)
  if(class(out1)!="MonteCarlo")stop("Only objects of class MonteCarlo can be merged with each other.")
  if(out1$meta$raw==FALSE)stop("MonteCarlo objects can only be merged if raw==TRUE.")
  
  for(i in 2:length(selector)){
    name2<-load(selector[i])
    out2<-get(name2)
    if(class(out2)!="MonteCarlo")stop("Only objects of class MonteCarlo can be merged with each other.")
    if(out2$meta$raw==FALSE)stop("MonteCarlo objects can only be merged if raw==TRUE.")
    if(FALSE%in%(names(out1$param.list)==names(out2$param.list)))stop("MonteCarlo objects can only be merged if param.list is identical.")
    for(j in 1:length(out1$param.list)){
      if(FALSE%in%(out1$param.list[[j]]==out2$param.list[[j]]))stop("MonteCarlo objects can only be merged if param.list is identical.")
    }
    if(FALSE%in%(capture.output(out1$meta$func)==capture.output(out2$meta$func)))stop("MonteCarlo objects can be only merged if func is the same for all of them.")
    for(k in 1:length(out1$results)){
      out1$results[[k]]<-abind(out1$results[[k]],out2$results[[k]])
    }
    out1$meta$M<-out1$meta$M+out2$meta$M
    out1$meta$ncpus<-c(out1$meta$ncpus,out2$meta$ncpus)
    out1$meta$time<-out1$meta$time+out2$meta$time
  }
  setwd(old_path)
  
  return(out1)
}


#erg<-MergeResults(path="C:\\Users\\Christian\\Google Drive\\Seasonal Long Memory\\SCLM Specification\\Monte Carlo Results\\gg", identifier="gg", bind_along=2)


#T.grid<-c(500)
#phi.grid<-seq(0,0.6,0.1)
#d.grid<-seq(0,0.4,0.1)
#cpar.grid<-c(0.2,0.25)

#param.list=list("T"=T.grid, "phi"=phi.grid, "d"=d.grid, "cpar"=cpar.grid)

#MakeTable(output, param.list=param.list,rows=c("phi","T"), cols=c("d","cpar"), digits=3)




