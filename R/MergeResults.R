
#' @title Merge output arrays from distributed simulation.
#' @description \code{MergeResults} is a utility function that allows to bind several output arrays together,
#' @details Arrays have to be saved using \code{save}
#' @param identifier string that is common to the subset of files in the selected directory that are supposed to be merged.
#' @param path path to directory that contains the files to be merged
#' @param bind_along array dimension that stacks monte carlo repetitions. By default the last one.
#' @import abind
#' @examples #erg<-MergeResults(path="C:/Users/Monte Carlo Results/gg", identifier="gg", bind_along=2)
#'@export

MergeResults<-function(identifier,path,bind_along=NULL){

old_path<-getwd()
setwd(path)  
selector<-list.files()[grep(identifier, x=list.files())]
cat("selected files:","\n",paste(selector,"\n"))
a<-load(selector[1])
load(selector[1])
help<-get(a)
n_it<-length(help)
for(i in 1:n_it){assign(paste("coll.res",i, sep=""),NULL)}

for(i in 1:length(selector)){
  load(selector[i])
  if(is.null(bind_along)){for(j in 1:n_it){assign(paste("coll.res",j, sep=""),abind(get(paste("coll.res",j,sep="")),out[[j]]))}}else{for(j in 1:n_it){assign(paste("coll.res",j, sep=""),abind(get(paste("coll.res",j,sep="")),out[[j]], along=bind_along))}}  
}

eval(parse(text=paste("output<-list(",paste(paste("'",names(help),"'","=",paste("coll.res",1:n_it, sep="")), collapse=","),")")))
setwd(old_path)
return(output)
}

#erg<-MergeResults(path="C:\\Users\\Christian\\Google Drive\\Seasonal Long Memory\\SCLM Specification\\Monte Carlo Results\\gg", identifier="gg", bind_along=2)


#T.grid<-c(500)
#phi.grid<-seq(0,0.6,0.1)
#d.grid<-seq(0,0.4,0.1)
#cpar.grid<-c(0.2,0.25)

#param.list=list("T"=T.grid, "phi"=phi.grid, "d"=d.grid, "cpar"=cpar.grid)

#MakeTable(output, param.list=param.list,rows=c("phi","T"), cols=c("d","cpar"), digits=3)




