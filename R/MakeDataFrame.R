#######################################################################################################################################
#
#   MakeFrame generates data.frames that contain the simulation results and the
#   respective parameter constellations. These can be used for visualization and
#   analasis of simulation results.
#
#######################################################################################################################################




#' @title Conversion of MonteCarlo outputs to data.frame.
#' @description \code{MakeFrame} takes the output of \code{MonteCarlo} as its argument and returns a 
#' data.frame that contains the simulation results.
#' @details 
#' Each row of the data.frame contains the values returned by \code{func} for one repetition of the simulation 
#' and the respective values of the parameters.
#' @param output A \code{MonteCarlo} object returned by the \code{MonteCarlo()} function.
#' @return A data.frame that contains the simulation results.
#' @importFrom reshape melt.array
#' @examples
#' test_func<-function(n,loc,scale){
#'  sample<-rnorm(n, loc, scale)
#'  stat<-sqrt(n)*mean(sample)/sd(sample)
#'  decision<-abs(stat)>1.96
#'  return(list("decision"=decision, "stat"=stat))
#'}
#'
#'n_grid<-c(50,100,250,500)
#'loc_grid<-c(0,1)
#'scale_grid<-c(1,2)
#'
#'param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
#'erg<-MonteCarlo(func=test_func, nrep=250, param_list=param_list, ncpus=1)
#'df<-MakeFrame(erg)
#'head(df)
#'
#'library(dplyr)
#'library(ggplot2)
#'tbl <- tbl_df(df)
#'ggplot(filter(tbl, loc==0)) + geom_density(aes(x=stat, col=factor(n)))
#'
#'@export
MakeFrame<-function(output){
  
  if(class(output) != "MonteCarlo")stop("output has to be an object of class MonteCarlo.")
  param_list <- output$param_list
  output_df <- NULL
  for (j in 1:length(output$results)) {
    melted <- melt.array(output$results[[j]], varnames = c(names(param_list),"rep"))
    melted <- melted[, -(length(param_list) + 1)]
    if (j == 1) {
      output_df <- melted
      names(output_df)[ncol(output_df)] <- names(output$results)[j]
    }
    else {output_df[, names(output$results)[j]] <- melted[,ncol(melted)]
    }
  }
  for (i in 1:(ncol(output_df) - length(output$results))){
    if (is.numeric(param_list[[i]])) {
      aux <- unlist(strsplit(as.character(output_df[, i]), split = "="))
      output_df[,i]<-as.numeric(aux[seq(2,length(aux),2)])
    }
  }
  output_df
}

