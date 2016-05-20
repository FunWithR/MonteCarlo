
#library(mvtnorm)

#reg.func<-function(T,mux,sdx,sig.noise,sig.eps,beta0,beta1,rho){
#cov_noise_e<-sig.noise*sig.eps*rho
#noise_e<-rmvnorm(T, mean=c(0,0), sigma=matrix(c(sig.noise^2,cov_noise_e,cov_noise_e,sig.eps^2),2,2))
#x<-rnorm(T,mux,sdx)
#noise<-noise_e[,1]
#eps<-noise_e[,2]
#X<-cbind(1,x+noise)
#beta<-c(beta0,beta1)
#y<-X%*%beta+eps
#beta.hat<-solve(t(X)%*%X)%*%t(X)%*%y
#sig.eps2.hat<-mean((y-X%*%beta.hat)^2)
#sd.beta<-sqrt(diag(sig.eps2.hat*solve(t(X)%*%X)))
#bias<-beta-beta.hat
#test<-abs((beta.hat[2]-beta1)/sd.beta)>1.96
#return(list("bias"=bias[2],"test"=test[2]))
#}

#reg.func(1000,0,1,1,1,0,1,0)

#T.grid<-c(50,100,500)
#mux.grid<-c(0)
#sdx.grid<-c(1,2)
#sig.noise.grid<-c(0.1,1)
#sig.eps.grid<-c(0.5,1,2)
#beta0.grid<-c(0)
#beta1.grid<-c(1)
#rho.grid<-c(-0.5,0,0.5)

#param.list=list("T"=T.grid, "mux"=mux.grid, "sdx"=sdx.grid,
#                "sig.noise"=sig.noise.grid, "sig.eps"=sig.eps.grid,
#                "beta0"=beta0.grid, "beta1"=beta1.grid, "rho"=rho.grid)


#output<-MonteCarlo(func=reg.func, M=250, param.list=param.list, ncpus=1, timeNtest=FALSE, save.res=FALSE, raw=TRUE, max.grid<-5000)

#rows<-c("T", "sig.noise", "beta0","sdx")
#cols<-c("mux","beta1","sig.eps","rho")
#MakeTable(output, rows, cols, func=reg.func, digits=2)

#rows<-c("beta0","sdx","T", "sig.noise","rho")
#cols<-c("mux","beta1","sig.eps")
#MakeTable(output, rows, cols, func=reg.func, digits=2)

#rows<-c("T", "sig.noise", "beta0","sdx")
#cols<-c("mux","beta1","sig.eps","rho")
#MakeTable(output, rows, cols, func=reg.func, digits=2)

#rows<-c("T", "sig.noise", "beta0","sdx")
#cols<-c("mux","beta1","sig.eps","rho")
#MakeTable(output, rows, cols, func=reg.func, digits=2)





